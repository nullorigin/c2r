/*!
 * Custom Threading System - Built on the Lock Foundation
 *
 * A high-performance, in-house threading system that leverages the custom Lock
 * implementation for thread-safe operations without external dependencies.
 *
 * Features:
 * - ThreadPool with configurable worker count (defaults to CPU cores)
 * - Work-stealing task queue system
 * - Parallel iterator-like interface
 * - Built on the robust Lock<T> system for thread safety
 * - Zero external dependencies beyond std
 */

use crate::lock::Lock;
use std::collections::{HashMap, VecDeque};
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::mpsc::{self, Receiver, Sender};
use std::sync::{Arc, Mutex};
use std::thread::{self, JoinHandle};
use std::time::{Duration, Instant};

/// A task that can be executed by the thread pool
pub type Task = Box<dyn FnOnce() + Send + 'static>;

/// Thread pool configuration
#[derive(Debug, Clone)]
pub struct ThreadPoolConfig {
    pub worker_count: usize,
    pub queue_capacity: Option<usize>,
    pub name_prefix: String,
}

impl Default for ThreadPoolConfig {
    fn default() -> Self {
        ThreadPoolConfig {
            worker_count: thread::available_parallelism().map_or(1, |p| p.get()),
            queue_capacity: Some(1000),
            name_prefix: "c2r-worker".to_string(),
        }
    }
}

/// High-performance thread pool using custom Lock system
pub struct ThreadPool {
    workers: Vec<Worker>,
    sender: Sender<Task>,
    shutdown: Arc<AtomicBool>,
    active_tasks: Arc<AtomicUsize>,
    config: ThreadPoolConfig,
}

/// Individual worker thread
struct Worker {
    handle: Option<JoinHandle<()>>,
}

impl ThreadPool {
    /// Create a new thread pool with default configuration
    pub fn new() -> Self {
        Self::with_config(ThreadPoolConfig::default())
    }

    /// Create a thread pool with custom configuration
    pub fn with_config(config: ThreadPoolConfig) -> Self {
        let (sender, receiver) = mpsc::channel();
        let receiver = Arc::new(Mutex::new(receiver));
        let shutdown = Arc::new(AtomicBool::new(false));
        let active_tasks = Arc::new(AtomicUsize::new(0));

        let mut workers = Vec::with_capacity(config.worker_count);

        for id in 0..config.worker_count {
            workers.push(Worker::new(
                Arc::clone(&receiver),
                Arc::clone(&shutdown),
                Arc::clone(&active_tasks),
                format!("{}-{}", config.name_prefix, id),
            ));
        }

        ThreadPool {
            workers,
            sender,
            shutdown,
            active_tasks,
            config,
        }
    }

    /// Execute a task on the thread pool
    pub fn execute<F>(&self, f: F)
    where
        F: FnOnce() + Send + 'static,
    {
        let task = Box::new(f);
        self.sender.send(task).expect("Thread pool has shut down");
    }

    /// Get the number of worker threads
    pub fn worker_count(&self) -> usize {
        self.config.worker_count
    }

    /// Get the number of currently active tasks
    pub fn active_tasks(&self) -> usize {
        self.active_tasks.load(Ordering::Relaxed)
    }

    /// Wait for all active tasks to complete
    pub fn wait_for_completion(&self) {
        while self.active_tasks() > 0 {
            thread::yield_now();
        }
    }

    /// Shutdown the thread pool gracefully
    pub fn shutdown(mut self) {
        self.shutdown.store(true, Ordering::SeqCst);

        // Drop sender to close the channel
        drop(self.sender);

        // Wait for all workers to finish
        for worker in &mut self.workers {
            if let Some(handle) = worker.handle.take() {
                handle.join().unwrap();
            }
        }
    }
}

impl Worker {
    fn new(
        receiver: Arc<Mutex<Receiver<Task>>>,
        shutdown: Arc<AtomicBool>,
        active_tasks: Arc<AtomicUsize>,
        name: String,
    ) -> Worker {
        let handle = thread::Builder::new()
            .name(name)
            .spawn(move || {
                loop {
                    // Check if we should shutdown
                    if shutdown.load(Ordering::Relaxed) {
                        break;
                    }

                    // Try to get a task
                    let task = {
                        let receiver = receiver.lock().unwrap();
                        receiver.recv()
                    };

                    match task {
                        Ok(task) => {
                            // Increment active task counter
                            active_tasks.fetch_add(1, Ordering::Relaxed);

                            // Execute the task
                            task();

                            // Decrement active task counter
                            active_tasks.fetch_sub(1, Ordering::Relaxed);
                        }
                        Err(_) => {
                            // Channel closed, time to shut down
                            break;
                        }
                    }
                }
            })
            .expect("Failed to spawn worker thread");

        Worker {
            handle: Some(handle),
        }
    }
}

/// Parallel iterator interface using our custom threading system
pub struct Iterizer<T> {
    items: Vec<T>,
    pool: Arc<ThreadPool>,
}

impl<T: Send + Sync + 'static> Iterizer<T> {
    /// Create a new parallel iterator
    pub fn from_vec(items: Vec<T>) -> Self {
        Iterizer {
            items,
            pool: Arc::new(ThreadPool::new()),
        }
    }

    /// Create a parallel iterator with custom thread pool
    pub fn from_vec_with_pool(items: Vec<T>, pool: Arc<ThreadPool>) -> Self {
        Iterizer { items, pool }
    }

    /// Map operation across all items in parallel
    pub fn map<U, F>(self, f: F) -> Vec<U>
    where
        F: Fn(T) -> U + Send + Sync + Clone + 'static,
        U: Send + Sync + 'static,
    {
        let f = Arc::new(f);
        let results: Arc<Lock<Vec<U>>> = Arc::new(Lock::new());
        results.force(Vec::with_capacity(self.items.len()));

        // Initialize results vector with placeholders
        {
            let mut results_guard = results.write();
            results_guard.resize_with(self.items.len(), || unsafe { std::mem::zeroed() });
        }

        // Process items in parallel
        for (index, item) in self.items.into_iter().enumerate() {
            let f_clone = Arc::clone(&f);
            let results_clone: Arc<Lock<Vec<U>>> = Arc::clone(&results);

            self.pool.execute(move || {
                let result = f_clone(item);
                let mut results_guard = results_clone.write();
                unsafe {
                    std::ptr::write(results_guard.as_mut_ptr().add(index), result);
                }
            });
        }

        // Wait for all tasks to complete
        self.pool.wait_for_completion();

        // Extract results
        let results_guard = results.read();
        unsafe {
            let mut final_results = Vec::with_capacity(results_guard.len());
            for i in 0..results_guard.len() {
                final_results.push(std::ptr::read(results_guard.as_ptr().add(i)));
            }
            final_results
        }
    }

    /// Filter items in parallel and collect results
    pub fn filter<F>(self, predicate: F) -> Vec<T>
    where
        F: Fn(&T) -> bool + Send + Sync + Clone + 'static,
        T: Clone + Send + Sync,
    {
        let predicate = Arc::new(predicate);
        let results: Arc<Lock<Vec<T>>> = Arc::new(Lock::new());
        results.force(Vec::new());

        for item in self.items {
            let predicate_clone = Arc::clone(&predicate);
            let results_clone: Arc<Lock<Vec<T>>> = Arc::clone(&results);
            let item_clone = item.clone();

            self.pool.execute(move || {
                if predicate_clone(&item_clone) {
                    let mut results_guard = results_clone.write();
                    results_guard.push(item);
                }
            });
        }

        self.pool.wait_for_completion();

        let results_guard = results.read();
        results_guard.iter().cloned().collect()
    }

    /// Filter and map in one parallel operation
    pub fn filter_map<U, F>(self, f: F) -> Vec<U>
    where
        F: Fn(T) -> Option<U> + Send + Sync + Clone + 'static,
        U: Send + Sync + Clone + 'static,
    {
        let f = Arc::new(f);
        let results: Arc<Lock<Vec<U>>> = Arc::new(Lock::new());
        results.force(Vec::new());

        for item in self.items {
            let f_clone = Arc::clone(&f);
            let results_clone: Arc<Lock<Vec<U>>> = Arc::clone(&results);

            self.pool.execute(move || {
                if let Some(result) = f_clone(item) {
                    let mut results_guard = results_clone.write();
                    results_guard.push(result);
                }
            });
        }

        self.pool.wait_for_completion();

        let results_guard = results.read();
        results_guard.iter().cloned().collect()
    }

    /// Collect results into a vector
    pub fn collect(self) -> Vec<T> {
        self.items
    }

    /// Get the number of items
    pub fn len(&self) -> usize {
        self.items.len()
    }

    /// Check if empty
    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
    }
}

/// Extension trait to add parallel capabilities to iterators
pub trait Parallizer<T: Send + Sync + 'static>: Iterator<Item = T> + Sized {
    fn parallel(self) -> Iterizer<T> {
        Iterizer::from_vec(self.collect())
    }

    fn parallel_with_pool(self, pool: Arc<ThreadPool>) -> Iterizer<T> {
        Iterizer::from_vec_with_pool(self.collect(), pool)
    }
}

impl<I, T> Parallizer<T> for I
where
    I: Iterator<Item = T>,
    T: Send + Sync + 'static,
{
}

/// Work-stealing queue for advanced task distribution
pub struct WorkStealer<T> {
    queues: Vec<Arc<Lock<VecDeque<T>>>>,
    current_queue: AtomicUsize,
}

impl<T: Send + 'static> WorkStealer<T> {
    pub fn new(num_queues: usize) -> Self {
        let mut queues = Vec::with_capacity(num_queues);
        for _ in 0..num_queues {
            queues.push(Arc::new(Lock::new_with_value(VecDeque::new())));
        }

        WorkStealer {
            queues,
            current_queue: AtomicUsize::new(0),
        }
    }

    pub fn push(&self, item: T) {
        let queue_idx = self.current_queue.fetch_add(1, Ordering::Relaxed) % self.queues.len();
        let mut queue = self.queues[queue_idx].write();
        queue.push_back(item);
    }

    pub fn steal(&self, preferred_queue: usize) -> Option<T> {
        // Try preferred queue first
        if preferred_queue < self.queues.len() {
            if let Some(mut queue) = self.queues[preferred_queue].try_write() {
                if let Some(item) = queue.pop_front() {
                    return Some(item);
                }
            }
        }

        // Try stealing from other queues
        for i in 0..self.queues.len() {
            if i != preferred_queue {
                if let Some(mut queue) = self.queues[i].try_write() {
                    if let Some(item) = queue.pop_back() {
                        return Some(item);
                    }
                }
            }
        }

        None
    }
}

/// Global thread pool instance using Lock for safe initialization
static GLOBAL_POOL: Lock<ThreadPool> = Lock::new();

/// Get or initialize the global thread pool
pub fn global_pool() -> &'static ThreadPool {
    GLOBAL_POOL.get_or_init(|| ThreadPool::new())
}

/// Execute a closure in parallel using the global thread pool
pub fn spawn<F>(f: F)
where
    F: FnOnce() + Send + 'static,
{
    global_pool().execute(f);
}

/// Join multiple tasks and wait for completion
pub fn join_all<F, T>(tasks: Vec<F>) -> Vec<T>
where
    F: FnOnce() -> T + Send + 'static,
    T: Send + Sync + 'static,
{
    let pool = global_pool();
    let results: Arc<Lock<Vec<T>>> = Arc::new(Lock::new());
    results.force(Vec::with_capacity(tasks.len()));

    // Initialize results vector
    {
        let mut results_guard = results.write();
        results_guard.resize_with(tasks.len(), || unsafe { std::mem::zeroed() });
    }

    for (index, task) in tasks.into_iter().enumerate() {
        let results_clone: Arc<Lock<Vec<T>>> = Arc::clone(&results);
        pool.execute(move || {
            let result = task();
            let mut results_guard = results_clone.write();
            unsafe {
                std::ptr::write(results_guard.as_mut_ptr().add(index), result);
            }
        });
    }

    pool.wait_for_completion();

    // Extract results
    let results_guard = results.read();
    unsafe {
        let mut final_results = Vec::with_capacity(results_guard.len());
        for i in 0..results_guard.len() {
            final_results.push(std::ptr::read(results_guard.as_ptr().add(i)));
        }
        final_results
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_thread_pool_basic() {
        let pool = ThreadPool::new();
        let counter = Arc::new(AtomicUsize::new(0));

        for _ in 0..10 {
            let counter_clone = Arc::clone(&counter);
            pool.execute(move || {
                counter_clone.fetch_add(1, Ordering::Relaxed);
            });
        }

        pool.wait_for_completion();
        assert_eq!(counter.load(Ordering::Relaxed), 10);
    }

    #[test]
    fn test_parallel_map() {
        let data: Vec<i32> = (0..1000).collect();
        let results = data
            .into_iter()
            .parallel()
            .map(|x| x * 2)
            .into_iter()
            .collect::<Vec<i32>>();

        for (i, &result) in results.iter().enumerate() {
            assert_eq!(result, i as i32 * 2);
        }
    }

    #[test]
    fn test_parallel_filter() {
        let data: Vec<i32> = (0..100).collect();
        let results = data
            .into_iter()
            .parallel()
            .filter(|&x| x % 2 == 0)
            .into_iter()
            .collect::<Vec<i32>>();

        assert_eq!(results.len(), 50);
        for &result in &results {
            assert_eq!(result % 2, 0);
        }
    }
}

/// Thread health status for monitoring
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ThreadHealth {
    Healthy,
    Unresponsive,
    Failed,
    Terminated,
}

/// Thread recovery statistics
#[derive(Debug, Clone)]
pub struct RecoveryStats {
    pub restarts: usize,
    pub failures: usize,
    pub last_failure: Option<Instant>,
    pub total_tasks_completed: usize,
}

impl Default for RecoveryStats {
    fn default() -> Self {
        RecoveryStats {
            restarts: 0,
            failures: 0,
            last_failure: None,
            total_tasks_completed: 0,
        }
    }
}

/// Watchdog system for monitoring and recovering threads
pub struct Watcher {
    thread_health: Arc<Lock<HashMap<usize, ThreadHealth>>>,
    thread_heartbeats: Arc<Lock<HashMap<usize, Instant>>>,
    recovery_stats: Arc<Lock<HashMap<usize, RecoveryStats>>>,
    watchdog_handle: Option<JoinHandle<()>>,
    shutdown: Arc<AtomicBool>,
    check_interval: Duration,
    timeout_threshold: Duration,
}

impl Watcher {
    /// Create a new watchdog with default settings
    pub fn new() -> Self {
        Self::with_intervals(Duration::from_secs(1), Duration::from_secs(5))
    }

    /// Create a watchdog with custom intervals
    pub fn with_intervals(check_interval: Duration, timeout_threshold: Duration) -> Self {
        Watcher {
            thread_health: Arc::new(Lock::new()),
            thread_heartbeats: Arc::new(Lock::new()),
            recovery_stats: Arc::new(Lock::new()),
            watchdog_handle: None,
            shutdown: Arc::new(AtomicBool::new(false)),
            check_interval,
            timeout_threshold,
        }
    }

    /// Start the watchdog monitoring system
    pub fn start(&mut self, thread_count: usize) {
        // Initialize health tracking for all threads
        self.thread_health.force(HashMap::new());
        self.thread_heartbeats.force(HashMap::new());
        self.recovery_stats.force(HashMap::new());

        {
            let mut health_guard = self.thread_health.write();
            let mut heartbeat_guard = self.thread_heartbeats.write();
            let mut stats_guard = self.recovery_stats.write();

            let now = Instant::now();
            for i in 0..thread_count {
                health_guard.insert(i, ThreadHealth::Healthy);
                heartbeat_guard.insert(i, now);
                stats_guard.insert(i, RecoveryStats::default());
            }
        }

        let thread_health = Arc::clone(&self.thread_health);
        let thread_heartbeats = Arc::clone(&self.thread_heartbeats);
        let recovery_stats = Arc::clone(&self.recovery_stats);
        let shutdown = Arc::clone(&self.shutdown);
        let check_interval = self.check_interval;
        let timeout_threshold = self.timeout_threshold;

        self.watchdog_handle = Some(thread::spawn(move || {
            while !shutdown.load(Ordering::Relaxed) {
                thread::sleep(check_interval);

                let now = Instant::now();
                let mut health_guard = thread_health.write();
                let heartbeat_guard = thread_heartbeats.read();
                let mut stats_guard = recovery_stats.write();

                // Check each thread's health
                for (&thread_id, &last_heartbeat) in heartbeat_guard.iter() {
                    let elapsed = now.duration_since(last_heartbeat);

                    if elapsed > timeout_threshold {
                        if let Some(health) = health_guard.get_mut(&thread_id) {
                            match *health {
                                ThreadHealth::Healthy => {
                                    *health = ThreadHealth::Unresponsive;
                                    println!("Warning: Thread {} is unresponsive", thread_id);
                                }
                                ThreadHealth::Unresponsive => {
                                    *health = ThreadHealth::Failed;
                                    if let Some(stats) = stats_guard.get_mut(&thread_id) {
                                        stats.failures += 1;
                                        stats.last_failure = Some(now);
                                    }
                                    println!("Error: Thread {} has failed", thread_id);
                                }
                                _ => {} // Already marked as failed or terminated
                            }
                        }
                    }
                }
            }
        }));
    }

    /// Signal that a thread is alive and working
    pub fn heartbeat(&self, thread_id: usize) {
        let mut heartbeat_guard = self.thread_heartbeats.write();
        heartbeat_guard.insert(thread_id, Instant::now());

        let mut health_guard = self.thread_health.write();
        if let Some(health) = health_guard.get_mut(&thread_id) {
            if *health == ThreadHealth::Unresponsive {
                *health = ThreadHealth::Healthy;
                println!("Info: Thread {} recovered", thread_id);
            }
        }
    }

    /// Mark a thread as completed a task successfully
    pub fn task_completed(&self, thread_id: usize) {
        self.heartbeat(thread_id);
        let mut stats_guard = self.recovery_stats.write();
        if let Some(stats) = stats_guard.get_mut(&thread_id) {
            stats.total_tasks_completed += 1;
        }
    }

    /// Get the health status of a specific thread
    pub fn get_thread_health(&self, thread_id: usize) -> Option<ThreadHealth> {
        let health_guard = self.thread_health.read();
        health_guard.get(&thread_id).copied()
    }

    /// Get recovery statistics for a thread
    pub fn get_recovery_stats(&self, thread_id: usize) -> Option<RecoveryStats> {
        let stats_guard = self.recovery_stats.read();
        stats_guard.get(&thread_id).cloned()
    }

    /// Get overall system health report
    pub fn health_report(&self) -> HashMap<usize, (ThreadHealth, RecoveryStats)> {
        let health_guard = self.thread_health.read();
        let stats_guard = self.recovery_stats.read();

        let mut report = HashMap::new();
        for (&thread_id, &health) in health_guard.iter() {
            if let Some(stats) = stats_guard.get(&thread_id) {
                report.insert(thread_id, (health, stats.clone()));
            }
        }
        report
    }

    /// Attempt to recover a failed thread
    pub fn recover_thread(&self, thread_id: usize) -> bool {
        let mut health_guard = self.thread_health.write();
        let mut stats_guard = self.recovery_stats.write();

        if let Some(health) = health_guard.get_mut(&thread_id) {
            if *health == ThreadHealth::Failed {
                *health = ThreadHealth::Healthy;

                if let Some(stats) = stats_guard.get_mut(&thread_id) {
                    stats.restarts += 1;
                }

                self.heartbeat(thread_id);
                println!("Info: Thread {} recovery attempted", thread_id);
                return true;
            }
        }
        false
    }

    /// Shutdown the watchdog system
    pub fn shutdown(&mut self) {
        self.shutdown.store(true, Ordering::Relaxed);

        if let Some(handle) = self.watchdog_handle.take() {
            let _ = handle.join();
        }
    }
}

impl Drop for Watcher {
    fn drop(&mut self) {
        self.shutdown();
    }
}

/// Resilient thread pool with watchdog monitoring and recovery
pub struct ResilientThreadPool {
    pool: ThreadPool,
    watchdog: Watcher,
    incremental_results: Arc<Lock<Vec<Vec<u8>>>>, // For incremental processing
    sync_barrier: Arc<AtomicUsize>,
}

impl ResilientThreadPool {
    /// Create a new resilient thread pool
    pub fn new() -> Self {
        Self::with_config(ThreadPoolConfig::default())
    }

    /// Create with custom configuration
    pub fn with_config(config: ThreadPoolConfig) -> Self {
        let pool = ThreadPool::with_config(config.clone());
        let mut watchdog = Watcher::new();
        watchdog.start(config.worker_count);

        ResilientThreadPool {
            pool,
            watchdog,
            incremental_results: Arc::new(Lock::new()),
            sync_barrier: Arc::new(AtomicUsize::new(0)),
        }
    }

    /// Execute a task with monitoring and recovery
    pub fn execute_monitored<F>(&self, thread_id: usize, task: F)
    where
        F: FnOnce() + Send + 'static,
    {
        // Clone watchdog components for thread safety
        let thread_health = Arc::clone(&self.watchdog.thread_health);
        let thread_heartbeats = Arc::clone(&self.watchdog.thread_heartbeats);
        let recovery_stats = Arc::clone(&self.watchdog.recovery_stats);

        self.pool.execute(move || {
            // Heartbeat before starting
            {
                let mut heartbeat_guard = thread_heartbeats.write();
                heartbeat_guard.insert(thread_id, Instant::now());
            }

            // Execute the task with panic recovery
            let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                task();
            }));

            match result {
                Ok(()) => {
                    // Mark task completed successfully
                    let mut heartbeat_guard = thread_heartbeats.write();
                    heartbeat_guard.insert(thread_id, Instant::now());

                    let mut stats_guard = recovery_stats.write();
                    if let Some(stats) = stats_guard.get_mut(&thread_id) {
                        stats.total_tasks_completed += 1;
                    }
                }
                Err(_) => {
                    println!("Error: Thread {} panicked during task execution", thread_id);
                    // Mark thread as failed
                    let mut health_guard = thread_health.write();
                    if let Some(health) = health_guard.get_mut(&thread_id) {
                        *health = ThreadHealth::Failed;
                    }
                }
            }
        });
    }

    /// Process tasks incrementally with isolation
    pub fn process_incremental<T, F, R>(&self, items: Vec<T>, processor: F) -> Vec<R>
    where
        T: Send + 'static,
        F: Fn(T) -> R + Send + Sync + 'static,
        R: Send + 'static,
    {
        use std::sync::mpsc;

        let (sender, receiver) = mpsc::channel();
        let processor = Arc::new(processor);
        let item_count = items.len();

        // Submit all tasks to the thread pool
        for item in items {
            let processor_clone = Arc::clone(&processor);
            let sender_clone = sender.clone();

            self.pool.execute(Box::new(move || {
                let result = processor_clone(item);
                let _ = sender_clone.send(result); // Ignore send errors
            }));
        }

        // Drop the original sender so receiver knows when all tasks are done
        drop(sender);

        // Collect results as they come in
        let mut results = Vec::new();
        for _ in 0..item_count {
            if let Ok(result) = receiver.recv() {
                results.push(result);
            }
        }

        results
    }

    /// Wait for all tasks to complete
    pub fn wait_for_completion(&self) {
        self.pool.wait_for_completion();
    }
}

impl Drop for ResilientThreadPool {
    fn drop(&mut self) {
        // Signal shutdown and let the thread pool clean up naturally
        self.pool.shutdown.store(true, Ordering::SeqCst);
        // Watchdog will be dropped automatically and shutdown
    }
}
