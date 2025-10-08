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

use crate::{Maybe, MaybeLock, OptionLock};
use core::ops::{Fn, FnOnce};
use core::option::Option::Some;
use core::time;
use std::collections::{HashMap, VecDeque};
use std::ops::Deref;
use std::ptr::drop_in_place;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::mpsc::{self, Receiver, Sender};
use std::sync::{Arc, Mutex};
use std::thread::{self, JoinHandle};
use std::time::{Duration, Instant};

/// A task that can be executed by the thread pool
pub type Task = Box<dyn FnOnce() + Send + Sync + 'static>;

/// Thread pool configuration
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ThreadPoolConfig {
    pub worker_count: usize,
    pub queue_capacity: Option<usize>,
    pub name_prefix: &'static str,
}
impl ThreadPoolConfig {
    pub const fn new() -> Self {
        ThreadPoolConfig {
            worker_count: 1,
            queue_capacity: Some(1000),
            name_prefix: "c2r-worker",
        }
    }
}
impl Default for ThreadPoolConfig {
    fn default() -> Self {
        ThreadPoolConfig {
            worker_count: thread::available_parallelism().map_or(1, |p| p.get()),
            queue_capacity: Some(1000),
            name_prefix: "c2r-worker",
        }
    }
}

/// High-performance thread pool using custom Lock system
#[derive(Debug, PartialEq)]
pub struct ThreadPool {
    workers: Vec<Worker>,
    tasks: ThreadTasks,
    config: ThreadPoolConfig,
}
impl Default for ThreadPool {
    fn default() -> Self {
        Self::with_config(ThreadPoolConfig::new())
    }
}
impl Drop for ThreadPool {
    fn drop(&mut self) {
        // Signal shutdown to all worker threads
        self.tasks.shutdown.store(true, Ordering::SeqCst);
        
        // Gracefully join all worker threads, preventing resource leaks
        for worker in &mut self.workers {
            if let Some(handle) = worker.handle.take() {
                // Ignore join errors - thread might have already panicked
                let _ = handle.join();
            }
        }
    }
}
impl std::hash::Hash for ThreadPool {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.workers.hash(state);
        self.tasks.hash(state);
        self.config.hash(state);
    }
}
/// Individual worker thread
#[derive(Debug, Default)]
pub struct Worker {
    handle: Option<JoinHandle<()>>,
}
impl Clone for Worker {
    fn clone(&self) -> Self {
        Worker {
            handle: None,
        }
    }
}
impl PartialEq for Worker {
    fn eq(&self, other: &Self) -> bool {
        match (&self.handle, &other.handle) {
            (None, None) => true,
            (Some(h1), Some(h2)) => h1.is_finished() == h2.is_finished(),
            _ => false,
        }
    }
}
impl std::hash::Hash for Worker {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.handle.is_some().hash(state);
        if let Some(ref handle) = self.handle {
            handle.is_finished().hash(state);
        }
    }
}
#[derive(Debug)]
pub struct ThreadTasks {
    pub sender: Sender<Task>,
    pub receiver: Arc<Mutex<Receiver<Task>>>,
    pub active_tasks: Arc<AtomicUsize>,
    pub shutdown: Arc<AtomicBool>,
}
impl Clone for ThreadTasks {
    fn clone(&self) -> Self {
        ThreadTasks {
            sender: self.sender.clone(),
            receiver: self.receiver.clone(),
            active_tasks: self.active_tasks.clone(),
            shutdown: self.shutdown.clone(),
        }
    }
}

impl PartialEq for ThreadTasks {
    fn eq(&self, other: &Self) -> bool {
        self.receiver.is_poisoned() == other.receiver.is_poisoned()
            && self.active_tasks.load(Ordering::Relaxed)
                == other.active_tasks.load(Ordering::Relaxed)
            && self.shutdown.load(Ordering::Relaxed) == other.shutdown.load(Ordering::Relaxed)
    }
}

impl std::hash::Hash for ThreadTasks {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.receiver.is_poisoned().hash(state);
        self.active_tasks.load(Ordering::Relaxed).hash(state);
        self.shutdown.load(Ordering::Relaxed).hash(state);
    }
}

impl ThreadTasks {
    pub fn new() -> Self {
        let (sender, receiver) = mpsc::channel();
        ThreadTasks {
            sender,
            receiver: Arc::new(Mutex::new(receiver)),
            active_tasks: Arc::new(AtomicUsize::new(0)),
            shutdown: Arc::new(AtomicBool::new(false)),
        }
    }
}

impl ThreadPool {
    /// Create a new thread pool with default configuration
    pub fn new() -> Self {
        Self::with_config(ThreadPoolConfig::default())
    }

    pub fn new_with_config(config: ThreadPoolConfig) -> Self {
        Self::with_config(config)
    }

    pub fn with_config(config: ThreadPoolConfig) -> Self {
        let mut workers = Vec::with_capacity(config.worker_count);
        let tasks = ThreadTasks::new();

        for id in 0..config.worker_count {
            workers.push(Worker::new(
                tasks.receiver.clone(),
                tasks.shutdown.clone(),
                tasks.active_tasks.clone(),
                format!("{}-{}", config.name_prefix, id),
            ));
        }

        ThreadPool {
            workers,
            tasks,
            config,
        }
    }

    /// Execute a task on the thread pool
    pub fn execute<F>(&self, f: F)
    where
        F: FnOnce() + Send + Sync + 'static,
    {
        let task = Box::new(f);
        let _ = self.tasks.sender.send(task);
    }

    /// Get the number of worker threads
    pub fn worker_count(&self) -> usize {
        self.config.worker_count
    }

    /// Get the number of currently active tasks
    pub fn active_tasks(&self) -> usize {
        self.tasks.active_tasks.load(Ordering::Relaxed)
    }

    /// Wait for all active tasks to complete
    pub fn wait_for_completion(&self) {
        while self.active_tasks() > 0 {
            thread::yield_now();
        }
    }

    /// Shutdown the thread pool gracefully
    pub fn shutdown(self) {
        self.tasks.shutdown.store(true, Ordering::SeqCst);

        // Wait for all workers to finish
        for mut worker in self.workers.clone() {
            if let Some(handle) = worker.handle.take() {
                let _ = handle.join();
            }
        }
    }
}

impl Clone for ThreadPool {
    fn clone(&self) -> Self {
        Self {
            workers: self.workers.clone(),
            tasks: self.tasks.clone(),
            config: self.config.clone(),
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

                    // Try to get a task with timeout to respect shutdown
                    let task = {
                        if let Ok(receiver) = receiver.lock() {
                            receiver.recv_timeout(std::time::Duration::from_millis(100))
                        } else {
                            break;
                        }
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
                        Err(std::sync::mpsc::RecvTimeoutError::Timeout) => {
                            // Timeout occurred, continue loop to check shutdown flag
                            continue;
                        }
                        Err(std::sync::mpsc::RecvTimeoutError::Disconnected) => {
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
#[derive(Debug, Clone, PartialEq)]
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
        F: Fn(T) -> U + Send + Sync + 'static,
        U: Clone + Send + Sync + 'static,
        T: Clone,
    {
        let f = Arc::new(f);
        let results: MaybeLock<Vec<Option<U>>> = MaybeLock::some(vec![None; self.items.len()]);

        for (index, item) in self.items.into_iter().enumerate() {
            let f_clone = Arc::clone(&f);
            let mut results_clone = results.clone();

            self.pool.execute(move || {
                let result = f_clone(item);
                let mut results_guard = results_clone.write();
                let results_vec = results_guard.as_mut();
                results_vec[index] = Some(result);
            });
        }

        self.pool.wait_for_completion();

        let results_guard = results.read();
        let results_vec = results_guard.as_ref();
        results_vec
            .iter()
            .filter_map(|opt| opt.clone())
            .collect()
    }

    /// Filter items in parallel and collect results
    pub fn filter<F>(self, predicate: F) -> Vec<T>
    where
        F: Fn(&T) -> bool + Send + Sync + 'static,
        T: Clone,
    {
        let predicate = Arc::new(predicate);
        let results: MaybeLock<Vec<T>> = MaybeLock::some(Vec::new());

        for item in self.items {
            let predicate_clone = Arc::clone(&predicate);
            let mut results_clone = results.clone();

            self.pool.execute(move || {
                if predicate_clone(&item) {
                    let mut results_guard = results_clone.write();
                    let results_vec = results_guard.as_mut();
                    results_vec.push(item);
                }
            });
        }

        self.pool.wait_for_completion();

        let results_guard = results.read();
        let results_vec = results_guard.as_ref();
        results_vec.clone()
    }

    /// Filter and map in one parallel operation
    pub fn filter_map<U, F>(self, f: F) -> Vec<U>
    where
        F: Fn(T) -> Option<U> + Send + Sync + 'static,
        U: Clone + Send + Sync + 'static,
        T: Clone,
    {
        let f = Arc::new(f);
        let results: MaybeLock<Vec<U>> = MaybeLock::some(Vec::new());

        for item in self.items {
            let f_clone = Arc::clone(&f);
            let mut results_clone = results.clone();

            self.pool.execute(move || {
                if let Some(result) = f_clone(item) {
                    let mut results_guard = results_clone.write();
                    let results_vec = results_guard.as_mut();
                    results_vec.push(result);
                }
            });
        }

        self.pool.wait_for_completion();

        let results_guard = results.read();
        let results_vec = results_guard.as_ref();
        results_vec.clone()
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

#[derive(Debug)]
pub struct WorkStealer<T> {
    queues: Vec<MaybeLock<VecDeque<T>>>,
    current_queue: AtomicUsize,
}
impl<T: Clone> Clone for WorkStealer<T> {
    fn clone(&self) -> Self {
        WorkStealer {
            queues: self.queues.clone(),
            current_queue: self.current_queue.load(Ordering::Relaxed).into(),
        }
    }
}
impl<T: PartialEq> PartialEq for WorkStealer<T> {
    fn eq(&self, other: &Self) -> bool {
        self.queues == other.queues && self.current_queue.load(Ordering::Relaxed) == other.current_queue.load(Ordering::Relaxed)
    }
}
impl<T: PartialEq> Eq for WorkStealer<T> {}
impl<T: Ord> PartialOrd for WorkStealer<T> {
    fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl<T: Ord> Ord for WorkStealer<T> {
    fn cmp(&self, other: &Self) -> core::cmp::Ordering {
        self.current_queue.load(Ordering::Relaxed).cmp(&other.current_queue.load(Ordering::Relaxed))
    }
}   
impl<T: Send + 'static> WorkStealer<T> {
    pub fn new(num_queues: usize) -> Self {
        let mut queues = Vec::with_capacity(num_queues);
        for _ in 0..num_queues {
            queues.push(MaybeLock::some(VecDeque::<T>::new()));
        }

        WorkStealer {
            queues,
            current_queue: AtomicUsize::new(0),
        }
    }

    pub fn push(&mut self, item: T) {
        let queue_idx = self.current_queue.fetch_add(1, Ordering::Relaxed) % self.queues.len();
        let mut queue = self.queues[queue_idx].write();
        let deque = queue.as_mut();
        deque.push_back(item);
    }

    pub fn steal(&mut self, preferred_queue: usize) -> Maybe<T> {
        if preferred_queue < self.queues.len() {
            let mut queue = self.queues[preferred_queue].write();
            let deque = queue.as_mut();
            if let Some(item) = deque.pop_front() {
                return Maybe::Some(item);
            }
        }

        for i in 0..self.queues.len() {
            if i != preferred_queue {
                let mut queue = self.queues[i].write();
                let deque = queue.as_mut();
                if let Some(item) = deque.pop_back() {
                    return Maybe::Some(item);
                }
            }
        }

        Maybe::None
    }
}

/// Global thread pool instance using MaybeLock for safe initialization
static GLOBAL_POOL: MaybeLock<ThreadPool> = MaybeLock::new();

/// Get or initialize the global thread pool
pub fn global_pool() -> &'static ThreadPool {
    GLOBAL_POOL.get_or_init(|| ThreadPool::new())
}

/// Shutdown the global thread pool - call this before program exit
pub fn shutdown_global_pool() {
    // Signal shutdown to the global pool if it exists
    if let Maybe::Some(pool) = GLOBAL_POOL.try_get() {
        pool.tasks.shutdown.store(true, Ordering::SeqCst);
    }
}

/// Execute a closure in parallel using the global thread pool
pub fn spawn<F>(f: F)
where
    F: FnOnce() + Send + Sync + 'static,
{
    let pool = global_pool();
    pool.execute(f);
}

/// Join multiple tasks and wait for completion
pub fn join_all<F, T>(tasks: Vec<F>) -> Vec<T>
where
    F: FnOnce() -> T + Send + Sync + 'static,
    T: Clone + Send + Sync + 'static,
{
    let pool = global_pool();
    let results: MaybeLock<Vec<Option<T>>> = MaybeLock::some(vec![None; tasks.len()]);

    for (index, task) in tasks.into_iter().enumerate() {
        let mut results_clone = results.clone();
        pool.execute(move || {
            let result = task();
            let mut results_guard = results_clone.write();
            let results_vec = results_guard.as_mut();
            results_vec[index] = Some(result);
        });
    }

    pool.wait_for_completion();

    let results_guard = results.read();
    let results_vec = results_guard.as_ref();
    results_vec
        .iter()
        .filter_map(|opt| opt.clone())
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

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
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ThreadHealth {
    Healthy,
    Unresponsive,
    Failed,
    Terminated,
}

/// Thread recovery statistics
#[derive(Debug, Clone,PartialEq,Eq,PartialOrd,Ord,Hash)]
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
#[derive(Debug)]
pub struct Watcher {
    thread_health: MaybeLock<HashMap<usize, ThreadHealth>>,
    thread_heartbeats: MaybeLock<HashMap<usize, Instant>>,
    recovery_stats: MaybeLock<HashMap<usize, RecoveryStats>>,
    watchdog_handle: MaybeLock<Option<JoinHandle<()>>>,
    shutdown: Arc<AtomicBool>,
    check_interval: Duration,
    timeout_threshold: Duration,
}

impl Clone for Watcher {
    fn clone(&self) -> Self {
        Self {
            thread_health: self.thread_health.clone(),
            thread_heartbeats: self.thread_heartbeats.clone(),
            recovery_stats: self.recovery_stats.clone(),
            watchdog_handle: MaybeLock::some(None),
            shutdown: self.shutdown.clone(),
            check_interval: self.check_interval,
            timeout_threshold: self.timeout_threshold,
        }
    }
}

impl PartialEq for Watcher {
    fn eq(&self, other: &Self) -> bool {
        self.thread_health == other.thread_health
            && self.thread_heartbeats == other.thread_heartbeats
            && self.recovery_stats == other.recovery_stats
            && self.shutdown.load(Ordering::Relaxed)
                == other.shutdown.load(Ordering::Relaxed)
            && self.check_interval == other.check_interval
            && self.timeout_threshold == other.timeout_threshold
    }
}

impl Eq for Watcher {}

impl std::hash::Hash for Watcher {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.thread_health.iter().for_each(|(k, v)| {
            k.hash(state);
            v.hash(state);
        });
        self.thread_heartbeats.iter().for_each(|(k, v)| {
            k.hash(state);
            v.hash(state);
        });
        self.recovery_stats.iter().for_each(|(k, v)| {
            k.hash(state);
            v.hash(state);
        });
        self.shutdown.load(Ordering::Relaxed).hash(state);
        self.check_interval.hash(state);
        self.timeout_threshold.hash(state);
    }
}

impl Watcher {
    /// Create a new watchdog with default settings
    pub fn new() -> Self {
        Self::with_intervals(Duration::from_secs(1), Duration::from_secs(5))
    }

    /// Create a watchdog with custom intervals
    pub fn with_intervals(check_interval: Duration, timeout_threshold: Duration) -> Self {
        Watcher {
            thread_health: MaybeLock::some(HashMap::new()),
            thread_heartbeats: MaybeLock::some(HashMap::new()),
            recovery_stats: MaybeLock::some(HashMap::new()),
            watchdog_handle: MaybeLock::some(None),
            shutdown: Arc::new(AtomicBool::new(false)),
            check_interval,
            timeout_threshold,
        }
    }

    /// Start the watchdog monitoring system
    pub fn start(&mut self, thread_count: usize) {
        // Initialize health tracking for all threads
        {
            let mut health_guard = self.thread_health.write();
            let health_map = health_guard.as_mut();
            health_map.clear();
            for i in 0..thread_count {
                health_map.insert(i, ThreadHealth::Healthy);
            }
        }
        {
            let mut heartbeat_guard = self.thread_heartbeats.write();
            let heartbeat_map = heartbeat_guard.as_mut();
            heartbeat_map.clear();
            for i in 0..thread_count {
                heartbeat_map.insert(i, Instant::now());
            }
        }
        {
            let mut stats_guard = self.recovery_stats.write();
            let stats_map = stats_guard.as_mut();
            stats_map.clear();
            for i in 0..thread_count {
                stats_map.insert(i, RecoveryStats::default());
            }
        }

        let mut thread_health = self.thread_health.clone();
        let thread_heartbeats = self.thread_heartbeats.clone();
        let mut recovery_stats = self.recovery_stats.clone();
        let shutdown = self.shutdown.clone();
        let check_interval = self.check_interval;
        let timeout_threshold = self.timeout_threshold;

        let handle = thread::spawn(move || {
            while !shutdown.load(Ordering::Relaxed) {
                thread::sleep(check_interval);

                let now = Instant::now();
                let mut health_guard = thread_health.write();
                let heartbeat_guard = thread_heartbeats.read();
                let mut stats_guard = recovery_stats.write();

                let (heartbeat_map, health_map, stats_map) = (
                    heartbeat_guard.as_ref(),
                    health_guard.as_mut(),
                    stats_guard.as_mut(),
                );
                
                for (&thread_id, &last_heartbeat) in heartbeat_map.iter() {
                    let elapsed = now.duration_since(last_heartbeat);

                    if elapsed > timeout_threshold {
                        if let Some(health) = health_map.get_mut(&thread_id) {
                            match *health {
                                ThreadHealth::Healthy => {
                                    *health = ThreadHealth::Unresponsive;
                                    println!("Warning: Thread {} is unresponsive", thread_id);
                                }
                                ThreadHealth::Unresponsive => {
                                    *health = ThreadHealth::Failed;
                                    if let Some(stats) = stats_map.get_mut(&thread_id) {
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
        });

        let mut handle_guard = self.watchdog_handle.write();
        *handle_guard.as_mut() = Some(handle);
    }

    /// Signal that a thread is alive and working
    pub fn heartbeat(&mut self, thread_id: usize) {
        let mut heartbeat_guard = self.thread_heartbeats.write();
        let heartbeat_map = heartbeat_guard.as_mut();
        heartbeat_map.insert(thread_id, Instant::now());

        let mut health_guard = self.thread_health.write();
        let health_map = health_guard.as_mut();
        if let Some(health) = health_map.get_mut(&thread_id) {
            if *health == ThreadHealth::Unresponsive {
                *health = ThreadHealth::Healthy;
                println!("Info: Thread {} recovered", thread_id);
            }
        }
    }

    /// Mark a thread as completed a task successfully
    pub fn task_completed(&mut self, thread_id: usize) {
        self.heartbeat(thread_id);
        let mut stats_guard = self.recovery_stats.write();
        let stats_map = stats_guard.as_mut();
        if let Some(stats) = stats_map.get_mut(&thread_id) {
            stats.total_tasks_completed += 1;
        }
    }

    /// Get the health status of a specific thread
    pub fn get_thread_health(&self, thread_id: usize) -> Option<ThreadHealth> {
        let health_guard = self.thread_health.read();
        let health_map = health_guard.as_ref();
        health_map.get(&thread_id).copied()
    }

    /// Get recovery statistics for a thread
    pub fn get_recovery_stats(&self, thread_id: usize) -> Option<RecoveryStats> {
        let stats_guard = self.recovery_stats.read();
        let stats_map = stats_guard.as_ref();
        stats_map.get(&thread_id).cloned()
    }

    /// Get overall system health report
    pub fn health_report(&self) -> HashMap<usize, (ThreadHealth, RecoveryStats)> {
        let mut report = HashMap::new();
        let health_guard = self.thread_health.read();
        let stats_guard = self.recovery_stats.read();

        let health_map = health_guard.as_ref();
        let stats_map = stats_guard.as_ref();

        for (&thread_id, &health) in health_map.iter() {
            if let Some(stats) = stats_map.get(&thread_id) {
                report.insert(thread_id, (health, stats.clone()));
            }
        }

        report
    }

    /// Attempt to recover a failed thread
    pub fn recover_thread(&mut self, thread_id: usize) -> bool {
        let mut recovered = false;
        let mut health_guard = self.thread_health.write();
        let health_map = health_guard.as_mut();
        if let Some(health) = health_map.get_mut(&thread_id) {
            if *health == ThreadHealth::Failed {
                *health = ThreadHealth::Healthy;
                
                // Reset heartbeat for recovered thread
                let mut heartbeat_guard = self.thread_heartbeats.write();
                let heartbeat_map = heartbeat_guard.as_mut();
                heartbeat_map.insert(thread_id, Instant::now());
                
                let mut stats_guard = self.recovery_stats.write();
                let stats_map = stats_guard.as_mut();
                if let Some(stats) = stats_map.get_mut(&thread_id) {
                    stats.restarts += 1;
                }
                println!("Info: Thread {} recovery attempted successfully", thread_id);
                recovered = true;
            }
        }
        recovered
    }

    /// Shutdown the watchdog system
    pub fn shutdown(&mut self) {
        self.shutdown.store(true, Ordering::SeqCst);
        
        // Safely extract handle to avoid double-drop
        if let Maybe::Some(mut handle_opt) = self.watchdog_handle.take() {
            if let Some(handle) = handle_opt.take() {
                let _ = handle.join();
            }
        }
    }
}

/// Resilient thread pool with watchdog monitoring and recovery
#[derive(Debug)]
pub struct ResilientThreadPool {
    pool: ThreadPool,
    watchdog: MaybeLock<Watcher>,
    incremental_results: MaybeLock<Vec<Vec<u8>>>, // For incremental processing
    sync_barrier: Arc<AtomicUsize>,
}

impl Drop for ResilientThreadPool {
    fn drop(&mut self) {
        // Safely shutdown watchdog only if it's initialized
        if let Maybe::Some(mut watchdog) = self.watchdog.take() {
            watchdog.shutdown();
        }
        // ThreadPool will be dropped automatically by its own Drop impl
    }
}

impl PartialEq for ResilientThreadPool {
    fn eq(&self, other: &Self) -> bool {
        self.pool == other.pool
            && self.watchdog == other.watchdog
            && self.incremental_results == other.incremental_results
            && self
                .sync_barrier
                .load(core::sync::atomic::Ordering::Relaxed)
                == other
                    .sync_barrier
                    .load(core::sync::atomic::Ordering::Relaxed)
    }
}

impl Eq for ResilientThreadPool {}

impl std::hash::Hash for ResilientThreadPool {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.pool.hash(state);
        self.watchdog.hash(state);
        self.incremental_results.hash(state);
        self.sync_barrier
            .load(core::sync::atomic::Ordering::Relaxed)
            .hash(state);
    }
}

impl ResilientThreadPool {
    /// Create a new resilient thread pool
    pub fn new() -> Self {
        let mut config = ThreadPoolConfig::new();
        config.name_prefix = "resilient";
        config.worker_count = 4;
        config.queue_capacity = Some(1024);
        Self::with_config(config)
    }

    /// Create with custom configuration
    pub fn with_config(config: ThreadPoolConfig) -> Self {
        let pool = ThreadPool::with_config(config.clone());
        let mut watchdog = MaybeLock::some(Watcher::new());
        {
            let mut watchdog_guard = watchdog.write();
            watchdog_guard.as_mut().start(config.worker_count);
        }

        ResilientThreadPool {
            pool,
            watchdog,
            incremental_results: MaybeLock::some(Vec::new()),
            sync_barrier: Arc::new(AtomicUsize::new(0)),
        }
    }
    
    /// Shutdown the resilient thread pool
    pub fn shutdown(&mut self) {
        // Shutdown watchdog first
        if let Maybe::Some(mut watchdog) = self.watchdog.take() {
            watchdog.shutdown();
        }
        // Signal shutdown to the underlying pool
        self.pool.tasks.shutdown.store(true, Ordering::SeqCst);
    }

    /// Execute a task with monitoring and recovery
    pub fn execute_monitored<F>(&mut self, thread_id: usize, task: F)
    where
        F: FnOnce() + Send + Sync + 'static,
    {
        let mut watchdog = self.watchdog.clone();
        
        self.pool.execute(move || {
            // Heartbeat before starting
            {
                let mut watchdog_guard = watchdog.write();
                watchdog_guard.as_mut().heartbeat(thread_id);
            }

            // Execute the task with panic recovery
            let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                task();
            }));

            match result {
                Ok(()) => {
                    // Mark task completed successfully
                    let mut watchdog_guard = watchdog.write();
                    watchdog_guard.as_mut().task_completed(thread_id);
                }
                Err(_) => {
                    println!("Error: Thread {} panicked during task execution", thread_id);
                    // Attempt recovery
                    let mut watchdog_guard = watchdog.write();
                    watchdog_guard.as_mut().recover_thread(thread_id);
                }
            }
        });
    }

    /// Process tasks incrementally with isolation
    pub fn process_incremental<T, F, R>(&mut self, items: Vec<T>, processor: F) -> Vec<R>
    where
        T: Send + Sync + 'static,
        F: Fn(T) -> R + Send + Sync + 'static,
        R: Send + Sync + 'static,
    {
        use std::sync::mpsc;

        let (sender, receiver) = mpsc::channel();
        let processor = Arc::new(processor);
        let item_count = items.len();

        // Submit all tasks to the thread pool
        for (index, item) in items.into_iter().enumerate() {
            let processor_clone = Arc::clone(&processor);
            let sender_clone = sender.clone();
            let thread_id = index % self.pool.worker_count();

            self.execute_monitored(thread_id, move || {
                let result = processor_clone(item);
                let _ = sender_clone.send(result);
            });
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

impl Default for ResilientThreadPool {
    fn default() -> Self {
        Self::new()
    }
}

impl Clone for ResilientThreadPool {
    fn clone(&self) -> Self {
        Self::new()
    }
}
