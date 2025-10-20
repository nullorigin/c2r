#[cfg(test)]
pub mod pattern_matching_tests;

#[cfg(test)]
pub mod pattern_debug_standalone_test;

#[cfg(test)]
pub mod hashmap_test;

#[cfg(test)]
pub mod pattern_cache_test;
#[cfg(test)]
pub mod pattern_range_test;
#[cfg(test)]
pub mod simple_pattern_cache_test;
#[cfg(test)]
pub mod trait_registry_report_test;

// New comprehensive test suites for debugging
#[cfg(test)]
pub mod patternizer_tests;
#[cfg(test)]
pub mod samplizer_tests;
#[cfg(test)]
pub mod handler_integration_tests;
