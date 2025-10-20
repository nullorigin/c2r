#[cfg(test)]
mod simple_tests {
    use crate::pattern::{PatternResult, Patternizer};

    #[test]
    fn test_basic_pattern_cache_functionality() {
        // This is a minimal test to verify basic functionality
        println!("Testing pattern cache basic functionality...");

        // Create a patternizer
        let mut patternizer = Patternizer::new();

        // Add a basic cache entry
        patternizer.match_cache.insert(
            "test_key".to_string(),
            PatternResult::Match { consumed_tokens: 5 },
        );

        // Verify it was inserted
        assert_eq!(patternizer.match_cache.len(), 1);
        println!("✅ Basic cache insertion works");

        // Test basic JSON creation (without file I/O)
        let temp_file = "/tmp/simple_pattern_test.json";

        match patternizer.save_cache_to_json(temp_file) {
            Ok(()) => {
                println!("✅ JSON serialization completed");

                // Try to load it back
                let mut new_patternizer = Patternizer::new();
                match new_patternizer.load_cache_from_json(temp_file) {
                    Ok(()) => {
                        println!("✅ JSON deserialization completed");
                        println!(
                            "Loaded cache entries: {}",
                            new_patternizer.match_cache.len()
                        );
                    }
                    Err(e) => {
                        println!("❌ Load failed: {}", e);
                        panic!("Load failed: {}", e);
                    }
                }

                // Clean up
                let _ = std::fs::remove_file(temp_file);
            }
            Err(e) => {
                println!("❌ Save failed: {}", e);
                panic!("Save failed: {}", e);
            }
        }

        println!("✅ Basic pattern cache test completed!");
    }
}
