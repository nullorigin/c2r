#[cfg(test)]
mod tests {
    use crate::pattern::{PatternResult, Patternizer};
    use std::fs;

    #[test]
    fn test_pattern_cache_save_load() {
        let mut patternizer = Patternizer::new();

        // Create some dummy cache entries
        patternizer.match_cache.insert(
            "test_key".to_string(),
            PatternResult::NoMatch {
                reason: "Test pattern didn't match".to_string(),
            },
        );
        patternizer.match_cache.insert(
            "match_key".to_string(),
            PatternResult::Match { consumed_tokens: 5 },
        );
        patternizer.match_cache.insert(
            "count_key".to_string(),
            PatternResult::CountOf {
                offsets: vec![1..2, 3..4, 7..8],
            },
        );

        // Save cache to temporary file
        let temp_file = "/tmp/test_pattern_cache.json";
        let result = patternizer.save_cache_to_json(temp_file);

        match result {
            Ok(()) => {
                println!("✅ Pattern cache saved successfully");

                // Verify file exists and has content
                if let Ok(content) = fs::read_to_string(temp_file) {
                    println!("Cache file content length: {} bytes", content.len());
                    println!(
                        "Cache content preview: {}",
                        &content[..content.len().min(200)]
                    );
                    assert!(content.len() > 0);

                    // Try to parse as JSON
                    match crate::json::parse(&content) {
                        Ok(json_data) => {
                            println!("✅ JSON parsing successful");

                            // Verify structure
                            assert!(json_data["metadata"].is_object());
                            assert!(json_data["cache"].is_object());

                            println!("✅ JSON structure validation passed");
                        }
                        Err(e) => {
                            panic!("JSON parsing failed: {}", e);
                        }
                    }

                    // Test loading
                    let mut new_patternizer = Patternizer::new();
                    match new_patternizer.load_cache_from_json(temp_file) {
                        Ok(()) => {
                            println!("✅ Pattern cache loaded successfully");
                            assert_eq!(new_patternizer.match_cache.len(), 3);

                            // Verify specific entries were loaded correctly
                            assert!(new_patternizer.match_cache.contains_key("test_key"));
                            assert!(new_patternizer.match_cache.contains_key("match_key"));
                            assert!(new_patternizer.match_cache.contains_key("count_key"));

                            println!("✅ Cache content verified after load");
                        }
                        Err(e) => {
                            panic!("Failed to load cache: {}", e);
                        }
                    }
                }

                // Clean up
                let _ = fs::remove_file(temp_file);
            }
            Err(e) => {
                panic!("Failed to save pattern cache: {}", e);
            }
        }

        println!("✅ Pattern cache save/load test completed successfully");
    }

    #[test]
    fn test_cache_with_all_pattern_result_types() {
        let mut patternizer = Patternizer::new();
        // Add one of each PatternResult type
        patternizer.match_cache.insert(
            "match".to_string(),
            PatternResult::Match {
                consumed_tokens: 10,
            },
        );
        patternizer.match_cache.insert(
            "nomatch".to_string(),
            PatternResult::NoMatch {
                reason: "No pattern found".to_string(),
            },
        );
        patternizer.match_cache.insert(
            "countof".to_string(),
            PatternResult::CountOf {
                offsets: vec![0..1, 2..3, 4..5, 6..7],
            },
        );
        patternizer.match_cache.insert(
            "sequence".to_string(),
            PatternResult::Sequence { range: 5..15 },
        );
        patternizer.match_cache.insert(
            "fuzzy".to_string(),
            PatternResult::Fuzzy {
                offsets: vec![1..2, 5..6, 9..10],
            },
        );
        patternizer.match_cache.insert(
            "reject".to_string(),
            PatternResult::Reject {
                reason: "Pattern rejected".to_string(),
            },
        );

        let temp_file = "/tmp/comprehensive_pattern_cache.json";

        // Test save and load cycle
        assert!(patternizer.save_cache_to_json(temp_file).is_ok());

        let mut new_patternizer = Patternizer::new();
        assert!(new_patternizer.load_cache_from_json(temp_file).is_ok());

        // Verify all entries loaded
        assert_eq!(new_patternizer.match_cache.len(), 6);
        assert!(new_patternizer.match_cache.contains_key("match"));
        assert!(new_patternizer.match_cache.contains_key("nomatch"));
        assert!(new_patternizer.match_cache.contains_key("countof"));
        assert!(new_patternizer.match_cache.contains_key("sequence"));
        assert!(new_patternizer.match_cache.contains_key("fuzzy"));
        assert!(new_patternizer.match_cache.contains_key("reject"));

        // Clean up
        let _ = fs::remove_file(temp_file);

        println!("✅ Comprehensive PatternResult cache test passed");
    }
}
