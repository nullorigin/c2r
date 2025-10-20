#[cfg(test)]
mod range_tests {
    use crate::pattern::{PatternResult, Patternizer};
    use std::fs;

    #[test]
    fn test_range_offsets_serialization() {
        println!("Testing range-based offsets serialization...");

        let mut patternizer = Patternizer::new();

        // Test CountOf with ranges
        patternizer.match_cache.insert(
            "count_ranges".to_string(),
            PatternResult::CountOf {
                offsets: vec![0..3, 5..8, 10..15],
            },
        );

        // Test Fuzzy with ranges
        patternizer.match_cache.insert(
            "fuzzy_ranges".to_string(),
            PatternResult::Fuzzy {
                offsets: vec![2..4, 7..9, 12..20],
            },
        );

        let temp_file = "/tmp/range_pattern_test.json";

        // Save to JSON
        assert!(patternizer.save_cache_to_json(temp_file).is_ok());
        println!("✅ Range serialization successful");

        // Read and verify JSON structure
        if let Ok(content) = fs::read_to_string(temp_file) {
            println!("JSON content preview:");
            let lines: Vec<&str> = content.lines().take(20).collect();
            for line in lines {
                println!("  {}", line);
            }

            // Parse and verify structure
            let json_data = crate::json::parse(&content).expect("JSON should parse");
            assert!(json_data["cache"].is_object());
            println!("✅ JSON structure valid");
        }

        // Load back and verify
        let mut new_patternizer = Patternizer::new();
        assert!(new_patternizer.load_cache_from_json(temp_file).is_ok());

        // Verify loaded data
        assert_eq!(new_patternizer.match_cache.len(), 2);
        assert!(new_patternizer.match_cache.contains_key("count_ranges"));
        assert!(new_patternizer.match_cache.contains_key("fuzzy_ranges"));

        // Verify the actual range values
        if let Some(PatternResult::CountOf { offsets }) =
            new_patternizer.match_cache.get("count_ranges")
        {
            assert_eq!(offsets.len(), 3);
            assert_eq!(offsets[0], 0..3);
            assert_eq!(offsets[1], 5..8);
            assert_eq!(offsets[2], 10..15);
            println!("✅ CountOf ranges loaded correctly");
        } else {
            panic!("CountOf result not found or wrong type");
        }

        if let Some(PatternResult::Fuzzy { offsets }) =
            new_patternizer.match_cache.get("fuzzy_ranges")
        {
            assert_eq!(offsets.len(), 3);
            assert_eq!(offsets[0], 2..4);
            assert_eq!(offsets[1], 7..9);
            assert_eq!(offsets[2], 12..20);
            println!("✅ Fuzzy ranges loaded correctly");
        } else {
            panic!("Fuzzy result not found or wrong type");
        }

        // Clean up
        let _ = fs::remove_file(temp_file);

        println!("✅ Range offset serialization test completed successfully!");
    }
}
