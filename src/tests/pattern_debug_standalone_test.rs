#![cfg(test)]

//! Standalone Pattern Debug Test
//!
//! This test provides pattern debugging functionality as a standalone test
//! rather than integrated into the main conversion process.

use crate::pattern::PatternResult;
use crate::token::Tokenizer;
use crate::Result;
use std::collections::HashSet;
use std::path::Path;
use std::time::SystemTime;

/// Represents a pattern matching attempt for standalone debugging
#[derive(Debug, Clone)]
pub struct PatternMatchAttempt {
    /// The pattern name/ID that was attempted
    pub pattern_name: String,
    /// The handler type (function, struct, etc.)
    pub handler_type: String,
    /// The tokens that were tested against the pattern
    pub tokens: Vec<String>, // Store as strings for JSON serialization
    /// The result of the pattern match
    pub result: PatternMatchResult,
    /// Additional context about where this match was attempted
    pub context: MatchContext,
    /// Timestamp of the attempt
    pub timestamp: SystemTime,
}

/// Simplified pattern result for JSON serialization
#[derive(Debug, Clone)]
pub enum PatternMatchResult {
    Match {
        consumed_tokens: usize,
    },
    CountOf {
        offset_count: usize,
    },
    Sequence {
        range_start: usize,
        range_end: usize,
    },
    Fuzzy {
        offset_count: usize,
    },
    NoMatch {
        reason: String,
    },
    Reject {
        reason: String,
    },
    TypeMismatch {
        expected: String,
        actual: String,
        position: usize,
        reason: String,
    },
    ValueMismatch {
        expected: String,
        actual: String,
        position: usize,
        reason: String,
    },
    StructureMismatch {
        expected_pattern: String,
        actual_structure: String,
        reason: String,
    },
    CachedNegative {
        pattern_id: String,
        cache_hit_count: usize,
        reason: String,
    },

    CachedPositive {
        pattern_id: String,
        cache_hit_count: usize,
        reason: String,
    },
}

/// Context information about where the pattern match was attempted
#[derive(Debug, Clone)]
pub struct MatchContext {
    /// File being processed
    pub file_path: Option<String>,
    /// Line number in source (if available)
    pub line_number: Option<usize>,
    /// Position in token stream
    pub token_position: usize,
    /// Total tokens in the stream
    pub total_tokens: usize,
}

/// Statistics about pattern matching
#[derive(Debug, Clone)]
pub struct PatternDebugData {
    pub total_attempts: usize,
    pub successful_matches: usize,
    pub failed_matches: usize,
    pub patterns_tried: HashSet<String>,
    pub handler_types_used: HashSet<String>,
    pub session_start_time: SystemTime,
    pub input_file: String,
    pub tokens_processed: usize,
}

impl Default for PatternDebugData {
    fn default() -> Self {
        Self {
            total_attempts: 0,
            successful_matches: 0,
            failed_matches: 0,
            patterns_tried: HashSet::new(),
            handler_types_used: HashSet::new(),
            session_start_time: SystemTime::now(),
            input_file: String::new(),
            tokens_processed: 0,
        }
    }
}

/// Standalone pattern debugger that collects pattern matching attempts
pub struct StandalonePatternDebugger {
    attempts: Vec<PatternMatchAttempt>,
    stats: PatternDebugData,
}

impl StandalonePatternDebugger {
    pub fn new(input_file: String) -> Self {
        let mut stats = PatternDebugData::default();
        stats.input_file = input_file;

        Self {
            attempts: Vec::new(),
            stats,
        }
    }

    /// Record a pattern matching attempt
    pub fn record_attempt(
        &mut self,
        pattern_name: String,
        handler_type: String,
        tokens: &[crate::Token],
        result: &PatternResult,
        position: usize,
        total_tokens: usize,
        file_path: Option<String>,
    ) {
        let attempt = PatternMatchAttempt {
            pattern_name: pattern_name.clone(),
            handler_type: handler_type.clone(),
            tokens: tokens.iter().take(5).map(|t| t.to_string()).collect(),
            result: Self::convert_pattern_result(result),
            context: MatchContext {
                file_path,
                line_number: None,
                token_position: position,
                total_tokens,
            },
            timestamp: SystemTime::now(),
        };

        self.attempts.push(attempt);
        self.update_stats(&pattern_name, &handler_type, result);
    }

    fn update_stats(&mut self, pattern_name: &str, handler_type: &str, result: &PatternResult) {
        self.stats.total_attempts += 1;
        self.stats.patterns_tried.insert(pattern_name.to_string());
        self.stats
            .handler_types_used
            .insert(handler_type.to_string());

        match result {
            PatternResult::Match { .. }
            | PatternResult::Sequence { .. }
            | PatternResult::Fuzzy { .. } => {
                self.stats.successful_matches += 1;
            }
            _ => {
                self.stats.failed_matches += 1;
            }
        }
    }

    /// Convert PatternResult to our simplified version
    fn convert_pattern_result(result: &PatternResult) -> PatternMatchResult {
        match result {
            PatternResult::Match { consumed_tokens } => PatternMatchResult::Match {
                consumed_tokens: *consumed_tokens,
            },
            PatternResult::CountOf { offsets } => PatternMatchResult::CountOf {
                offset_count: offsets.len(),
            },
            PatternResult::Sequence { range } => PatternMatchResult::Sequence {
                range_start: range.start,
                range_end: range.end,
            },
            PatternResult::Fuzzy { offsets } => PatternMatchResult::Fuzzy {
                offset_count: offsets.len(),
            },
            PatternResult::NoMatch { reason } => PatternMatchResult::NoMatch {
                reason: reason.clone(),
            },
            PatternResult::Reject { reason } => PatternMatchResult::Reject {
                reason: reason.clone(),
            },
            PatternResult::TypeMismatch {
                expected_type,
                actual_type,
                position,
                reason,
            } => PatternMatchResult::TypeMismatch {
                expected: format!("{:?}", expected_type),
                actual: format!("{:?}", actual_type),
                position: *position,
                reason: reason.clone(),
            },
            PatternResult::ValueMismatch {
                expected_value,
                actual_value,
                position,
                reason,
            } => PatternMatchResult::ValueMismatch {
                expected: expected_value.clone(),
                actual: actual_value.clone(),
                position: *position,
                reason: reason.clone(),
            },
            PatternResult::StructureMismatch {
                expected_pattern,
                actual_structure,
                reason,
            } => PatternMatchResult::StructureMismatch {
                expected_pattern: expected_pattern.clone(),
                actual_structure: actual_structure.clone(),
                reason: reason.clone(),
            },
            PatternResult::CachedNegative {
                pattern_id,
                cache_hit_count,
                reason,
            } => PatternMatchResult::CachedNegative {
                pattern_id: pattern_id.clone(),
                cache_hit_count: *cache_hit_count,
                reason: reason.clone(),
            },
            PatternResult::CachedPositive {
                pattern_id,
                cache_hit_count,
                reason,
            } => PatternMatchResult::CachedPositive {
                pattern_id: pattern_id.clone(),
                cache_hit_count: *cache_hit_count,
                reason: reason.clone(),
            },
        }
    }

    /// Export all collected pattern debugging data to JSON files
    pub fn export_results(&self, output_dir: &str) -> std::result::Result<(), std::io::Error> {
        std::fs::create_dir_all(output_dir)?;

        // Separate matched and unmatched attempts
        let (matched_attempts, unmatched_attempts): (Vec<_>, Vec<_>) =
            self.attempts.iter().partition(|attempt| {
                matches!(
                    attempt.result,
                    PatternMatchResult::Match { .. }
                        | PatternMatchResult::Sequence { .. }
                        | PatternMatchResult::Fuzzy { .. }
                )
            });

        // Export matched patterns
        self.export_attempts_to_json(
            &matched_attempts,
            &format!("{}/matched_patterns.json", output_dir),
            true,
        )?;

        // Export unmatched patterns
        self.export_attempts_to_json(
            &unmatched_attempts,
            &format!("{}/unmatched_patterns.json", output_dir),
            false,
        )?;

        // Export statistics
        self.export_statistics_to_json(&format!("{}/pattern_statistics.json", output_dir))?;

        println!("Pattern debugging data exported to {}/", output_dir);
        println!("  - Matched patterns: {}", matched_attempts.len());
        println!("  - Unmatched patterns: {}", unmatched_attempts.len());
        println!("  - Total attempts: {}", self.stats.total_attempts);

        Ok(())
    }

    fn export_attempts_to_json(
        &self,
        attempts: &[&PatternMatchAttempt],
        file_path: &str,
        is_matched: bool,
    ) -> std::result::Result<(), std::io::Error> {
        let key = if is_matched {
            "matched_patterns"
        } else {
            "unmatched_patterns"
        };
        let mut json = format!("{{\n  \"{}\": [\n", key);

        for (i, attempt) in attempts.iter().enumerate() {
            json.push_str("    {\n");
            json.push_str(&format!(
                "      \"pattern_name\": \"{}\",\n",
                attempt.pattern_name
            ));
            json.push_str(&format!(
                "      \"handler_type\": \"{}\",\n",
                attempt.handler_type
            ));
            json.push_str(&format!("      \"tokens\": {:?},\n", attempt.tokens));
            json.push_str(&format!(
                "      \"token_position\": {},\n",
                attempt.context.token_position
            ));
            json.push_str(&format!(
                "      \"total_tokens\": {},\n",
                attempt.context.total_tokens
            ));

            match &attempt.result {
                PatternMatchResult::Match { consumed_tokens } => {
                    json.push_str("      \"result\": \"Match\",\n");
                    json.push_str(&format!("      \"consumed_tokens\": {}\n", consumed_tokens));
                }
                PatternMatchResult::NoMatch { reason } => {
                    json.push_str("      \"result\": \"NoMatch\",\n");
                    json.push_str(&format!(
                        "      \"reason\": \"{}\"\n",
                        reason.replace('"', "\\\"")
                    ));
                }
                _ => {
                    json.push_str("      \"result\": \"Other\"\n");
                }
            }

            json.push_str("    }");
            if i < attempts.len() - 1 {
                json.push_str(",");
            }
            json.push_str("\n");
        }

        json.push_str("  ]\n}");

        std::fs::write(file_path, json)?;
        Ok(())
    }

    fn export_statistics_to_json(
        &self,
        file_path: &str,
    ) -> std::result::Result<(), std::io::Error> {
        let mut json = String::from("{\n  \"statistics\": {\n");
        json.push_str(&format!(
            "    \"total_attempts\": {},\n",
            self.stats.total_attempts
        ));
        json.push_str(&format!(
            "    \"successful_matches\": {},\n",
            self.stats.successful_matches
        ));
        json.push_str(&format!(
            "    \"failed_matches\": {},\n",
            self.stats.failed_matches
        ));
        json.push_str(&format!(
            "    \"tokens_processed\": {},\n",
            self.stats.tokens_processed
        ));
        json.push_str(&format!(
            "    \"input_file\": \"{}\",\n",
            self.stats.input_file
        ));
        json.push_str(&format!(
            "    \"session_start\": \"{:?}\",\n",
            self.stats.session_start_time
        ));

        json.push_str("    \"patterns_tried\": [");
        let patterns: Vec<&String> = self.stats.patterns_tried.iter().collect();
        for (i, pattern) in patterns.iter().enumerate() {
            json.push_str(&format!("\"{}\"", pattern));
            if i < patterns.len() - 1 {
                json.push_str(", ");
            }
        }
        json.push_str("],\n");

        json.push_str("    \"handler_types_used\": [");
        let handlers: Vec<&String> = self.stats.handler_types_used.iter().collect();
        for (i, handler) in handlers.iter().enumerate() {
            json.push_str(&format!("\"{}\"", handler));
            if i < handlers.len() - 1 {
                json.push_str(", ");
            }
        }
        json.push_str("]\n");

        json.push_str("  }\n}");

        std::fs::write(file_path, json)?;
        Ok(())
    }

    /// Display a summary of pattern matching results
    pub fn display_summary(&self) {
        println!("🔍 Pattern Debugging Summary");
        println!("═══════════════════════════════════════════════════════════════");
        println!("📁 Input file: {}", self.stats.input_file);
        println!("📊 Total attempts: {}", self.stats.total_attempts);
        println!("✅ Successful matches: {}", self.stats.successful_matches);
        println!("❌ Failed matches: {}", self.stats.failed_matches);
        println!(
            "🎯 Unique patterns tried: {}",
            self.stats.patterns_tried.len()
        );
        println!(
            "🔧 Handler types used: {}",
            self.stats.handler_types_used.len()
        );

        if self.stats.total_attempts > 0 {
            let success_rate =
                (self.stats.successful_matches as f64 / self.stats.total_attempts as f64) * 100.0;
            println!("📈 Success rate: {:.1}%", success_rate);
        }
    }
}

/// Test that demonstrates standalone pattern debugging
#[test]
fn test_standalone_pattern_debugging() -> Result<()> {
    println!("🧪 Testing standalone pattern debugging...");

    // Create a test debugger
    let mut debugger = StandalonePatternDebugger::new("test_file.c".to_string());

    // Create some test tokens
    let mut tokenizer = Tokenizer::new();
    let c_code = b"int main(int argc, char* argv[]) { return 0; }".to_vec();
    let tokens = tokenizer.tokenize(c_code)?;

    // Test recording different pattern results
    debugger.record_attempt(
        "function_pattern".to_string(),
        "function_handler".to_string(),
        &tokens[0..5],
        &PatternResult::Match { consumed_tokens: 3 },
        0,
        tokens.len(),
        Some("test_file.c".to_string()),
    );

    debugger.record_attempt(
        "struct_pattern".to_string(),
        "struct_handler".to_string(),
        &tokens[5..8],
        &PatternResult::NoMatch {
            reason: "Not a struct".to_string(),
        },
        5,
        tokens.len(),
        Some("test_file.c".to_string()),
    );

    debugger.record_attempt(
        "enum_pattern".to_string(),
        "enum_handler".to_string(),
        &tokens[8..10],
        &PatternResult::Reject {
            reason: "Wrong token type".to_string(),
        },
        8,
        tokens.len(),
        Some("test_file.c".to_string()),
    );

    // Display summary
    debugger.display_summary();

    // Export results to a test directory
    let test_output_dir = "test_pattern_debug_output";
    debugger.export_results(test_output_dir)?;

    // Verify files were created
    assert!(Path::new(&format!("{}/matched_patterns.json", test_output_dir)).exists());
    assert!(Path::new(&format!("{}/unmatched_patterns.json", test_output_dir)).exists());
    assert!(Path::new(&format!("{}/pattern_statistics.json", test_output_dir)).exists());

    // Clean up test files
    let _ = std::fs::remove_dir_all(test_output_dir);

    println!("✅ Standalone pattern debugging test passed!");
    Ok(())
}

/// Comprehensive test with real C code parsing and pattern matching
#[test]
fn test_comprehensive_standalone_pattern_debugging() -> Result<()> {
    println!("🧪 Testing comprehensive standalone pattern debugging with real C code...");

    let mut debugger = StandalonePatternDebugger::new("comprehensive_test.c".to_string());

    // Use more complex C code
    let c_code = b"
        #include <stdio.h>
        
        typedef struct Point {
            int x;
            int y;
        } Point;
        
        enum Color {
            RED,
            GREEN,
            BLUE
        };
        
        int main(int argc, char* argv[]) {
            Point p = {10, 20};
            enum Color c = RED;
            printf(\"Point: (%d, %d), Color: %d\\n\", p.x, p.y, c);
            return 0;
        }
    "
        .to_vec();

    let mut tokenizer = Tokenizer::new();
    let tokens = tokenizer.tokenize(c_code)?;

    println!(
        "📊 Tokenized {} tokens from comprehensive C code",
        tokens.len()
    );

    // Simulate pattern matching attempts for different constructs
    let pattern_tests = vec![
        ("include_pattern", "include_handler"),
        ("typedef_pattern", "typedef_handler"),
        ("struct_pattern", "struct_handler"),
        ("enum_pattern", "enum_handler"),
        ("function_pattern", "function_handler"),
        ("variable_pattern", "variable_handler"),
        ("printf_pattern", "function_handler"),
    ];

    // Test each pattern against different token windows
    for (pattern_name, handler_type) in pattern_tests {
        for window_start in (0..tokens.len()).step_by(10) {
            let window_end = (window_start + 5).min(tokens.len());
            if window_start < window_end {
                let window_tokens = &tokens[window_start..window_end];

                // Simulate different match results based on pattern type
                let result = match pattern_name {
                    "include_pattern" => {
                        if window_tokens
                            .iter()
                            .any(|t| t.to_string().contains("include"))
                        {
                            PatternResult::Match { consumed_tokens: 3 }
                        } else {
                            PatternResult::NoMatch {
                                reason: "No include found".to_string(),
                            }
                        }
                    }
                    "struct_pattern" => {
                        if window_tokens.iter().any(|t| t.to_string() == "struct") {
                            PatternResult::Match { consumed_tokens: 4 }
                        } else {
                            PatternResult::NoMatch {
                                reason: "No struct keyword".to_string(),
                            }
                        }
                    }
                    "enum_pattern" => {
                        if window_tokens.iter().any(|t| t.to_string() == "enum") {
                            PatternResult::Match { consumed_tokens: 2 }
                        } else {
                            PatternResult::NoMatch {
                                reason: "No enum keyword".to_string(),
                            }
                        }
                    }
                    "function_pattern" => {
                        if window_tokens.iter().any(|t| t.to_string() == "(") {
                            PatternResult::Fuzzy {
                                offsets: vec![0..1, 2..3],
                            }
                        } else {
                            PatternResult::Reject {
                                reason: "No function signature".to_string(),
                            }
                        }
                    }
                    _ => PatternResult::NoMatch {
                        reason: "Generic no match".to_string(),
                    },
                };

                debugger.record_attempt(
                    pattern_name.to_string(),
                    handler_type.to_string(),
                    window_tokens,
                    &result,
                    window_start,
                    tokens.len(),
                    Some("comprehensive_test.c".to_string()),
                );
            }
        }
    }

    // Display comprehensive summary
    debugger.display_summary();

    // Export to test directory
    let test_output_dir = "comprehensive_pattern_debug_output";
    debugger.export_results(test_output_dir)?;

    // Verify comprehensive results
    let matched_content =
        std::fs::read_to_string(&format!("{}/matched_patterns.json", test_output_dir))?;
    let unmatched_content =
        std::fs::read_to_string(&format!("{}/unmatched_patterns.json", test_output_dir))?;
    let stats_content =
        std::fs::read_to_string(&format!("{}/pattern_statistics.json", test_output_dir))?;

    assert!(
        !matched_content.is_empty(),
        "Matched patterns file should not be empty"
    );
    assert!(
        !unmatched_content.is_empty(),
        "Unmatched patterns file should not be empty"
    );
    assert!(
        !stats_content.is_empty(),
        "Statistics file should not be empty"
    );

    // Verify some expected content
    assert!(
        stats_content.contains("comprehensive_test.c"),
        "Statistics should contain input filename"
    );
    assert!(
        matched_content.contains("matched_patterns"),
        "Matched file should contain matched_patterns key"
    );
    assert!(
        unmatched_content.contains("unmatched_patterns"),
        "Unmatched file should contain unmatched_patterns key"
    );

    // Clean up
    let _ = std::fs::remove_dir_all(test_output_dir);

    println!("✅ Comprehensive standalone pattern debugging test passed!");
    println!(
        "   - Recorded {} total attempts",
        debugger.stats.total_attempts
    );
    println!(
        "   - Found {} successful matches",
        debugger.stats.successful_matches
    );
    println!(
        "   - Tried {} unique patterns",
        debugger.stats.patterns_tried.len()
    );

    Ok(())
}
