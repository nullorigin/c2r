use crate::Result;
use crate::config::HandlerPhase::Process;
use crate::error::C2RError;
use crate::pattern::Patternizer;
use crate::sample::{Samplizer, SegmentType};
use crate::{ReportLevel::Info, report};
use std::collections::HashMap;

pub struct Analyzer {
    samplizer: Samplizer,
    patternizer: Patternizer,
}

impl Analyzer {
    pub fn new() -> Self {
        Self {
            samplizer: Samplizer::new(),
            patternizer: Patternizer::new("pattern_analysis"),
        }
    }

    pub fn analyze_patterns_with_samplizer(&mut self) -> Result<()> {
        println!("🔍 Starting Samplizer Pattern Analysis");
        println!("═══════════════════════════════════════════════════════════════════");

        // Test files to analyze
        let test_pairs = vec![
            (
                "test_files/simple_function.c",
                "test_files/simple_function.rs",
            ),
            (
                "test_files/comprehensive_test.c",
                "test_files/comprehensive_test.rs",
            ),
            ("test_files/enum_test.c", "test_files/enum_test.rs"),
            ("test_files/struct_test.c", "test_files/struct_test.rs"),
        ];

        let mut all_generated_patterns = Vec::new();

        // Analyze each file pair
        for (c_file, rust_file) in test_pairs {
            report!(
                "pattern_analysis",
                "analyze_patterns_with_samplizer",
                Info,
                Process,
                format!("Analyzing pair: {} -> {}", c_file, rust_file),
                true
            );

            match self.samplizer.analyze_file_pair(c_file, rust_file) {
                Ok(patterns) => {
                    println!("📊 Generated {} patterns from {}", patterns.len(), c_file);

                    // Display pattern details
                    for pattern in &patterns {
                        println!(
                            "  • {} (confidence: {:.2}) - Type: {:?}",
                            pattern.pattern_name, pattern.confidence_score, pattern.pattern_type
                        );
                        println!(
                            "    Extraction: {}",
                            pattern
                                .extraction_pattern
                                .as_ref()
                                .unwrap_or(&"None".to_string())
                        );
                    }

                    all_generated_patterns.extend(patterns);
                }
                Err(e) => {
                    println!("⚠️  Failed to analyze {}: {:?}", c_file, e);
                }
            }
        }

        // Categorize generated patterns by type
        let mut patterns_by_type: HashMap<SegmentType, Vec<_>> = HashMap::new();
        for pattern in &all_generated_patterns {
            patterns_by_type
                .entry(pattern.pattern_type.clone())
                .or_insert_with(Vec::new)
                .push(pattern);
        }

        // Compare with manual patterns
        self.compare_with_manual_patterns(&patterns_by_type)?;

        // Generate analysis report
        self.generate_pattern_analysis_report(&all_generated_patterns)?;

        Ok(())
    }

    /// Compare samplizer patterns with manually created patterns
    fn compare_with_manual_patterns(
        &self,
        samplizer_patterns: &HashMap<SegmentType, Vec<&crate::sample::SamplizerPattern>>,
    ) -> Result<()> {
        // Get current manual patterns from Patternizer
        let mut manual_patternizer = Patternizer::new("manual_comparison");
        manual_patternizer.initialize_common_patterns();
        report!(
            "pattern_analysis",
            "compare_with_manual_patterns",
            Info,
            Process,
            "Comparing samplizer patterns with manual patterns",
            true
        );

        // Analyze each pattern type
        for (segment_type, samplizer_patterns_list) in samplizer_patterns {
            let manual_pattern_names = self.get_manual_patterns_for_type(segment_type);
            report!(
                "pattern_analysis",
                "compare_with_manual_patterns",
                Info,
                Process,
                format!(
                    "{:?}: {} samplizer patterns vs {} manual patterns",
                    segment_type,
                    samplizer_patterns_list.len(),
                    manual_pattern_names.len()
                ),
                true
            );

            // Report differences
            for samplizer_pattern in samplizer_patterns_list {
                report!(
                    "pattern_analysis",
                    "compare_with_manual_patterns",
                    Info,
                    Process,
                    format!(
                        "Samplizer pattern '{}' (confidence: {:.2}) - extraction: {:?}",
                        samplizer_pattern.pattern_name,
                        samplizer_pattern.confidence_score,
                        samplizer_pattern.extraction_pattern
                    ),
                    true
                );
            }
        }

        Ok(())
    }

    /// Get manual pattern names for a given segment type
    fn get_manual_patterns_for_type(&self, segment_type: &SegmentType) -> Vec<String> {
        match segment_type {
            SegmentType::Function => vec![
                "function".to_string(),
                "main_function".to_string(),
                "function_declaration".to_string(),
                "function_definition".to_string(),
            ],
            SegmentType::Array => vec!["array_declaration".to_string(), "array_multi".to_string()],
            SegmentType::Struct => vec![
                "struct_declaration".to_string(),
                "struct_definition".to_string(),
            ],
            SegmentType::Enum => vec!["enum_declaration".to_string(), "typedef_enum".to_string()],
            SegmentType::TypeDef => vec![
                "typedef_simple".to_string(),
                "typedef_struct".to_string(),
                "typedef_enum".to_string(),
            ],
            SegmentType::Include => vec!["include_directive".to_string()],
            SegmentType::ControlFlow => vec![
                "control_flow_if".to_string(),
                "control_flow_for".to_string(),
                "control_flow_while".to_string(),
            ],
            _ => vec![],
        }
    }

    /// Generate detailed analysis report
    fn generate_pattern_analysis_report(
        &self,
        patterns: &[crate::sample::SamplizerPattern],
    ) -> Result<()> {
        report!(
            "pattern_analysis",
            "generate_pattern_analysis_report",
            Info,
            Process,
            format!("=== SAMPLIZER PATTERN ANALYSIS REPORT ==="),
            true
        );

        report!(
            "pattern_analysis",
            "generate_pattern_analysis_report",
            Info,
            Process,
            format!("Total patterns generated: {}", patterns.len()),
            true
        );

        // High confidence patterns
        let high_confidence = patterns.iter().filter(|p| p.confidence_score > 0.8).count();
        let medium_confidence = patterns
            .iter()
            .filter(|p| p.confidence_score > 0.5 && p.confidence_score <= 0.8)
            .count();
        let low_confidence = patterns
            .iter()
            .filter(|p| p.confidence_score <= 0.5)
            .count();

        report!(
            "pattern_analysis",
            "generate_pattern_analysis_report",
            Info,
            Process,
            format!(
                "Confidence distribution: High(>0.8): {}, Medium(0.5-0.8): {}, Low(<=0.5): {}",
                high_confidence, medium_confidence, low_confidence
            ),
            true
        );

        // Pattern types breakdown
        let mut type_counts: HashMap<SegmentType, usize> = HashMap::new();
        for pattern in patterns {
            *type_counts.entry(pattern.pattern_type.clone()).or_insert(0) += 1;
        }

        for (seg_type, count) in type_counts {
            report!(
                "pattern_analysis",
                "generate_pattern_analysis_report",
                Info,
                Process,
                format!("{:?} patterns: {}", seg_type, count),
                true
            );
        }

        // Integration stats
        report!(
            "pattern_analysis",
            "generate_pattern_analysis_report",
            Info,
            Process,
            format!("Integration stats: {:?}", self.samplizer.integration_stats),
            true
        );

        Ok(())
    }
}
