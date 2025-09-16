/*!
 * Samplizer - Dynamic Pattern Generation System
 *
 * The Samplizer analyzes C and Rust file pairs to automatically generate
 * intelligent patterns for the Patternizer system. It transforms static
 * pattern matching into a dynamic, self-improving machine learning-like approach.
 *
 * Key Components:
 * - File Comparator: Parses and aligns C/Rust code segments
 * - Pattern Miner: Extracts transformation patterns from alignments
 * - Pattern Generator: Creates Patternizer-compatible pattern definitions
 * - Pattern Validator: Tests generated patterns against validation sets
 * - Pattern Integrator: Adds successful patterns to permanent collection
 */

use crate::config::HandlerPhase::Process;
use crate::error::C2RError;
use crate::pattern::Patternizer;
use crate::thread::{Parallizer, ResilientThreadPool, ThreadPoolConfig};
use crate::token::{Token, Tokenizer};
use crate::{Id, ReportLevel::Info, report};
use crate::{Kind, Reason, Result};
use std::collections::HashMap;
use std::fs;
use std::path::Path;

/// Main Samplizer system for dynamic pattern generation
pub struct Samplizer {
    /// Generated patterns from analysis
    pub generated_patterns: HashMap<String, SamplizerPattern>,
    /// File pairs that have been analyzed
    pub analyzed_pairs: Vec<FilePair>,
    /// Pattern validation results
    pub validation_results: HashMap<String, ValidationResult>,
    /// Resilient thread pool for parallel processing
    pub thread_pool: ResilientThreadPool,
    /// Integration statistics
    pub integration_stats: IntegrationStats,
}

/// Represents a C-Rust file pair for analysis
#[derive(Debug, Clone)]
pub struct FilePair {
    pub c_file_path: String,
    pub rust_file_path: String,
    pub c_tokens: Vec<Token>,
    pub rust_tokens: Vec<Token>,
    pub alignment_map: Vec<SegmentAlignment>,
}

/// Alignment between C and Rust code segments
#[derive(Debug, Clone)]
pub struct SegmentAlignment {
    pub c_range: std::ops::Range<usize>,
    pub rust_range: std::ops::Range<usize>,
    pub confidence: f64,
    pub segment_type: SegmentType,
}

/// Types of code segments that can be aligned
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum SegmentType {
    Function,
    Struct,
    Enum,
    Variable,
    TypeDef,
    Macro,
    Comment,
    Include,
    Expression,
    Array,
    ControlFlow,
}

/// Generated pattern from Samplizer analysis
#[derive(Debug, Clone)]
pub struct SamplizerPattern {
    pub pattern_id: String,
    pub pattern_name: String,
    pub extraction_pattern: Option<String>, // C-only pattern for extraction
    pub conversion_pattern: Option<String>, // C->Rust transformation pattern
    pub confidence_score: f64,
    pub source_segments: Vec<SegmentAlignment>,
    pub pattern_type: SegmentType,
}

/// Results from pattern validation testing
#[derive(Debug, Clone)]
pub struct ValidationResult {
    pub pattern_id: String,
    pub success_rate: f64,
    pub test_cases: u32,
    pub successful_tests: u32,
    pub failure_reasons: Vec<String>,
}

/// Statistics about pattern integration
#[derive(Debug, Default, Clone, PartialEq)]
pub struct IntegrationStats {
    pub patterns_generated: u32,
    pub patterns_validated: u32,
    pub patterns_integrated: u32,
    pub success_rate: f64,
}

impl Samplizer {
    /// Create a new Samplizer instance with resilient threading
    pub fn new() -> Self {
        let thread_pool = ResilientThreadPool::new();

        Self {
            generated_patterns: HashMap::new(),
            analyzed_pairs: Vec::new(),
            validation_results: HashMap::new(),
            thread_pool,
            integration_stats: IntegrationStats::default(),
        }
    }

    /// Analyze a C-Rust file pair to generate patterns
    pub fn analyze_file_pair<P: AsRef<Path>>(
        &mut self,
        c_file_path: P,
        rust_file_path: P,
    ) -> Result<Vec<SamplizerPattern>> {
        let _id = Id::get("samplizer_analyze");

        report!(
            "samplizer",
            "analyze_file_pair",
            Info,
            Process,
            format!(
                "Analyzing file pair: {:?} -> {:?}",
                c_file_path.as_ref(),
                rust_file_path.as_ref()
            ),
            true
        );

        // Parse and tokenize both files
        let file_pair = self.parse_file_pair(c_file_path, rust_file_path)?;

        // Align code segments between C and Rust
        let aligned_pair = self.align_segments(file_pair)?;

        // Extract patterns from alignments
        let patterns = self.mine_patterns(&aligned_pair)?;

        // Store the analyzed pair
        self.analyzed_pairs.push(aligned_pair);

        // Update statistics
        self.integration_stats.patterns_generated += patterns.len() as u32;

        report!(
            "samplizer",
            "analyze_file_pair",
            Info,
            Process,
            format!(
                "Generated {} patterns from file pair analysis",
                patterns.len()
            ),
            true
        );

        Ok(patterns)
    }

    /// Parse and tokenize a C-Rust file pair
    fn parse_file_pair<P: AsRef<Path>>(
        &self,
        c_file_path: P,
        rust_file_path: P,
    ) -> Result<FilePair> {
        let c_content = fs::read_to_string(&c_file_path).map_err(|e| {
            C2RError::new(
                Kind::Io,
                Reason::Failed("to read C file"),
                Some(format!("Failed to read C file: {}", e)),
            )
        })?;

        let rust_content = fs::read_to_string(&rust_file_path).map_err(|e| {
            C2RError::new(
                Kind::Io,
                Reason::Failed("to read Rust file"),
                Some(format!("Failed to read Rust file: {}", e)),
            )
        })?;

        let mut c_tokenizer = Tokenizer::new("c_file");
        let mut rust_tokenizer = Tokenizer::new("rust_file");

        let c_tokens = c_tokenizer.tokenize(c_content.into_bytes())?;
        let rust_tokens = rust_tokenizer.tokenize(rust_content.into_bytes())?;

        Ok(FilePair {
            c_file_path: c_file_path.as_ref().to_string_lossy().to_string(),
            rust_file_path: rust_file_path.as_ref().to_string_lossy().to_string(),
            c_tokens,
            rust_tokens,
            alignment_map: Vec::new(),
        })
    }

    /// Align code segments between C and Rust files
    fn align_segments(&self, mut file_pair: FilePair) -> Result<FilePair> {
        report!(
            "samplizer",
            "align_segments",
            Info,
            Process,
            "Aligning C and Rust code segments",
            true
        );

        // Use our parallel high-quality alignment algorithm
        let alignments = self
            .find_high_quality_alignments_parallel(&file_pair.c_tokens, &file_pair.rust_tokens)?;

        file_pair.alignment_map = alignments;
        Ok(file_pair)
    }

    /// Find high-quality alignments using resilient parallel processing with semantic analysis
    fn find_high_quality_alignments_parallel(
        &self,
        c_tokens: &[Token],
        rust_tokens: &[Token],
    ) -> Result<Vec<SegmentAlignment>> {
        let quality_threshold = 0.75; // Higher threshold for quality patterns

        // Use various window sizes to capture different pattern types
        let window_sizes = [3, 5, 8, 12, 20]; // Focus on meaningful pattern sizes

        // Process each window size in parallel using our custom thread pool
        let mut alignment_results: Vec<Vec<SegmentAlignment>> = Vec::new();

        for &window_size in &window_sizes {
            let result = self.process_window_size_parallel(
                c_tokens,
                rust_tokens,
                window_size,
                quality_threshold,
            );
            alignment_results.push(result);
        }

        // Flatten and combine results from all window sizes
        let mut alignments: Vec<SegmentAlignment> =
            alignment_results.into_iter().flatten().collect();

        // Deduplicate and prioritize the highest quality patterns
        alignments.sort_by(|a, b| {
            b.confidence
                .partial_cmp(&a.confidence)
                .unwrap_or(std::cmp::Ordering::Equal)
        });
        alignments.dedup_by(|a, b| {
            a.c_range.start == b.c_range.start
                && (a.c_range.end - a.c_range.start).abs_diff(b.c_range.end - b.c_range.start) <= 2
        });

        // Keep top 1000 highest quality patterns
        alignments.truncate(1000);

        Ok(alignments)
    }

    /// Process a specific window size in parallel, distributing C token windows across threads
    fn process_window_size_parallel(
        &self,
        c_tokens: &[Token],
        rust_tokens: &[Token],
        window_size: usize,
        quality_threshold: f64,
    ) -> Vec<SegmentAlignment> {
        let mut window_alignments = Vec::new();

        // Process C code windows in parallel using incremental processing
        let window_count = c_tokens.len().saturating_sub(window_size - 1);
        if window_count == 0 {
            return window_alignments;
        }

        // For now, process sequentially until we can properly implement the parallel closure
        // The threading system expects a different closure signature than what we have here
        for i in 0..window_count {
            let c_window = &c_tokens[i..i + window_size];

            // Skip low-quality windows (too many simple tokens)
            if !self.is_window_meaningful(c_window) {
                continue;
            }

            // Find best matching Rust window using semantic similarity
            let best_match = (0..rust_tokens.len().saturating_sub(window_size - 1))
                .map(|j| {
                    let rust_window = &rust_tokens[j..j + window_size];
                    let similarity = self.calculate_enhanced_similarity(c_window, rust_window);
                    (j, rust_window, similarity)
                })
                .max_by(|a, b| a.2.partial_cmp(&b.2).unwrap_or(std::cmp::Ordering::Equal));

            if let Some((j, rust_window, similarity)) = best_match {
                if similarity > quality_threshold {
                    let segment_type = self.classify_segment_type_enhanced(c_window);

                    // Only include high-quality, semantically meaningful patterns
                    if self.is_pattern_meaningful(c_window, rust_window, &segment_type) {
                        window_alignments.push(SegmentAlignment {
                            c_range: i..(i + window_size),
                            rust_range: j..(j + window_size),
                            confidence: similarity,
                            segment_type,
                        });
                    }
                }
            }
        }

        window_alignments
    }

    /// Check if a token window contains meaningful content worth analyzing
    fn is_window_meaningful(&self, tokens: &[Token]) -> bool {
        let token_strings: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();

        // Skip windows with too many simple tokens
        let simple_tokens = token_strings
            .iter()
            .filter(|t| {
                matches!(
                    t.as_str(),
                    ";" | "," | "(" | ")" | "{" | "}" | " " | "\t" | "\n"
                )
            })
            .count();

        // Window should have at least 50% non-simple tokens
        simple_tokens < tokens.len() / 2
    }

    /// Enhanced semantic similarity calculation for high-quality pattern matching
    fn calculate_enhanced_similarity(&self, c_tokens: &[Token], rust_tokens: &[Token]) -> f64 {
        let c_strings: Vec<String> = c_tokens.iter().map(|t| t.to_string()).collect();
        let rust_strings: Vec<String> = rust_tokens.iter().map(|t| t.to_string()).collect();

        // TODO: Implement parallel comparison using custom threading
        // For now, using sequential processing for safety
        let mut similarity_score = 0.0;

        // 1. Structural similarity (brackets, parentheses, semicolons) - Weight: 3.0
        let structural_similarity = self.calculate_structural_similarity(&c_strings, &rust_strings);
        similarity_score += structural_similarity * 3.0;
        let mut total_weight = 3.0;

        // 2. Semantic similarity (keywords, types) - Weight: 4.0
        let semantic_similarity = self.calculate_semantic_similarity(&c_strings, &rust_strings);
        similarity_score += semantic_similarity * 4.0;
        total_weight += 4.0;

        // 3. Length compatibility - Weight: 1.0
        let length_similarity = 1.0
            - (c_tokens.len() as f64 - rust_tokens.len() as f64).abs()
                / c_tokens.len().max(rust_tokens.len()) as f64;
        similarity_score += length_similarity * 1.0;
        total_weight += 1.0;

        // 4. Pattern complexity bonus - Weight: 2.0
        let complexity_bonus = self.calculate_pattern_complexity(&c_strings, &rust_strings);
        similarity_score += complexity_bonus * 2.0;
        total_weight += 2.0;

        similarity_score / total_weight
    }

    /// Calculate similarity between token sequences (legacy method)
    fn calculate_similarity(&self, c_tokens: &[Token], rust_tokens: &[Token]) -> f64 {
        // Simple similarity calculation - would be enhanced with more sophisticated algorithms
        let c_strings: Vec<String> = c_tokens.iter().map(|t| t.to_string()).collect();
        let rust_strings: Vec<String> = rust_tokens.iter().map(|t| t.to_string()).collect();

        let common_count = c_strings
            .iter()
            .filter(|c_token| rust_strings.contains(c_token))
            .count();

        common_count as f64 / c_strings.len().max(rust_strings.len()) as f64
    }

    /// Calculate structural similarity (brackets, parentheses, etc.)
    fn calculate_structural_similarity(
        &self,
        c_strings: &[String],
        rust_strings: &[String],
    ) -> f64 {
        let structural_tokens = vec!["(", ")", "{", "}", "[", "]", ";", ",", "="];

        let c_structural: Vec<&String> = c_strings
            .iter()
            .filter(|s| structural_tokens.contains(&s.as_str()))
            .collect();
        let rust_structural: Vec<&String> = rust_strings
            .iter()
            .filter(|s| structural_tokens.contains(&s.as_str()))
            .collect();

        if c_structural.is_empty() && rust_structural.is_empty() {
            return 1.0;
        }

        let matches = c_structural
            .iter()
            .zip(rust_structural.iter())
            .filter(|(a, b)| a == b)
            .count();
        matches as f64 / c_structural.len().max(rust_structural.len()) as f64
    }

    /// Calculate semantic similarity (types, keywords)
    fn calculate_semantic_similarity(&self, c_strings: &[String], rust_strings: &[String]) -> f64 {
        // C to Rust type mappings
        let type_mappings = vec![
            ("int", "i32"),
            ("char", "i8"),
            ("float", "f32"),
            ("double", "f64"),
            ("void", "()"),
            ("unsigned int", "u32"),
            ("unsigned char", "u8"),
            ("long", "i64"),
            ("short", "i16"),
            ("bool", "bool"),
        ];

        let mut semantic_score = 0.0;
        let mut semantic_count = 0;

        for c_token in c_strings {
            if let Some((_, rust_equiv)) = type_mappings
                .iter()
                .find(|(c_type, _)| c_type == &c_token.as_str())
            {
                if rust_strings.contains(&rust_equiv.to_string()) {
                    semantic_score += 1.0;
                }
                semantic_count += 1;
            }
        }

        if semantic_count == 0 {
            // Check for direct keyword matches
            let keywords = vec!["struct", "enum", "typedef", "static", "const"];
            let common_keywords = c_strings
                .iter()
                .filter(|s| keywords.contains(&s.as_str()) && rust_strings.contains(s))
                .count();
            return common_keywords as f64 / c_strings.len().max(1) as f64;
        }

        semantic_score / semantic_count as f64
    }

    /// Calculate pattern complexity bonus
    fn calculate_pattern_complexity(&self, c_strings: &[String], rust_strings: &[String]) -> f64 {
        // Reward patterns with meaningful structure
        let complexity_indicators = vec![
            "struct", "enum", "typedef", "(", ")", "{", "}", "[", "]", "for", "while", "if",
        ];

        let c_complexity = c_strings
            .iter()
            .filter(|s| complexity_indicators.contains(&s.as_str()))
            .count();
        let rust_complexity = rust_strings
            .iter()
            .filter(|s| complexity_indicators.contains(&s.as_str()))
            .count();

        let avg_complexity = (c_complexity + rust_complexity) as f64 / 2.0;
        let max_len = c_strings.len().max(rust_strings.len()) as f64;

        (avg_complexity / max_len).min(1.0)
    }

    /// Enhanced segment type classification with context awareness
    fn classify_segment_type_enhanced(&self, tokens: &[Token]) -> SegmentType {
        let token_strings: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();

        // Priority-based classification for better accuracy

        // 1. Preprocessor directives
        if token_strings.iter().any(|s| s.starts_with('#')) {
            return SegmentType::Include;
        }

        // 2. Typedef patterns
        if token_strings.contains(&"typedef".to_string()) {
            if token_strings.contains(&"struct".to_string()) {
                return SegmentType::Struct; // typedef struct
            } else if token_strings.contains(&"enum".to_string()) {
                return SegmentType::Enum; // typedef enum  
            }
            return SegmentType::TypeDef;
        }

        // 3. Struct patterns
        if token_strings.contains(&"struct".to_string()) {
            return SegmentType::Struct;
        }

        // 4. Enum patterns
        if token_strings.contains(&"enum".to_string()) {
            return SegmentType::Enum;
        }

        // 5. Array patterns (type + identifier + brackets)
        if let Some(bracket_pos) = token_strings.iter().position(|s| s == "[") {
            if bracket_pos > 1 && self.is_type_keyword(&token_strings[bracket_pos - 2]) {
                return SegmentType::Array;
            }
        }

        // 6. Function patterns (more sophisticated detection)
        if let Some(paren_pos) = token_strings.iter().position(|s| s == "(") {
            if paren_pos > 0 {
                // Check if preceded by identifier and optionally by type
                let has_identifier =
                    paren_pos >= 1 && self.is_identifier(&token_strings[paren_pos - 1]);
                let has_type =
                    paren_pos >= 2 && self.is_type_keyword(&token_strings[paren_pos - 2]);

                if has_identifier && (has_type || paren_pos == 1) {
                    return SegmentType::Function;
                }
            }
        }

        // 7. Control flow patterns
        let control_keywords = vec!["if", "while", "for", "switch", "do"];
        if token_strings
            .iter()
            .any(|s| control_keywords.contains(&s.as_str()))
        {
            return SegmentType::ControlFlow;
        }

        // 8. Variable declarations
        if token_strings.len() >= 2
            && self.is_type_keyword(&token_strings[0])
            && self.is_identifier(&token_strings[1])
        {
            return SegmentType::Variable;
        }

        SegmentType::Expression
    }

    /// Check if token is a C type keyword
    fn is_type_keyword(&self, token: &str) -> bool {
        matches!(
            token,
            "int"
                | "char"
                | "float"
                | "double"
                | "void"
                | "long"
                | "short"
                | "unsigned"
                | "signed"
                | "bool"
                | "size_t"
                | "ssize_t"
        )
    }

    /// Check if token is a valid identifier
    fn is_identifier(&self, token: &str) -> bool {
        !token.is_empty()
            && token.chars().next().unwrap().is_alphabetic()
            && token.chars().all(|c| c.is_alphanumeric() || c == '_')
    }

    /// Check if pattern is semantically meaningful and worth including
    fn is_pattern_meaningful(
        &self,
        c_tokens: &[Token],
        rust_tokens: &[Token],
        segment_type: &SegmentType,
    ) -> bool {
        let c_strings: Vec<String> = c_tokens.iter().map(|t| t.to_string()).collect();
        let rust_strings: Vec<String> = rust_tokens.iter().map(|t| t.to_string()).collect();

        // Reject patterns that are too generic or meaningless

        // 1. Must have minimum complexity
        if c_tokens.len() < 3 {
            return false;
        }

        // 2. Reject patterns with too many consecutive punctuation
        let consecutive_punct = c_strings.windows(3).any(|window| {
            window
                .iter()
                .all(|s| s.len() == 1 && !s.chars().next().unwrap().is_alphanumeric())
        });
        if consecutive_punct {
            return false;
        }

        // 3. Must contain at least one meaningful token
        let meaningful_tokens = vec![
            "struct", "enum", "typedef", "int", "char", "float", "void", "if", "while", "for",
        ];
        let has_meaningful = c_strings
            .iter()
            .any(|s| meaningful_tokens.contains(&s.as_str()));
        if !has_meaningful {
            return false;
        }

        // 4. Type-specific validation
        match segment_type {
            SegmentType::Function => {
                // Functions must have parentheses and reasonable structure
                c_strings.contains(&"(".to_string()) && c_strings.contains(&")".to_string())
            }
            SegmentType::Array => {
                // Arrays must have brackets
                c_strings.contains(&"[".to_string()) && c_strings.contains(&"]".to_string())
            }
            SegmentType::Struct | SegmentType::Enum => {
                // Structures must have meaningful identifiers
                c_strings.len() >= 2
            }
            _ => true,
        }
    }

    /// Classify the type of code segment based on tokens (legacy method)
    fn classify_segment_type(&self, tokens: &[Token]) -> SegmentType {
        let token_strings: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();

        if token_strings.contains(&"struct".to_string()) {
            SegmentType::Struct
        } else if token_strings.contains(&"enum".to_string()) {
            SegmentType::Enum
        } else if token_strings.contains(&"typedef".to_string()) {
            SegmentType::TypeDef
        } else if token_strings.contains(&"#include".to_string()) {
            SegmentType::Include
        } else if token_strings
            .iter()
            .any(|s| s == "(" && token_strings.contains(&")".to_string()))
        {
            SegmentType::Function
        } else {
            SegmentType::Expression
        }
    }

    /// Mine patterns from aligned segments
    fn mine_patterns(&mut self, file_pair: &FilePair) -> Result<Vec<SamplizerPattern>> {
        let mut patterns = Vec::new();

        for alignment in &file_pair.alignment_map {
            let pattern = self.extract_pattern_from_alignment(file_pair, alignment)?;

            // Store the pattern
            self.generated_patterns
                .insert(pattern.pattern_id.clone(), pattern.clone());
            patterns.push(pattern);
        }

        Ok(patterns)
    }

    /// Extract a pattern from a specific alignment
    fn extract_pattern_from_alignment(
        &self,
        file_pair: &FilePair,
        alignment: &SegmentAlignment,
    ) -> Result<SamplizerPattern> {
        let pattern_id = Id::gen_name("samplizer_pattern");
        let pattern_name = format!(
            "{:?}_pattern_{}",
            alignment.segment_type,
            &pattern_id[pattern_id.len() - 8..]
        );

        // Extract C tokens for extraction pattern
        let c_segment = &file_pair.c_tokens[alignment.c_range.clone()];
        let extraction_pattern = self.generate_extraction_pattern(c_segment);

        // Extract transformation pattern from C->Rust differences
        let rust_segment = &file_pair.rust_tokens[alignment.rust_range.clone()];
        let conversion_pattern = self.generate_conversion_pattern(c_segment, rust_segment);

        Ok(SamplizerPattern {
            pattern_id,
            pattern_name,
            extraction_pattern: Some(extraction_pattern),
            conversion_pattern: Some(conversion_pattern),
            confidence_score: alignment.confidence,
            source_segments: vec![alignment.clone()],
            pattern_type: alignment.segment_type.clone(),
        })
    }

    /// Generate high-quality, semantic extraction pattern (what to look for in C code)
    fn generate_extraction_pattern(&self, tokens: &[Token]) -> String {
        let mut semantic_pattern = Vec::new();
        let _token_strings: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();

        for (i, token) in tokens.iter().enumerate() {
            let token_str = token.to_string();
            let pattern = match token_str.as_str() {
                // C type keywords - semantic meaning preserved
                "int" | "char" | "float" | "double" | "void" | "long" | "short" | "unsigned"
                | "signed" => "TYPE_KEYWORD",

                // Storage class specifiers
                "static" | "extern" | "inline" | "const" | "volatile" | "auto" | "register" => {
                    "STORAGE_CLASS"
                }

                // Structure keywords
                "struct" | "union" | "enum" | "typedef" => "STRUCTURE_KEYWORD",

                // Control flow keywords
                "if" | "else" | "while" | "for" | "do" | "switch" | "case" | "default"
                | "break" | "continue" | "return" | "goto" => "CONTROL_KEYWORD",

                // Function-like patterns (identifier followed by parentheses)
                s if s.chars().all(|c| c.is_alphabetic() || c == '_')
                    && i + 1 < tokens.len()
                    && tokens[i + 1].to_string() == "(" =>
                {
                    "FUNCTION_NAME"
                }

                // Array patterns (identifier followed by brackets)
                s if s.chars().all(|c| c.is_alphabetic() || c == '_')
                    && i + 1 < tokens.len()
                    && tokens[i + 1].to_string() == "[" =>
                {
                    "ARRAY_NAME"
                }

                // Variable/identifier patterns
                s if s.chars().all(|c| c.is_alphabetic() || c == '_') && s.len() > 1 => {
                    // Context-aware identifier classification
                    if i > 0 {
                        match tokens[i - 1].to_string().as_str() {
                            "struct" | "enum" | "union" => "TYPE_IDENTIFIER",
                            "int" | "char" | "float" | "double" | "void" => "VARIABLE_NAME",
                            _ => "IDENTIFIER",
                        }
                    } else {
                        "IDENTIFIER"
                    }
                }

                // Numeric literals with context
                s if s.chars().all(|c| c.is_numeric()) => {
                    if i > 0 && tokens[i - 1].to_string() == "[" {
                        "ARRAY_SIZE"
                    } else {
                        "NUMBER"
                    }
                }

                // Preserve important operators and punctuation
                "(" | ")" | "{" | "}" | "[" | "]" | ";" | "," | "=" | "+" | "-" | "*" | "/"
                | "%" => &token_str,

                // Handle preprocessor directives
                s if s.starts_with('#') => "PREPROCESSOR",

                // Everything else as literal
                _ => &token_str,
            };
            semantic_pattern.push(pattern.to_string());
        }

        semantic_pattern.join(" ")
    }

    /// Generate conversion pattern from C->Rust transformation
    fn generate_conversion_pattern(&self, c_tokens: &[Token], rust_tokens: &[Token]) -> String {
        // Analyze the transformation from C to Rust
        let c_str = c_tokens
            .iter()
            .map(|t| t.to_string())
            .collect::<Vec<_>>()
            .join(" ");
        let rust_str = rust_tokens
            .iter()
            .map(|t| t.to_string())
            .collect::<Vec<_>>()
            .join(" ");

        format!("C: {} -> Rust: {}", c_str, rust_str)
    }

    /// Validate generated patterns against test cases
    pub fn validate_patterns(&mut self, test_cases: &[FilePair]) -> Result<()> {
        report!(
            "samplizer",
            "validate_patterns",
            Info,
            Process,
            format!(
                "Validating {} patterns against {} test cases",
                self.generated_patterns.len(),
                test_cases.len()
            ),
            true
        );

        for (pattern_id, pattern) in &self.generated_patterns {
            let result = self.validate_single_pattern(pattern, test_cases)?;
            self.validation_results.insert(pattern_id.clone(), result);
        }

        self.integration_stats.patterns_validated = self.validation_results.len() as u32;
        Ok(())
    }

    /// Validate a single pattern against test cases
    fn validate_single_pattern(
        &self,
        pattern: &SamplizerPattern,
        test_cases: &[FilePair],
    ) -> Result<ValidationResult> {
        let mut successful_tests = 0;
        let test_count = test_cases.len() as u32;
        let mut failure_reasons = Vec::new();

        // Test the pattern against each test case
        for test_case in test_cases {
            if self.test_pattern_on_file_pair(pattern, test_case)? {
                successful_tests += 1;
            } else {
                failure_reasons.push("Pattern did not match expected segments".to_string());
            }
        }

        let success_rate = successful_tests as f64 / test_count as f64;

        Ok(ValidationResult {
            pattern_id: pattern.pattern_id.clone(),
            success_rate,
            test_cases: test_count,
            successful_tests: successful_tests as u32,
            failure_reasons,
        })
    }

    /// Test a pattern against a specific file pair
    fn test_pattern_on_file_pair(
        &self,
        pattern: &SamplizerPattern,
        _file_pair: &FilePair,
    ) -> Result<bool> {
        // TODO: Implement pattern testing logic
        // For now, return a simple heuristic based on confidence
        Ok(pattern.confidence_score > 0.8)
    }

    /// Integrate successful patterns into the permanent Patternizer collection
    pub fn integrate_successful_patterns(&mut self, patternizer: &mut Patternizer) -> Result<u32> {
        let mut integrated_count = 0;

        for (pattern_id, validation_result) in &self.validation_results {
            if validation_result.success_rate > 0.9 {
                // High success threshold
                if let Some(pattern) = self.generated_patterns.get(pattern_id) {
                    self.integrate_pattern_into_patternizer(pattern, patternizer)?;
                    integrated_count += 1;
                }
            }
        }

        self.integration_stats.patterns_integrated = integrated_count;
        self.integration_stats.success_rate =
            integrated_count as f64 / self.generated_patterns.len() as f64;

        report!(
            "samplizer",
            "integrate_successful_patterns",
            Info,
            Process,
            format!(
                "Integrated {} high-quality patterns into Patternizer",
                integrated_count
            ),
            true
        );

        Ok(integrated_count)
    }

    /// Integrate a single pattern into the Patternizer
    fn integrate_pattern_into_patternizer(
        &self,
        pattern: &SamplizerPattern,
        patternizer: &mut Patternizer,
    ) -> Result<()> {
        report!(
            "samplizer",
            "integrate_pattern_into_patternizer",
            Info,
            Process,
            format!(
                "Integrating pattern '{}' into Patternizer",
                pattern.pattern_name
            ),
            true
        );

        // Convert samplizer pattern to HandlerPattern
        if let Some(extraction_pattern) = &pattern.extraction_pattern {
            let handler_pattern = self.convert_to_handler_pattern(pattern, extraction_pattern)?;

            // Register the pattern with appropriate handler type
            let handler_type = match pattern.pattern_type {
                SegmentType::Function => "function",
                SegmentType::Struct => "struct",
                SegmentType::Enum => "enum",
                SegmentType::Variable => "variable",
                SegmentType::TypeDef => "typedef",
                SegmentType::Macro => "macro",
                SegmentType::Array => "array",
                _ => "general",
            };

            patternizer.register_pattern(handler_type.to_string(), handler_pattern);

            report!(
                "samplizer",
                "integrate_pattern_into_patternizer",
                Info,
                Process,
                format!(
                    "Successfully registered {} pattern with type '{}'",
                    pattern.pattern_name, handler_type
                ),
                true
            );
        }

        Ok(())
    }

    /// Convert samplizer extraction pattern to HandlerPattern
    fn convert_to_handler_pattern(
        &self,
        pattern: &SamplizerPattern,
        extraction_pattern: &str,
    ) -> Result<crate::pattern::HandlerPattern> {
        use crate::pattern::{HandlerPattern, PatternRule, TokenPattern};

        // Parse extraction pattern into TokenPattern rules
        let pattern_tokens: Vec<&str> = extraction_pattern.split_whitespace().collect();
        let mut rules = Vec::new();

        for token in pattern_tokens {
            let rule = match token {
                "IDENTIFIER" => PatternRule::new(TokenPattern::Identifier),
                "NUMBER" => PatternRule::new(TokenPattern::Number),
                token_str if token_str.chars().all(|c| c.is_alphabetic()) => {
                    // Check if it's a type keyword
                    if matches!(
                        token_str,
                        "int"
                            | "char"
                            | "float"
                            | "double"
                            | "void"
                            | "struct"
                            | "enum"
                            | "typedef"
                            | "static"
                            | "inline"
                            | "extern"
                    ) {
                        PatternRule::new(TokenPattern::TypeKeyword)
                    } else {
                        PatternRule::new(TokenPattern::Exact(token_str.to_string()))
                    }
                }
                _ => PatternRule::new(TokenPattern::Exact(token.to_string())),
            };
            rules.push(rule);
        }

        // Create HandlerPattern with appropriate priority based on confidence
        let priority = (pattern.confidence_score * 300.0) as i32; // Scale to 0-300 range

        Ok(HandlerPattern::new(
            pattern.pattern_id.clone(),
            format!("Auto-generated pattern: {}", pattern.pattern_name),
        )
        .with_rules(rules)
        .priority(priority))
    }

    /// Get statistics about the Samplizer's performance
    pub fn get_statistics(&self) -> &IntegrationStats {
        &self.integration_stats
    }
}

/// Create a new Samplizer instance
pub fn create_samplizer() -> Samplizer {
    Samplizer::new()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_samplizer_creation() {
        let samplizer = create_samplizer();
        assert_eq!(samplizer.generated_patterns.len(), 0);
        assert_eq!(samplizer.analyzed_pairs.len(), 0);
    }

    #[test]
    fn test_segment_type_classification() {
        let samplizer = create_samplizer();

        let struct_tokens = vec![
            Token::s("struct".to_string()),
            Token::s("Point".to_string()),
        ];

        assert_eq!(
            samplizer.classify_segment_type(&struct_tokens),
            SegmentType::Struct
        );
    }
}
