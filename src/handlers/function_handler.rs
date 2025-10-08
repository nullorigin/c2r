//! Function handler implementation using trait-based system
//! Uses Patternizer and Samplizer for intelligent pattern detection and processing

use crate::common::{IdentifierCase};
use crate::{Handlizer, PatternResult, RedirectRequest, context, match_patterns_with_context, report};
use crate::{
    Context, ConvertedFunction, ConvertedElement, ElementInfo, ExtractedFunction, ExtractedElement, HandlerPhase, HandlerReport, HandlerResult, Id, ReportLevel, Result, Handler
};
use crate::handlers::common::convert_identifier_to_rust;
use crate::info::FunctionInfo;
use std::collections::HashMap;
use std::ops::Range;

/// Pattern matching result for function analysis
#[derive(Debug, Clone)]
struct PatternMatch {
    pattern_name: String,
    captures: HashMap<String, String>,
    confidence: f64,
    range: Range<usize>,
}

/// Function handler struct implementing the Handler trait with constant functions
#[derive(Debug)]
pub struct FunctionHandler;

impl FunctionHandler {
    pub fn new() -> Self {
        Self
    }
    
    /// Enhanced pattern detection using both Patternizer and Samplizer
    fn detect_function_pattern(&self, patternizer: &mut crate::Patternizer, samplizer: &crate::Samplizer, tokens: &[crate::Token], token_range: Range<usize>) -> Result<(bool, f64)> {
        // First use Patternizer for structural detection
        let pattern_match = patternizer.match_pattern("function", tokens);

        // Then use Samplizer for confidence scoring
        let patterns = vec!["function_declaration".to_string(), "function_definition".to_string()];
        let confidence = samplizer.analyze_with_range(&patterns, tokens, token_range)?;

        // Combined heuristics for function detection
        let has_function_structure = self.has_function_structure(tokens);
        let enhanced_confidence = if has_function_structure {
            confidence * 1.3  // Boost confidence if structure looks function-like
        } else {
            confidence * 0.7  // Reduce if structure doesn't match
        };

        let pattern_success = !matches!(pattern_match, crate::PatternResult::NoMatch { .. });

        Ok((pattern_success || enhanced_confidence > 0.7, enhanced_confidence))
    }
    
    /// Analyze token structure for function-like patterns - IMPROVED for accuracy
    fn has_function_structure(&self, tokens: &[crate::Token]) -> bool {
        if tokens.len() < 4 {  // Need at least: type name ( )
            return false;
        }
        
        let token_strings: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();
        
        // DEBUG: Print tokens being analyzed (remove this later)
        if tokens.len() <= 30 {  // Only print for reasonable token sets
            println!("🔍 Function Handler analyzing {} tokens: {:?}", tokens.len(), token_strings);
        }
        
        // STEP 1: Only reject tokens that are clearly incompatible with function definitions
        // Note: Don't reject "return", "if", etc. as they can appear INSIDE functions
        for token_str in &token_strings {
            if matches!(token_str.as_str(), 
                "#include" | "#define" | "#ifdef" | "#ifndef" | "#endif" |  // preprocessor
                "struct" | "enum" | "union" | "typedef"  // other type definitions
            ) {
                return false;
            }
        }
        
        // STEP 2: Look for ANY function signature pattern in the token sequence
        // (handles multiple functions in one sequence)
        let mut found_any_function = false;
        let mut i = 0;
        
        while i < token_strings.len() - 4 {  // Need at least 4 tokens ahead
            // Look for return type
            if matches!(token_strings[i].as_str(), 
                "int" | "long" | "short" | "char" | "float" | "double" | "void" |
                "unsigned" | "signed" | "const" | "static" | "extern" | "inline"
            ) {
                // Look for function name next
                if i + 1 < token_strings.len() 
                    && token_strings[i + 1].chars().all(|c| c.is_alphanumeric() || c == '_')
                    && !token_strings[i + 1].chars().next().unwrap_or('0').is_numeric()
                    && token_strings[i + 1].len() > 0 {
                    
                    // Look for opening parenthesis
                    if i + 2 < token_strings.len() && token_strings[i + 2] == "(" {
                        // Found potential function pattern: type name (
                        found_any_function = true;
                        break;
                    }
                }
            }
            i += 1;
        }
        
        println!("🔍 Function structure analysis: found_any_function = {}", found_any_function);
        found_any_function
    }
    
    /// Analyze tokens using registered patterns with enhanced capture system
    fn analyze_with_patterns(&self, context: &mut Context, tokens: &[crate::Token]) -> Result<Vec<PatternMatch>> {
        let mut pattern_matches = Vec::new();
        
        // Use each pattern from the base handler with the new capture system
        for pattern_name in &self.supported_patterns() {
            match context.samplizer.analyze_with_captures(tokens, pattern_name) {
                Ok(analysis_result) => {
                    if analysis_result.success {
                        pattern_matches.push(PatternMatch {
                            pattern_name: pattern_name.to_string(),
                            range: 0..analysis_result.captures.pattern_metadata.tokens_consumed,
                            captures: analysis_result.captures.to_hashmap(), // Convert to HashMap for compatibility
                            confidence: analysis_result.confidence,
                        });
                    }
                }
                Err(_) => {
                    // Fallback to basic pattern matching if capture system fails
                    let result = context.patternizer.match_pattern(pattern_name, tokens);
                    if let PatternResult::Match { consumed_tokens } = result {
                        pattern_matches.push(PatternMatch {
                            pattern_name: pattern_name.to_string(),
                            range: 0..consumed_tokens,
                            captures: HashMap::new(),
                            confidence: 0.8,
                        });
                    }
                }
            }
        }
        
        Ok(pattern_matches)
    }
        
        /// Extract function info using pattern matching results
        fn extract_function_info_with_patterns(&self, tokens: &[crate::Token], pattern_results: &[PatternMatch]) -> Result<FunctionInfo> {
            let mut function_info = FunctionInfo {
                name: String::new(),
                return_type: "void".to_string(),
                parameters: Vec::new(),
                is_static: false,
                is_inline: false,
                has_body: false,
                complexity: "low".to_string(), // Default complexity
                is_declaration: false, // Will be determined by pattern analysis
                is_definition: false,  // Will be determined by pattern analysis
                is_extern: false,      // Check for extern keyword
                is_unsafe: false,      // Check for unsafe patterns
                is_variadic: false,    // Check for ... in parameters
                parameter_count: 0,    // Will be set from parameters.len()
                storage_class: None,   // Will be set if static/extern found
            };
            
            // Use pattern results to guide token analysis
            for pattern_match in pattern_results {
                match pattern_match.pattern_name.as_str() {
                    "function_declaration" | "function_definition" => {
                        // Since we don't have a full capture system yet, 
                        // extract info from the matched token range
                        let matched_tokens = &tokens[pattern_match.range.clone()];
                        
                        // Determine if it's declaration vs definition
                        function_info.is_declaration = pattern_match.pattern_name == "function_declaration";
                        function_info.is_definition = pattern_match.pattern_name == "function_definition";
                        
                        // Check for function body (presence of '{' indicates definition)
                        function_info.has_body = matched_tokens.iter()
                            .any(|t| t.to_string() == "{");
                            
                        // Update definition/declaration flags based on body
                        if function_info.has_body {
                            function_info.is_definition = true;
                            function_info.is_declaration = false;
                        }
                    }
                    _ => {} // Other patterns
                }
            }
            
            // Fallback to basic extraction if patterns didn't provide enough info
            if function_info.name.is_empty() {
                function_info = self.extract_function_info(tokens)?;
            }
            
            Ok(function_info)
        }
        
        /// Convert to Rust using pattern matching results
        fn convert_to_rust_with_patterns(&self, function_info: &FunctionInfo, tokens: &[crate::Token], pattern_results: &[PatternMatch]) -> Result<String> {
            let mut rust_code = String::new();
            
            // Add documentation with pattern confidence
            let avg_confidence: f64 = pattern_results.iter()
                .map(|p| p.confidence)
                .sum::<f64>() / pattern_results.len() as f64;
                
            rust_code.push_str(&format!("/// Converted from C function: {} (pattern confidence: {:.2})\n", 
                                       function_info.name, avg_confidence));
            
            // Use the enhanced conversion logic
            let base_conversion = self.convert_to_rust(function_info, tokens)?;
            rust_code.push_str(&base_conversion);
            
            Ok(rust_code)
        }
    
    /// Extract function information from tokens
    fn extract_function_info(&self, tokens: &[crate::Token]) -> Result<FunctionInfo> {
        let mut function_info = FunctionInfo::default();
        
        // Parse function signature
        let mut in_params = false;
        let mut brace_count = 0;
        
        for (i, token) in tokens.iter().enumerate() {
            let token_str = token.to_string();
            
            match token_str.as_str() {
                "static" | "inline" | "extern" => {
                    function_info.storage_class = Some(token_str);
                }
                "(" => {
                    in_params = true;
                }
                ")" => {
                    in_params = false;
                }
                "{" => {
                    brace_count += 1;
                    if brace_count == 1 {
                        function_info.has_body = true;
                    }
                }
                "}" => {
                    brace_count -= 1;
                }
                _ => {
                    if !in_params && function_info.name.is_empty() && i > 0 && tokens[i-1].to_string() != "(" {
                        // This might be the function name
                        if token_str.chars().all(|c| c.is_alphanumeric() || c == '_') {
                            function_info.name = token_str;
                        }
                    }
                }
            }
        }
        
        Ok(function_info)
    }
    
    /// Convert function to Rust code
    fn convert_to_rust(&self, function_info: &FunctionInfo, tokens: &[crate::Token]) -> Result<String> {
        let mut rust_code = String::new();
        
        // Add documentation comment
        rust_code.push_str(&format!("/// Converted from C function: {}\n", function_info.name));
        
        // Add function visibility
        if function_info.is_static {
            // Static functions become private
        } else {
            rust_code.push_str("pub ");
        }
        
        // Add unsafe keyword if function uses pointers or unsafe operations
        if self.function_needs_unsafe(tokens) {
            rust_code.push_str("unsafe ");
        }
        
        // Add function keyword
        rust_code.push_str("fn ");
        
        // Add function name (convert to snake_case)
        let rust_name = convert_identifier_to_rust(&function_info.name, IdentifierCase::SnakeCase);
        rust_code.push_str(&rust_name);
        
        // Process parameters
        rust_code.push('(');
        let params = self.convert_parameters(&function_info.parameters, tokens)?;
        rust_code.push_str(&params);
        rust_code.push(')');
        
        // Add return type
        let return_type = self.convert_return_type(&function_info.return_type)?;
        if return_type != "()" {
            rust_code.push_str(&format!(" -> {}", return_type));
        }
        
        // Add function body or semicolon
        if function_info.has_body {
            rust_code.push_str(" {\n");
            rust_code.push_str(&self.convert_function_body(tokens)?);
            rust_code.push_str("\n}");
        } else {
            rust_code.push(';');
        }
        
        Ok(rust_code)
    }
    
    /// Check if function needs unsafe keyword
    fn function_needs_unsafe(&self, tokens: &[crate::Token]) -> bool {
        // Check for pointer types, malloc, free, etc.
        tokens.iter().any(|token| {
            matches!(token.to_string().as_str(), 
                "*" | "malloc" | "free" | "memcpy" | "memset" | "sizeof"
            )
        })
    }
    
    /// Convert C parameters to Rust parameters
    fn convert_parameters(&self, params: &[String], _tokens: &[crate::Token]) -> Result<String> {
        if params.is_empty() {
            return Ok(String::new());
        }
        
        let rust_params: Vec<String> = params.iter().map(|param| {
            self.convert_single_parameter(param)
        }).collect();
        
        Ok(rust_params.join(", "))
    }
    
    /// Convert a single C parameter to Rust
    fn convert_single_parameter(&self, param: &str) -> String {
        // Parse "type name" format
        let parts: Vec<&str> = param.trim().split_whitespace().collect();
        if parts.len() < 2 {
            return format!("/* invalid param: {} */", param);
        }
        
        let c_type = parts[..parts.len()-1].join(" ");
        let param_name = parts[parts.len()-1];
        
        // Convert parameter name to snake_case
        let rust_name = convert_identifier_to_rust(param_name, IdentifierCase::SnakeCase);
        
        // Convert C type to Rust type
        let rust_type = self.convert_c_type_to_rust_type(&c_type);
        
        format!("{}: {}", rust_name, rust_type)
    }
    
    /// Convert C type to Rust type
    fn convert_c_type_to_rust_type(&self, c_type: &str) -> String {
        match c_type.trim() {
            "int" => "i32".to_string(),
            "long" => "i64".to_string(),
            "short" => "i16".to_string(),
            "char" => "i8".to_string(),
            "unsigned int" | "uint" => "u32".to_string(),
            "unsigned long" => "u64".to_string(),
            "unsigned short" => "u16".to_string(),
            "unsigned char" | "uchar" => "u8".to_string(),
            "float" => "f32".to_string(),
            "double" => "f64".to_string(),
            "void" => "()".to_string(),
            "char*" | "char *" => "*const std::ffi::c_char".to_string(),
            "const char*" | "const char *" => "*const std::ffi::c_char".to_string(),
            "void*" | "void *" => "*mut std::ffi::c_void".to_string(),
            _ if c_type.ends_with('*') => {
                let base_type = c_type.trim_end_matches('*').trim();
                let rust_base = self.convert_c_type_to_rust_type(base_type);
                format!("*mut {}", rust_base)
            },
            _ => format!("/* unknown type: {} */", c_type),
        }
    }
    
    /// Convert return type
    fn convert_return_type(&self, return_type: &str) -> Result<String> {
        Ok(self.convert_c_type_to_rust_type(return_type))
    }
    
    /// Convert function body (basic implementation)
    fn convert_function_body(&self, tokens: &[crate::Token]) -> Result<String> {
        // For now, provide a basic conversion framework
        let mut body = String::new();
        body.push_str("    // Function body conversion\n");
        body.push_str("    // TODO: Implement statement-by-statement conversion\n");
        
        // Add basic return placeholder for now
        body.push_str("    // TODO: Convert function body statements\n");
        body.push_str("    // Placeholder return - needs intelligent conversion\n");
        body.push_str("    return Default::default();");
        
        Ok(body)
    }
    
    // Removed duplicate function - now using centralized convert_identifier_to_rust from common.rs
}

impl Handler for FunctionHandler {
    /// Constant function approach - compile-time constants
    fn id(&self) -> Id { 
        Id::get("function_handler")
    }
    
    fn role(&self) -> String { 
        "function".to_string() 
    }
    
    fn priority(&self) -> u64 { 
        200  // High priority for function detection
    }
    
    fn supported_patterns(&self) -> Vec<String> {
        vec!["function_declaration".to_string(), "function_definition".to_string(), "function_call".to_string()]
    }
    
    fn can_process(&self, context: &mut Context, token_slot: usize, token_range: Range<usize>) -> Result<bool> {
        // Get tokens from the specified slot and range using Context::tokenizer
        let tokens = context.tokenizer.slots()[token_slot].tokens()[token_range.clone()].to_vec();
        
        if tokens.is_empty() {
            return Ok(false);
        }
        let tokens = tokens;
        let (can_process, confidence) = self.detect_function_pattern(&mut context.patternizer, &context.samplizer, &tokens, token_range)?;

        // Add report using Context::registry
        context.registry.add_report(HandlerReport::new(
                &format!("function_handler_can_process_{}", token_slot),
                std::sync::Arc::new(self.id()),
                "Function Handler (Trait)".to_string(),
                "can_process".to_string(),
                format!("Function detection: {} (confidence: {:.2})", can_process, confidence),
                ReportLevel::Info,
                HandlerPhase::Process,
            ).with_tokens(tokens.len(), if can_process { tokens.len() } else { 0 })
             .with_success(can_process));
            
        Ok(can_process)
    }
    
    fn process(&self, context: &mut Context,     token_slot: usize, token_range: Range<usize>) -> Result<HandlerResult> {
        // Get tokens from the specified slot and range using Context::with_tokenizer
        let tokens = context.tokenizer.slots()[token_slot].tokens()[token_range.clone()].to_vec();
        
        if tokens.is_empty() {
            return Ok(HandlerResult::NotHandled(
                None,
                token_range,
                self.id(),
            ));
        }
        let tokens = tokens;
        // **STEP 1: Use Pattern Matching to Detect Function Structure**
        let (pattern_match, confidence) = self.detect_function_pattern(&mut context.patternizer, &context.samplizer, &tokens, token_range.clone())?;
        
        if !pattern_match || confidence < 0.7 {
            return Ok(HandlerResult::NotHandled(
                Some(tokens.to_vec()),
                token_range,
                self.id(),
            ));
        }
        
        // **STEP 2: Use Pattern Results to Guide Extraction**
        let pattern_results = self.analyze_with_patterns(context, &tokens)?;
        let function_info = self.extract_function_info_with_patterns(&tokens, &pattern_results)?;
        
        // **STEP 3: Pattern-Driven Code Generation**
        let rust_code = self.convert_to_rust_with_patterns(&function_info, &tokens, &pattern_results)?;
        
        // Add processing report with pattern details
        context.registry.add_report(HandlerReport::new(
                &format!("function_handler_process_{}", token_slot),
                std::sync::Arc::new(self.id()),
                "Function Handler (Trait)".to_string(),
                "process".to_string(),
                format!("Converted function '{}' using patterns (confidence: {:.2})", 
                       function_info.name, confidence),
                ReportLevel::Info,
                HandlerPhase::Process,
            ).with_tokens(tokens.len(), tokens.len())
             .with_success(true));
        
        Ok(HandlerResult::Processed(
            Some(tokens.to_vec()),
            token_range,
            rust_code,
            self.id(),
        ))
    }

    fn extract(&self, context: &mut Context, token_slot: usize, token_range: Range<usize>) -> Result<Option<ExtractedElement>> {
    let tokens = context.tokenizer.get_tokens(token_slot, token_range.clone());
    
    if tokens.is_empty() {
        return Ok(None);
    }
    
    let function_info = self.extract_function_info(&tokens)?;
    let rust_code = self.convert_to_rust(&function_info, &tokens)?;
    
    let extracted_function = ExtractedFunction {
        id: Id::get(Id::gen_name(&self.id().name()).as_str()),
        code: rust_code,
        info: function_info.clone(),
        tokens: tokens.clone(),
        token_range,
        metadata: vec![
            ("is_static".to_string(), function_info.is_static.to_string()),
            ("is_inline".to_string(), function_info.is_inline.to_string()),
            ("is_extern".to_string(), function_info.is_extern.to_string()),
            ("has_body".to_string(), function_info.has_body.to_string()),
            ("parameter_count".to_string(), function_info.parameter_count.to_string()),
            ("is_variadic".to_string(), function_info.is_variadic.to_string()),
        ],
    };
    
    Ok(Some(ExtractedElement::Function(extracted_function)))
}
    
    fn convert(&self, context: &mut Context, token_slot: usize, token_range: Range<usize>) -> Result<Option<ConvertedElement>> {
        let tokens = context.tokenizer.slots()[token_slot].tokens()[token_range.clone()].to_vec();
        if tokens.is_empty() {
            return Ok(None);
        }
        let tokens = tokens;
        let function_info = self.extract_function_info(&tokens)?;
        let rust_code = self.convert_to_rust(&function_info, &tokens)?;
        
        let converted_function = ConvertedFunction {
            name: function_info.name.clone(),
            return_type: function_info.return_type.clone(),
            parameters: function_info.parameters.clone(),
            body: rust_code.clone(),
            code: rust_code.clone(),
            is_unsafe: false, // TODO: Extract from function_info
            is_public: false,
            is_definition: true,
        };
        
        Ok(Some(ConvertedElement::Function(converted_function)))
    }
    
    fn document(&self, context: &mut Context, info: ElementInfo) -> Result<Option<String>> {
        if let ElementInfo::Function(function_info) = info {
            let doc = format!(
                "/// Function: {}\n/// Converted from C function definition\n",
                function_info.name
            );
            Ok(Some(doc))
        } else {
            Ok(None)
        }
    }
    
    fn report(&self, context: &mut Context) -> Result<HandlerReport> {
        let reports = context.registry.get_reports_by_handler("function_handler_trait");
        if reports.is_empty() {
            return Ok(HandlerReport::new(
                "function_handler_report",
                std::sync::Arc::new(self.id()),
                "Function Handler (Trait)".to_string(),
                "report".to_string(),
                "No reports found".to_string(),
                ReportLevel::Info,
                HandlerPhase::Report,
            ).with_success(true)
             .with_metadata("total_reports".to_string(), "0".to_string()));
        }
        let reports = reports;
        let (info_count, warning_count, error_count) = reports
            .iter()
            .fold((0, 0, 0), |(info, warn, err), report| {
                match report.level {
                    ReportLevel::Info => (info + 1, warn, err),
                    ReportLevel::Warning => (info, warn + 1, err),
                    ReportLevel::Error => (info, warn, err + 1),
                    ReportLevel::Debug => (info + 1, warn, err),
                }
            });
        
        Ok(HandlerReport::new(
            "function_handler_report",
            std::sync::Arc::new(self.id()),
            "Function Handler (Trait)".to_string(),
            "report".to_string(),
            format!(
                "Function handler processed {} info, {} warnings, {} errors",
                info_count, warning_count, error_count
            ),
            ReportLevel::Info,
            HandlerPhase::Report,
        ).with_success(error_count == 0)
         .with_metadata("total_reports".to_string(), reports.len().to_string()))
    }
    
    fn handle_redirect(&self, context: &mut Context, token_slot: usize, token_range: Range<usize>, result: HandlerResult) -> Result<HandlerResult> {
        // INTELLIGENT REDIRECT: Function handler knows what's most likely to work next
        let tokens = context.tokenizer.slots()[token_slot].tokens()[token_range.clone()].to_vec();
        
        if tokens.is_empty() {
            return Ok(result);
        }
        let tokens = tokens;
        // Analyze tokens to suggest the best next handler
        let suggested_handler = self.suggest_next_handler(context, &tokens);
        
        let mut metadata = Vec::new();
        metadata.push(("analysis_reason".to_string(), format!("Function handler analyzed {} tokens", tokens.len())));
        
        let redirect_request = RedirectRequest {
            from_handler: self.id(),
            token_range: token_range.clone(),
            failed_patterns: self.supported_patterns(),
            suggested_handler,
            metadata,
        };
        
        // Return the result as-is to avoid deadlock in redirect workflow
        Ok(result)
    }
}

impl FunctionHandler {
    /// Intelligently suggest which handler should try next based on token analysis
    /// ENHANCED with hierarchical subhandler routing - PRIVATE METHOD
    fn suggest_next_handler(&self, context: &mut Context, tokens: &[crate::Token]) -> Option<Id> {
        let token_strings: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();
        
        println!("🧠 FunctionHandler analyzing tokens for intelligent redirect: {:?}", 
                 token_strings.iter().take(5).collect::<Vec<_>>());
        
        // PRIORITY 1: Function-internal constructs (higher accuracy subhandlers)
        for (i, token_str) in token_strings.iter().enumerate() {
            match token_str.as_str() {
                // Loop constructs - route to specialized LoopHandler
                "for" | "while" | "do" if i + 1 < token_strings.len() => {
                    println!("🎯 FunctionHandler → LoopHandler (detected: {})", token_str);
                    return Some(Id::get("loop_handler"));
                }
                
                // Variable declarations/assignments - route to VariableHandler  
                "int" | "long" | "short" | "char" | "float" | "double" | "void" | "unsigned" | "signed" 
                if i + 1 < token_strings.len() => {
                    // Check if this looks like a variable declaration
                    let next_token = &token_strings[i + 1];
                    if next_token.chars().all(|c| c.is_alphanumeric() || c == '_' || c == '*') {
                        println!("🎯 FunctionHandler → VariableHandler (detected: {} {})", token_str, next_token);
                        return Some(Id::get("variable_handler"));
                    }
                }
                
                // Return statements - specialized statement handler
                "return" => {
                    println!("🎯 FunctionHandler → StatementHandler (detected: return)");
                    return Some(Id::get("statement_handler"));
                }
                
                // Function calls - expression handler
                _ if i + 1 < token_strings.len() && token_strings[i + 1] == "(" => {
                    // This looks like a function call
                    if token_str.chars().all(|c| c.is_alphanumeric() || c == '_') {
                        println!("🎯 FunctionHandler → ExpressionHandler (detected function call: {})", token_str);
                        return Some(Id::get("expression_handler"));
                    }
                }
                _ => {}
            }
        }
        
        // PRIORITY 2: Assignment operations - route to VariableHandler
        if token_strings.iter().any(|t| matches!(t.as_str(), "=" | "+=" | "-=" | "*=" | "/=")) {
            println!("🎯 FunctionHandler → VariableHandler (detected assignment)");
            return Some(Id::get("variable_handler"));
        }
        
        // PRIORITY 3: Mathematical expressions - route to ExpressionHandler
        if token_strings.iter().any(|t| matches!(t.as_str(), "+" | "-" | "*" | "/" | "%" | "&&" | "||" | "==" | "!=" | "<" | ">")) {
            println!("🎯 FunctionHandler → ExpressionHandler (detected operators)");
            return Some(Id::get("expression_handler"));
        }
        
        // PRIORITY 4: Other top-level constructs
        for token_str in &token_strings {
            match token_str.as_str() {
                "struct" | "typedef" => {
                    println!("🎯 FunctionHandler → StructHandler (detected: {})", token_str);
                    return Some(Id::get("struct_handler"));
                }
                "enum" => {
                    println!("🎯 FunctionHandler → EnumHandler (detected: enum)");
                    return Some(Id::get("enum_handler"));
                }
                "#include" | "#define" | "#ifdef" => {
                    println!("🎯 FunctionHandler → MacroHandler (detected: {})", token_str);
                    return Some(Id::get("macro_handler"));
                }
                "//" | "/*" => {
                    println!("🎯 FunctionHandler → CommentHandler (detected: {})", token_str);
                    return Some(Id::get("comment_handler"));
                }
                "[" | "]" => {
                    println!("🎯 FunctionHandler → ArrayHandler (detected: array)");
                    return Some(Id::get("array_handler"));
                }
                _ => {}
            }
        }
        
        // PRIORITY 5: Default fallback
        println!("🎯 FunctionHandler → GlobalHandler (fallback)");
        Some(Id::get("global_handler"))
    }
}

impl Default for FunctionHandler {
    fn default() -> Self {
        Self::new()
    }
}

/// Factory function to create a function handler
pub fn create_function_handler() -> FunctionHandler {
    FunctionHandler::new()
}
