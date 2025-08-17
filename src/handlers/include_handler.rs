use crate::config::Config;
use crate::error::ConversionError;
use crate::file_utils::FileUtils;
use crate::handler::{HandlerResult, ParserContext, TokenHandler};
use crate::log;
use crate::token_parser::Token;
use std::path::Path;

/// Handler for preprocessing C #include directives
/// Example: #include "file.h"  -> use file;
/// Converts C include directives into Rust use statements
pub struct IncludeHandler {
    config: Config,
}

impl IncludeHandler {
    /// Create a new include handler with the provided configuration
    pub fn new(config: Config) -> Self {
        IncludeHandler { config }
    }

    /// Create a new include handler with default settings
    pub fn default() -> Self {
        IncludeHandler {
            config: Config::new(),
        }
    }

    /// Get the effective configuration, checking context first, then falling back to the instance config
    fn get_effective_config(&self, context: &ParserContext) -> Config {
        // Try to get config from context first
        if let Some(config) = context.get_value::<Config>("config") {
            log!(debug, "Using Config from ParserContext");
            config.clone()
        } else {
            // Fall back to our own config
            log!(debug, "Using Config from IncludeHandler instance");
            self.config.clone()
        }
    }

    /// Process an include directive
    fn process_include(
        &self,
        tokens: &[Token],
        context: &ParserContext,
    ) -> Result<Option<String>, ConversionError> {
        if tokens.len() < 3 || tokens[0] != Token::Hash {
            return Ok(None);
        }

        // Check for #include directive
        if let Token::Identifier(ref directive) = tokens[1] {
            if directive != "include" {
                return Ok(None);
            }

            // Get effective configuration for this processing
            let config = self.get_effective_config(context);

            // Handle local include "file.h"
            if let Token::StringLiteral(ref filename) = tokens[2] {
                return self.process_local_include(filename, &config);
            }

            // Handle system include <file.h>
            if tokens[2] == Token::LessThan {
                // Find the closing '>'
                let mut i = 3;
                let mut system_include = String::new();

                while i < tokens.len() && tokens[i] != Token::GreaterThan {
                    match &tokens[i] {
                        Token::Identifier(id) => system_include.push_str(id),
                        Token::Dot => system_include.push('.'),
                        Token::Slash => system_include.push('/'),
                        Token::Underscore => system_include.push('_'),
                        _ => {}
                    }
                    i += 1;
                }

                return self.process_system_include(&system_include, &config);
            }
        }

        Ok(None)
    }

    /// Process a local include file
    fn process_local_include(
        &self,
        filename: &str,
        config: &Config,
    ) -> Result<Option<String>, ConversionError> {
        // First try to resolve relative to base_dir
        let file_path = FileUtils::resolve_path(&config.base_dir, filename);

        // If not found, search in include directories
        let _file_path_option = if file_path.exists() {
            Some(file_path) // Use the resolved file_path
        } else if let Some(path) = FileUtils::find_in_include_dirs(filename, &config.include_dirs) {
            Some(path) // Use the found file path
        } else {
            // If not found, just generate a mod statement with a comment
            return self.generate_mod_statement(filename, true, config);
        };

        // Generate the mod statement
        self.generate_mod_statement(filename, false, config)
    }

    /// Process a system include file
    fn process_system_include(
        &self,
        filename: &str,
        config: &Config,
    ) -> Result<Option<String>, ConversionError> {
        if !config.process_system_includes {
            // Return a comment if we're not processing system includes
            return Ok(Some(format!("// System import: {} - disabled\n", filename)));
        }

        // Standard library mapping
        let stdlib_mappings = self.get_stdlib_mappings();

        if let Some(imports) = stdlib_mappings.get(filename) {
            // Return standard library import statements
            return Ok(Some(format!(
                "// System import: {}\n{}\n",
                filename, imports
            )));
        }

        // For other system includes, just return a comment
        Ok(Some(format!(
            "// System import: {} - not mapped\n",
            filename
        )))
    }

    /// Generate a Rust mod statement from an include path
    fn generate_mod_statement(
        &self,
        include_path: &str,
        not_found: bool,
        config: &Config,
    ) -> Result<Option<String>, ConversionError> {
        let path = Path::new(include_path);

        // Extract the module name from the file stem
        let file_name = path
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("unknown");

        // If there's no directory part, it's a simple mod statement
        if path.parent().is_none() || path.parent() == Some(Path::new("")) {
            let comment = if not_found {
                format!("// Local include not found: {}\n", include_path)
            } else {
                format!("// Include: {}\n", include_path)
            };

            return Ok(Some(format!("{}mod {};", comment, file_name)));
        }

        // Handle nested paths by creating nested modules
        let mut module_path = Vec::new();
        let mut current_path = path.parent();

        // Build up the module path from the directory structure
        while let Some(dir) = current_path {
            if let Some(dir_name) = dir.file_name().and_then(|s| s.to_str()) {
                if !dir_name.is_empty() {
                    module_path.push(dir_name);
                }
            }
            current_path = dir.parent();
        }

        // Reverse the path since we built it from leaf to root
        module_path.reverse();

        // Generate nested mod statements
        let comment = if not_found {
            format!("// Local include not found: {}\n", include_path)
        } else {
            format!("// Include: {}\n", include_path)
        };

        let mut result = comment;

        // Start with the outermost module
        if !module_path.is_empty() {
            result.push_str(&format!("mod {} {{ ", module_path[0]));

            // Add the middle modules
            for i in 1..module_path.len() {
                result.push_str(&format!("mod {} {{ ", module_path[i]));
            }

            // Add the innermost module
            result.push_str(&format!("mod {}; ", file_name));

            // Close all the braces
            for _ in 0..module_path.len() {
                result.push('}');
            }
        } else {
            // Simple case with no directory structure
            result.push_str(&format!("mod {};", file_name));
        }

        Ok(Some(result))
    }

    /// Get standard library mappings for common C headers
    fn get_stdlib_mappings(&self) -> std::collections::HashMap<String, &'static str> {
        let mut map = std::collections::HashMap::new();
        map.insert(
            "stdio.h".to_string(),
            "use std::io;\nuse std::io::{Read, Write};",
        );
        map.insert("stdlib.h".to_string(), "use std::alloc;\nuse std::mem;");
        map.insert(
            "string.h".to_string(),
            "use std::str;\nuse std::string::String;\nuse std::ffi::CStr;",
        );
        map
    }

    /// Add an include directory for searching header files
    pub fn add_include_dir(&mut self, dir: String) -> &mut Self {
        self.config.add_include_dir(dir);
        self
    }

    /// Set the base directory for resolving relative paths
    pub fn set_base_dir(&mut self, dir: String) -> &mut Self {
        self.config = self.config.clone().with_base_dir(dir);
        self
    }

    /// Set whether to process system includes
    pub fn with_system_includes(&mut self, enabled: bool) -> &mut Self {
        self.config = self.config.clone().with_system_includes(enabled);
        self
    }
}

impl TokenHandler for IncludeHandler {
    fn can_handle(&self, tokens: &[Token], _context: &ParserContext) -> bool {
        if tokens.len() < 3 || tokens[0] != Token::Hash {
            return false;
        }

        // Check for #include directive
        if let Token::Identifier(ref directive) = tokens[1] {
            if directive != "include" {
                return false;
            }

            // Handle local include "file.h"
            if let Token::StringLiteral(_) = tokens[2] {
                return true;
            }

            // Handle system include <file.h>
            if tokens[2] == Token::LessThan {
                // Look for the closing '>'
                for token in &tokens[3..] {
                    if *token == Token::GreaterThan {
                        return true;
                    }
                }
            }
        }

        false
    }

    fn handle(
        &self,
        tokens: &[Token],
        context: &mut ParserContext,
    ) -> Result<HandlerResult, ConversionError> {
        match self.process_include(tokens, context)? {
            Some(rust_code) => Ok(HandlerResult::Replace(rust_code)),
            None => Ok(HandlerResult::NotHandled),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token_parser::Tokenizer;

    #[test]
    fn test_local_include() {
        let c_code = "#include \"myheader.h\"";
        let mut tokenizer = Tokenizer::new(c_code);
        let tokens = tokenizer.tokenize();

        let handler = IncludeHandler::default();
        let result = handler.handle(&tokens, &mut ParserContext::new()).unwrap();

        if let HandlerResult::Replace(rust_code) = result {
            assert!(rust_code.contains("mod myheader;"));
        } else {
            panic!("Expected HandlerResult::Replace, got {:?}", result);
        }
    }

    #[test]
    fn test_nested_include() {
        let c_code = "#include \"utils/helper.h\"";
        let mut tokenizer = Tokenizer::new(c_code);
        let tokens = tokenizer.tokenize();

        let handler = IncludeHandler::default();
        let result = handler.handle(&tokens, &mut ParserContext::new()).unwrap();

        if let HandlerResult::Replace(rust_code) = result {
            assert!(rust_code.contains("mod utils { mod helper; }"));
        } else {
            panic!("Expected HandlerResult::Replace, got {:?}", result);
        }
    }

    #[test]
    fn test_system_include() {
        let c_code = "#include <stdio.h>";
        let mut tokenizer = Tokenizer::new(c_code);
        let tokens = tokenizer.tokenize();

        let handler = IncludeHandler::default();
        let result = handler.handle(&tokens, &mut ParserContext::new()).unwrap();

        if let HandlerResult::Replace(rust_code) = result {
            assert!(rust_code.contains("System import: stdio.h"));
        } else {
            panic!("Expected HandlerResult::Replace, got {:?}", result);
        }
    }

    #[test]
    fn test_system_include_processing() {
        let c_code = "#include <stdio.h>";
        let mut tokenizer = Tokenizer::new(c_code);
        let tokens = tokenizer.tokenize();

        let mut handler = IncludeHandler::default();
        handler.with_system_includes(true);

        let result = handler.handle(&tokens, &mut ParserContext::new()).unwrap();

        if let HandlerResult::Replace(rust_code) = result {
            assert!(rust_code.contains("std::io"));
        } else {
            panic!("Expected HandlerResult::Replace, got {:?}", result);
        }
    }
}
