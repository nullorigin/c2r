//! Common utility functions shared across multiple handlers

use std::io::Bytes;

use crate::system::system;

/// Convert C integer-as-boolean expressions to Rust boolean expressions
/// In C, integers can be used in boolean contexts (0 = false, non-zero = true)
/// In Rust, we need explicit comparisons: `!var` -> `var == 0`, `var` -> `var != 0`
pub fn convert_int_to_bool(expr: &str) -> String {
    // Handle `! identifier` pattern -> `identifier == 0`
    // Look for `!` followed by whitespace and an identifier (not already a comparison)
    let tokens: Vec<&str> = expr.split_whitespace().collect();
    let mut converted_tokens: Vec<String> = Vec::new();
    let mut i = 0;
    
    while i < tokens.len() {
        let token = tokens[i];
        
        if token == "!" && i + 1 < tokens.len() {
            let next = tokens[i + 1];
            // Check if next token is an identifier (not a comparison operator result)
            // and not already part of != 
            if is_int_variable(next) && !is_already_comparison(&tokens, i + 1) {
                converted_tokens.push(format!("{} == 0", next));
                i += 2;
                continue;
            }
        }
        
        // Handle bare integer variable in boolean context with && or ||
        if is_int_variable(token) && !is_already_comparison(&tokens, i) {
            // Check if this is in a boolean context (preceded or followed by && or ||)
            let in_bool_context = (i > 0 && matches!(tokens[i - 1], "&&" | "||")) ||
                                  (i + 1 < tokens.len() && matches!(tokens[i + 1], "&&" | "||"));
            if in_bool_context {
                converted_tokens.push(format!("{} != 0", token));
                i += 1;
                continue;
            }
        }
        
        converted_tokens.push(token.to_string());
        i += 1;
    }
    
    converted_tokens.join(" ")
}

/// Check if an identifier is likely an integer variable (not a pointer check or function call)
fn is_int_variable(s: &str) -> bool {
    // Must be a valid identifier
    if s.is_empty() || !s.chars().next().map(|c| c.is_alphabetic() || c == '_').unwrap_or(false) {
        return false;
    }
    // Must be all alphanumeric/underscore
    if !s.chars().all(|c| c.is_alphanumeric() || c == '_') {
        return false;
    }
    // Exclude keywords and common non-integers
    !matches!(s, "true" | "false" | "null" | "NULL" | "nullptr")
}

/// Check if the token at position is already part of a comparison expression
fn is_already_comparison(tokens: &[&str], pos: usize) -> bool {
    // Check if preceded by comparison operator
    if pos > 0 && matches!(tokens[pos - 1], "==" | "!=" | "<" | ">" | "<=" | ">=") {
        return true;
    }
    // Check if followed by comparison operator
    if pos + 1 < tokens.len() && matches!(tokens[pos + 1], "==" | "!=" | "<" | ">" | "<=" | ">=") {
        return true;
    }
    false
}

/// Smart join tokens: no space around parentheses, no space before ',' or ';'
pub fn smart_join_tokens(tokens: &[&str]) -> String {
    let mut result = String::new();
    for (i, token) in tokens.iter().enumerate() {
        if i > 0 {
            let prev = tokens[i - 1];
            // No space after '(' or before ')' or ',' or ';' or before '('
            let needs_space = prev != "(" && *token != ")" && *token != "," && *token != ";" && *token != "(";
            if needs_space {
                result.push(' ');
            }
        }
        result.push_str(token);
    }
    result
}

/// Convert C arrow operator (ptr->field or ptr -> field) to Rust (*ptr).field
pub fn convert_arrow_operator(expr: &str) -> String {
    let mut result = expr.to_string();
    
    // Handle spaced arrow: "ptr -> field" -> "(*ptr).field"
    while let Some(arrow_pos) = result.find(" -> ") {
        let before = &result[..arrow_pos];
        let after = &result[arrow_pos + 4..];
        
        // Find the identifier before the arrow (last word)
        let ptr_name = before.split_whitespace().last().unwrap_or("");
        let prefix = if before.len() > ptr_name.len() {
            &before[..before.len() - ptr_name.len()]
        } else {
            ""
        };
        
        // Find the field after the arrow (first word or until next operator)
        let field_end = after.find(|c: char| !c.is_alphanumeric() && c != '_').unwrap_or(after.len());
        let field = &after[..field_end];
        let suffix = &after[field_end..];
        
        result = format!("{}(*{}).{}{}", prefix, ptr_name, field, suffix);
    }
    
    // Handle non-spaced arrow: "ptr->field" -> "(*ptr).field"
    while let Some(arrow_pos) = result.find("->") {
        // Skip if already converted (would have parentheses)
        if arrow_pos > 0 && result.chars().nth(arrow_pos - 1) == Some(')') {
            break;
        }
        
        let before = &result[..arrow_pos];
        let after = &result[arrow_pos + 2..];
        
        // Find the identifier before the arrow
        let ptr_start = before.rfind(|c: char| !c.is_alphanumeric() && c != '_').map(|p| p + 1).unwrap_or(0);
        let ptr_name = &before[ptr_start..];
        let prefix = &before[..ptr_start];
        
        // Find the field after the arrow
        let field_end = after.find(|c: char| !c.is_alphanumeric() && c != '_').unwrap_or(after.len());
        let field = &after[..field_end];
        let suffix = &after[field_end..];
        
        result = format!("{}(*{}).{}{}", prefix, ptr_name, field, suffix);
    }
    
    result
}

/// Convert a C-style cast type, removing struct/enum/union and converting pointer syntax
pub fn convert_cast_type(s: &str) -> String {
    let mut t = s.to_string();
    // Remove struct/enum/union keywords
    t = t.replace("struct ", "").replace("enum ", "").replace("union ", "");
    // Convert pointer syntax
    if t.ends_with(" *") || t.ends_with("*") {
        let base = t.trim_end_matches(" *").trim_end_matches('*').trim();
        // Try to look up the type
        if let Some(rust_type) = system().lookup_type(base) {
            format!("*mut {}", rust_type)
        } else {
            format!("*mut {}", base)
        }
    } else {
        // Try to look up the type
        system().lookup_type(&t).unwrap_or(t)
    }
}

/// Convert C-style casts using type lookup (e.g., (int*)x -> (x as *mut i32))
pub fn convert_c_style_cast(arg: &str) -> String {
    let mut result = arg.to_string();
    let mut changed = true;
    
    // Keep looking for cast patterns until no more are found
    while changed {
        changed = false;
        
        // Find pattern: (identifier*) or (identifier **) etc.
        let chars: Vec<char> = result.chars().collect();
        for i in 0..chars.len() {
            if chars[i] == '(' {
                // Look for closing ) with * before it
                let mut j = i + 1;
                let mut type_start = j;
                let mut type_end = j;
                let mut star_count = 0;
                
                // Skip whitespace after (
                while j < chars.len() && chars[j].is_whitespace() {
                    j += 1;
                    type_start = j;
                }
                
                // Collect identifier (type name)
                while j < chars.len() && (chars[j].is_alphanumeric() || chars[j] == '_') {
                    j += 1;
                }
                type_end = j;
                
                // Skip whitespace
                while j < chars.len() && chars[j].is_whitespace() {
                    j += 1;
                }
                
                // Count *'s
                while j < chars.len() && chars[j] == '*' {
                    star_count += 1;
                    j += 1;
                }
                
                // Skip whitespace
                while j < chars.len() && chars[j].is_whitespace() {
                    j += 1;
                }
                
                // Check if we have ) - handle both pointer casts (int*) and non-pointer casts (int)
                if j < chars.len() && chars[j] == ')' && type_end > type_start {
                    let type_name: String = chars[type_start..type_end].iter().collect();
                    
                    // Only convert if it's a pointer cast OR a known C type (not arbitrary identifiers)
                    let is_c_builtin = matches!(type_name.as_str(), 
                        "int" | "char" | "float" | "double" | "long" | "short" | 
                        "unsigned" | "signed" | "void" | "size_t" | 
                        "uint8_t" | "uint16_t" | "uint32_t" | "uint64_t" |
                        "int8_t" | "int16_t" | "int32_t" | "int64_t");
                    
                    // Skip non-pointer casts of non-builtin types (could be function calls)
                    if star_count == 0 && !is_c_builtin {
                        // Not a cast we should convert, skip this match
                    } else
                    
                    // Try to look up the type
                    if let Some(rust_type) = system().lookup_type(&type_name) {
                        let ptr_str = "*mut ".repeat(star_count);
                        
                        // Get before and after
                        let before: String = chars[..i].iter().collect();
                        let after: String = chars[j+1..].iter().collect();
                        
                        // Count leading dereferences
                        let deref_count = before.chars().rev().take_while(|&c| c == '*').count();
                        let before_no_deref = before.trim_end_matches('*');
                        
                        // Find expression being cast - handle parenthesized expressions and member access
                        let after_trimmed = after.trim_start();
                        let (casted_expr, rest) = if after_trimmed.starts_with('(') {
                            // Find matching closing paren
                            let mut depth = 0;
                            let mut end_pos = 0;
                            for (idx, ch) in after_trimmed.chars().enumerate() {
                                match ch {
                                    '(' => depth += 1,
                                    ')' => {
                                        depth -= 1;
                                        if depth == 0 {
                                            end_pos = idx + 1;
                                            break;
                                        }
                                    }
                                    _ => {}
                                }
                            }
                            // Check for member access after the paren (e.g., (*node).data)
                            let after_paren = &after_trimmed[end_pos..];
                            if after_paren.starts_with('.') {
                                // Include the member access in the casted expression
                                let member_end = after_paren[1..].find(|c: char| 
                                    !c.is_alphanumeric() && c != '_'
                                ).map(|p| p + 1).unwrap_or(after_paren.len());
                                (&after_trimmed[..end_pos + member_end], &after_trimmed[end_pos + member_end..])
                            } else {
                                (&after_trimmed[..end_pos], &after_trimmed[end_pos..])
                            }
                        } else {
                            // Simple identifier - also check for member access
                            let mut expr_end = after_trimmed.find(|c: char| 
                                !c.is_alphanumeric() && c != '_' && c != '.'
                            ).unwrap_or(after_trimmed.len());
                            // Include member access chain (e.g., node.data.field)
                            (&after_trimmed[..expr_end], &after_trimmed[expr_end..])
                        };
                        
                        // Build Rust cast
                        let deref_prefix = "*".repeat(deref_count);
                        result = format!("{}{}({} as {}{}){}", 
                            before_no_deref,
                            deref_prefix,
                            casted_expr.trim(),
                            ptr_str,
                            rust_type,
                            rest);
                        changed = true;
                        break;
                    }
                }
            }
        }
    }
    
    result
}
