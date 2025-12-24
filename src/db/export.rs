//! Export functionality for DB trees
//!
//! Provides JSON and Rust code generation from DB trees.

use crate::db::node::kind;
use crate::db::tree::Tree;
use crate::db::web::Entry;
use std::fmt::Write;

// ============================================================================
// JSON Export
// ============================================================================

/// Convert tree to JSON string.
pub fn to_json(tree: &Tree) -> String {
    let mut output = String::new();
    output.push_str("{\n");

    // Metadata
    if let Some(_) = tree.get_metadata("source_file") {
        output.push_str("  \"metadata\": {\n");
        // Collect metadata entries
        let mut first = true;
        for key in ["source_file", "version", "generated_by"] {
            if let Some(value) = tree.get_metadata(key) {
                if !first {
                    output.push_str(",\n");
                }
                first = false;
                write!(output, "    \"{}\": {}", key, entry_to_json(value)).unwrap();
            }
        }
        output.push_str("\n  },\n");
    }

    // Roots
    output.push_str("  \"roots\": [\n");
    let roots: Vec<_> = tree.roots().collect();
    for (i, root) in roots.iter().enumerate() {
        output.push_str(&indent(&node_to_json(root), 4));
        if i < roots.len() - 1 {
            output.push(',');
        }
        output.push('\n');
    }
    output.push_str("  ]\n");
    output.push('}');

    output
}

fn entry_to_json(entry: &Entry) -> String {
    match entry {
        Entry::String(s, _) => format!("\"{}\"", escape_json(s)),
        Entry::Bool(b, _) => b.to_string(),
        Entry::I32(n, _) => n.to_string(),
        Entry::I64(n, _) => n.to_string(),
        Entry::F64(n, _) => n.to_string(),
        Entry::Usize(n, _) => n.to_string(),
        Entry::Unit(_) => "null".to_string(),
        Entry::Range(r, _) => format!("\"{}..{}\"", r.start, r.end),
        Entry::Vec(v, _) => {
            let items: Vec<String> = v.iter().map(entry_to_json).collect();
            format!("[{}]", items.join(", "))
        }
        Entry::HashMap(m, _) => {
            let items: Vec<String> = m
                .iter()
                .map(|(k, v)| format!("\"{}\": {}", k, entry_to_json(v)))
                .collect();
            format!("{{{}}}", items.join(", "))
        }
        Entry::Node {
            kind: _,
            name: _,
            attrs: _,
            ..
        } => node_to_json(entry),
        _ => format!("\"<{}>\"", entry.type_name()),
    }
}

fn node_to_json(node: &Entry) -> String {
    let mut output = String::new();
    output.push_str("{\n");

    if let Entry::Node {
        kind, name, attrs, ..
    } = node
    {
        write!(output, "  \"kind\": \"{}\",\n", kind).unwrap();
        write!(output, "  \"name\": \"{}\"", escape_json(name)).unwrap();

        // Source range
        if let Some(range) = node.source_range() {
            write!(
                output,
                ",\n  \"source_range\": \"{}..{}\"",
                range.start, range.end
            )
            .unwrap();
        }

        // Depth
        let depth = node.depth();
        if depth > 0 {
            write!(output, ",\n  \"depth\": {}", depth).unwrap();
        }

        // Declaration order
        if let Some(order) = node.declaration_order() {
            write!(output, ",\n  \"declaration_order\": {}", order).unwrap();
        }

        // Other attributes (excluding children, source_range, declaration_order)
        let skip_keys = ["children", "source_range", "declaration_order"];
        let other_attrs: Vec<_> = attrs
            .iter()
            .filter(|(k, _)| !skip_keys.contains(&k.as_str()))
            .collect();

        if !other_attrs.is_empty() {
            output.push_str(",\n  \"attributes\": {\n");
            for (i, (key, value)) in other_attrs.iter().enumerate() {
                write!(output, "    \"{}\": {}", key, entry_to_json(value)).unwrap();
                if i < other_attrs.len() - 1 {
                    output.push(',');
                }
                output.push('\n');
            }
            output.push_str("  }");
        }

        // Children
        if let Some(children) = node.get_vec_attr("children") {
            if !children.is_empty() {
                output.push_str(",\n  \"children\": [\n");
                for (i, child) in children.iter().enumerate() {
                    output.push_str(&indent(&node_to_json(child), 4));
                    if i < children.len() - 1 {
                        output.push(',');
                    }
                    output.push('\n');
                }
                output.push_str("  ]");
            }
        }
    }

    output.push_str("\n}");
    output
}

fn escape_json(s: &str) -> String {
    let mut result = String::with_capacity(s.len());
    for c in s.chars() {
        match c {
            '"' => result.push_str("\\\""),
            '\\' => result.push_str("\\\\"),
            '\n' => result.push_str("\\n"),
            '\r' => result.push_str("\\r"),
            '\t' => result.push_str("\\t"),
            c if c.is_control() => write!(result, "\\u{:04x}", c as u32).unwrap(),
            c => result.push(c),
        }
    }
    result
}

fn indent(s: &str, spaces: usize) -> String {
    let prefix = " ".repeat(spaces);
    s.lines()
        .map(|line| format!("{}{}", prefix, line))
        .collect::<Vec<_>>()
        .join("\n")
}

// ============================================================================
// Rust Code Generation
// ============================================================================

/// Convert tree to Rust code.
pub fn to_rust(tree: &Tree) -> String {
    let mut output = String::new();

    // Header
    output.push_str("// Generated by c2r DB system\n");
    output.push_str("// This code was generated from the intermediate representation\n\n");

    // Includes -> use statements
    for include in tree.includes() {
        if let Some(path) = include.name() {
            if include.get_bool_attr("is_system") == Some(true) {
                // System includes map to common Rust equivalents
                match path {
                    "stdio.h" => output.push_str("use std::io::{self, Write};\n"),
                    "stdlib.h" => output.push_str("use std::process;\n"),
                    "string.h" => output.push_str("// string.h -> use String methods\n"),
                    "math.h" => output.push_str("// math.h -> use std::f64 methods\n"),
                    _ => write!(output, "// #include <{}>\n", path).unwrap(),
                }
            } else {
                write!(output, "// #include \"{}\"\n", path).unwrap();
            }
        }
    }

    if !tree.includes().is_empty() {
        output.push('\n');
    }

    // Typedefs -> type aliases
    for typedef in tree.typedefs() {
        if let (Some(name), Some(target)) = (typedef.name(), typedef.get_string_attr("target_type"))
        {
            write!(output, "type {} = {};\n", name, c_type_to_rust(target)).unwrap();
        }
    }

    if !tree.typedefs().is_empty() {
        output.push('\n');
    }

    // Structs
    for s in tree.structs() {
        output.push_str(&struct_to_rust(s));
        output.push('\n');
    }

    // Enums
    for e in tree.enums() {
        output.push_str(&enum_to_rust(e));
        output.push('\n');
    }

    // Globals -> static/const
    for global in tree.globals() {
        output.push_str(&global_to_rust(global));
    }

    if !tree.globals().is_empty() {
        output.push('\n');
    }

    // Functions
    for func in tree.functions() {
        output.push_str(&function_to_rust(func));
        output.push('\n');
    }

    output
}

fn struct_to_rust(s: &Entry) -> String {
    let mut output = String::new();

    if let Some(name) = s.name() {
        // Check for packed attribute
        if s.get_bool_attr("is_packed") == Some(true) {
            output.push_str("#[repr(C, packed)]\n");
        } else {
            output.push_str("#[repr(C)]\n");
        }

        write!(output, "pub struct {} {{\n", name).unwrap();

        // Fields from children
        if let Some(children) = s.get_vec_attr("children") {
            for child in children {
                if child.kind() == Some(kind::FIELD) {
                    if let (Some(field_name), Some(field_type)) =
                        (child.name(), child.get_string_attr("type"))
                    {
                        write!(
                            output,
                            "    pub {}: {},\n",
                            field_name,
                            c_type_to_rust(field_type)
                        )
                        .unwrap();
                    }
                }
            }
        }

        output.push_str("}\n");
    }

    output
}

fn enum_to_rust(e: &Entry) -> String {
    let mut output = String::new();

    if let Some(name) = e.name() {
        output.push_str("#[repr(C)]\n");
        write!(output, "pub enum {} {{\n", name).unwrap();

        // Variants from children
        if let Some(children) = e.get_vec_attr("children") {
            for child in children {
                if child.kind() == Some(kind::VARIANT) {
                    if let Some(variant_name) = child.name() {
                        if let Some(value) = child.get_number_attr("value") {
                            write!(output, "    {} = {},\n", variant_name, value as i64).unwrap();
                        } else {
                            write!(output, "    {},\n", variant_name).unwrap();
                        }
                    }
                }
            }
        }

        output.push_str("}\n");
    }

    output
}

fn global_to_rust(g: &Entry) -> String {
    let mut output = String::new();

    if let (Some(name), Some(type_name)) = (g.name(), g.get_string_attr("type")) {
        let rust_type = c_type_to_rust(type_name);

        // Check if const
        if g.get_bool_attr("is_const") == Some(true) {
            write!(
                output,
                "pub const {}: {} = todo!();\n",
                name.to_uppercase(),
                rust_type
            )
            .unwrap();
        } else {
            write!(
                output,
                "pub static mut {}: {} = todo!();\n",
                name.to_uppercase(),
                rust_type
            )
            .unwrap();
        }
    }

    output
}

fn function_to_rust(f: &Entry) -> String {
    let mut output = String::new();

    if let Some(name) = f.name() {
        // Visibility
        if f.get_bool_attr("is_static") != Some(true) {
            output.push_str("pub ");
        }

        // Unsafe for extern functions
        if f.get_bool_attr("is_extern") == Some(true) {
            output.push_str("unsafe extern \"C\" ");
        }

        output.push_str("fn ");
        output.push_str(name);
        output.push('(');

        // Parameters from children
        let mut params = Vec::new();
        if let Some(children) = f.get_vec_attr("children") {
            for child in children {
                if child.kind() == Some(kind::PARAMETER) {
                    if let (Some(param_name), Some(param_type)) =
                        (child.name(), child.get_string_attr("type"))
                    {
                        params.push(format!("{}: {}", param_name, c_type_to_rust(param_type)));
                    }
                }
            }
        }
        output.push_str(&params.join(", "));
        output.push(')');

        // Return type
        if let Some(return_type) = f.get_string_attr("return_type") {
            if return_type != "void" {
                write!(output, " -> {}", c_type_to_rust(return_type)).unwrap();
            }
        }

        output.push_str(" {\n");
        output.push_str("    todo!()\n");
        output.push_str("}\n");
    }

    output
}

fn c_type_to_rust(c_type: &str) -> String {
    match c_type.trim() {
        "void" => "()".to_string(),
        "int" => "i32".to_string(),
        "unsigned int" | "unsigned" => "u32".to_string(),
        "long" => "i64".to_string(),
        "unsigned long" => "u64".to_string(),
        "short" => "i16".to_string(),
        "unsigned short" => "u16".to_string(),
        "char" => "i8".to_string(),
        "unsigned char" => "u8".to_string(),
        "float" => "f32".to_string(),
        "double" => "f64".to_string(),
        "size_t" => "usize".to_string(),
        "ssize_t" => "isize".to_string(),
        "bool" | "_Bool" => "bool".to_string(),
        s if s.ends_with('*') => {
            let inner = s.trim_end_matches('*').trim();
            if inner == "void" {
                "*mut std::ffi::c_void".to_string()
            } else if inner == "const char" {
                "*const i8".to_string()
            } else if inner == "char" {
                "*mut i8".to_string()
            } else {
                format!("*mut {}", c_type_to_rust(inner))
            }
        }
        s if s.starts_with("const ") => c_type_to_rust(&s[6..]),
        s => s.to_string(),
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::db::tree::Tree;

    #[test]
    fn test_to_json() {
        let tree =
            Tree::new().with_function("main", 0..100, |f| f.returns("int").param("argc", "int"));

        let json = to_json(&tree);
        assert!(json.contains("\"kind\": \"Function\""));
        assert!(json.contains("\"name\": \"main\""));
    }

    #[test]
    fn test_to_rust_function() {
        let tree = Tree::new().with_function("calculate", 0..100, |f| {
            f.returns("int").param("x", "int").param("y", "int")
        });

        let rust = to_rust(&tree);
        assert!(rust.contains("pub fn calculate"));
        assert!(rust.contains("x: i32"));
        assert!(rust.contains("-> i32"));
    }

    #[test]
    fn test_to_rust_struct() {
        let tree =
            Tree::new().with_struct("Point", 0..50, |s| s.field("x", "int").field("y", "int"));

        let rust = to_rust(&tree);
        assert!(rust.contains("pub struct Point"));
        assert!(rust.contains("pub x: i32"));
        assert!(rust.contains("pub y: i32"));
    }

    #[test]
    fn test_c_type_to_rust() {
        assert_eq!(c_type_to_rust("int"), "i32");
        assert_eq!(c_type_to_rust("unsigned int"), "u32");
        assert_eq!(c_type_to_rust("char*"), "*mut i8");
        assert_eq!(c_type_to_rust("void*"), "*mut std::ffi::c_void");
        assert_eq!(c_type_to_rust("const int"), "i32");
    }
}
