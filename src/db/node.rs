//! Node types and kind constants for the DB system
//!
//! This module defines the node kind constants and provides convenience
//! functions for creating nodes.

use crate::db::web::Entry;
use std::collections::HashMap;
use std::ops::Range;

// ============================================================================
// Node Kind Constants
// ============================================================================

/// Top-level declarations
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum NodeKind {
    // Top-level declarations
    Function,
    Struct,
    Enum,
    Typedef,
    TypeAlias,
    Global,
    Variable,
    Array,
    Macro,
    Include,
    Comment,
    Loop,
    Type,

    // Function components
    Parameter,
    ReturnType,
    FunctionBody,

    // Struct components
    Field,

    // Enum components
    Variant,

    // Statements
    VariableDeclaration,
    Assignment,
    Expression,
    IfStatement,
    WhileLoop,
    ForLoop,
    DoWhile,
    Switch,
    Case,
    Return,
    Break,
    Continue,
    FunctionCall,

    // Control flow keywords
    If,
    Else,
    While,
    For,
    Do,
    Goto,

    // Expressions
    BinaryOp,
    UnaryOp,
    Literal,
    Identifier,
    Cast,

    // Composite
    Block,
    Annotation,
    Sample,
    Unknown,
}

impl NodeKind {
    pub fn as_str(&self) -> &'static str {
        match self {
            NodeKind::Function => "Function",
            NodeKind::Struct => "Struct",
            NodeKind::Enum => "Enum",
            NodeKind::Typedef => "Typedef",
            NodeKind::TypeAlias => "TypeAlias",
            NodeKind::Global => "Global",
            NodeKind::Variable => "Variable",
            NodeKind::Array => "Array",
            NodeKind::Macro => "Macro",
            NodeKind::Include => "Include",
            NodeKind::Comment => "Comment",
            NodeKind::Loop => "Loop",
            NodeKind::Type => "Type",
            NodeKind::Parameter => "Parameter",
            NodeKind::ReturnType => "ReturnType",
            NodeKind::FunctionBody => "FunctionBody",
            NodeKind::Field => "Field",
            NodeKind::Variant => "Variant",
            NodeKind::VariableDeclaration => "VariableDeclaration",
            NodeKind::Assignment => "Assignment",
            NodeKind::Expression => "Expression",
            NodeKind::IfStatement => "IfStatement",
            NodeKind::WhileLoop => "WhileLoop",
            NodeKind::ForLoop => "ForLoop",
            NodeKind::DoWhile => "DoWhile",
            NodeKind::Switch => "Switch",
            NodeKind::Case => "Case",
            NodeKind::Return => "Return",
            NodeKind::Break => "Break",
            NodeKind::Continue => "Continue",
            NodeKind::FunctionCall => "FunctionCall",
            NodeKind::If => "If",
            NodeKind::Else => "Else",
            NodeKind::While => "While",
            NodeKind::For => "For",
            NodeKind::Do => "Do",
            NodeKind::Goto => "Goto",
            NodeKind::BinaryOp => "BinaryOp",
            NodeKind::UnaryOp => "UnaryOp",
            NodeKind::Literal => "Literal",
            NodeKind::Identifier => "Identifier",
            NodeKind::Cast => "Cast",
            NodeKind::Block => "Block",
            NodeKind::Annotation => "Annotation",
            NodeKind::Sample => "Sample",
            NodeKind::Unknown => "Unknown",
        }
    }
}
impl From<NodeKind> for &str {
    fn from(kind: NodeKind) -> Self {
        kind.as_str()
    }
}
impl From<NodeKind> for String {
    fn from(kind: NodeKind) -> Self {
        kind.as_str().to_string()
    }
}
// Re-export commonly used kinds at module level
pub use NodeKind::*;

// ============================================================================
// Node Creation Functions
// ============================================================================

/// Create a basic node with kind and name.
pub fn node(kind: impl Into<String>, name: impl Into<String>) -> Entry {
    Entry::node(kind.into(), name.into())
}

/// Create a node with source range.
pub fn node_with_range(kind: impl Into<String>, name: impl Into<String>, range: Range<usize>) -> Entry {
    let mut n = Entry::node(kind.into(), name.into());
    n.set_attr("source_range", Entry::range(range));
    n
}

/// Create a node with attributes.
pub fn node_with_attrs(
    kind: impl Into<String>,
    name: impl Into<String>,
    attrs: HashMap<String, Entry>,
) -> Entry {
    Entry::node_with_attrs(kind.into(), name.into(), attrs)
}

/// Create a function node.
pub fn function(name: impl Into<String>) -> Entry {
    node(NodeKind::Function, name)
}

/// Create a function node with return type.
pub fn function_with_return(name: impl Into<String>, return_type: impl Into<String>) -> Entry {
    let mut f = node(NodeKind::Function, name);
    f.set_attr("return_type", Entry::string(return_type.into()));
    f
}

/// Create a struct node.
pub fn struct_node(name: impl Into<String>) -> Entry {
    node(NodeKind::Struct, name)
}

/// Create an enum node.
pub fn enum_node(name: impl Into<String>) -> Entry {
    node(NodeKind::Enum, name)
}

/// Create a parameter node.
pub fn param(name: impl Into<String>, type_name: impl Into<String>) -> Entry {
    let mut p = node(NodeKind::Parameter, name);
    p.set_attr("type", Entry::string(type_name.into()));
    p
}

/// Create a field node.
pub fn field(name: impl Into<String>, type_name: impl Into<String>) -> Entry {
    let mut f = node(NodeKind::Field, name);
    f.set_attr("type", Entry::string(type_name.into()));
    f
}

/// Create a variant node.
pub fn variant(name: impl Into<String>) -> Entry {
    node(NodeKind::Variant, name)
}

/// Create a variant node with value.
pub fn variant_with_value(name: impl Into<String>, value: i64) -> Entry {
    let mut v = node(NodeKind::Variant, name);
    v.set_attr("value", Entry::i64(value));
    v
}

/// Create a global variable node.
pub fn global(name: impl Into<String>, type_name: impl Into<String>) -> Entry {
    let mut g = node(NodeKind::Global, name);
    g.set_attr("type", Entry::string(type_name.into()));
    g
}

/// Create a typedef node.
pub fn typedef(name: impl Into<String>, target_type: impl Into<String>) -> Entry {
    let mut t = node(NodeKind::Typedef, name);
    t.set_attr("target_type", Entry::string(target_type.into()));
    t
}

/// Create a macro node.
pub fn macro_node(name: impl Into<String>) -> Entry {
    node(NodeKind::Macro, name)
}

/// Create an include node.
pub fn include(path: impl Into<String>, is_system: bool) -> Entry {
    let mut i = node(NodeKind::Include, path);
    i.set_attr("is_system", Entry::bool(is_system));
    i
}

/// Create a comment node.
pub fn comment(text: impl Into<String>) -> Entry {
    let mut c = node(NodeKind::Comment, "");
    c.set_attr("text", Entry::string(text.into()));
    c
}

// ============================================================================
// Node Extension Methods
// ============================================================================

/// Extension trait for Web nodes.
pub trait NodeExt {
    /// Add an attribute with builder pattern.
    fn with_attr(self, key: impl Into<String>, value: Entry) -> Self;

    /// Set source range with builder pattern.
    fn with_range(self, range: Range<usize>) -> Self;

    /// Set depth with builder pattern.
    fn with_depth(self, depth: usize) -> Self;

    /// Set owner with builder pattern.
    fn with_owner(self, owner: impl Into<String>) -> Self;

    /// Set declaration order with builder pattern.
    fn with_order(self, order: usize) -> Self;

    /// Add a child node (stored in "children" attribute).
    fn with_child(self, child: Entry) -> Self;

    /// Add multiple children.
    fn with_children(self, children: Vec<Entry>) -> Self;
}

impl NodeExt for Entry {
    fn with_attr(mut self, key: impl Into<String>, value: Entry) -> Self {
        self.set_attr(key, value);
        self
    }

    fn with_range(mut self, range: Range<usize>) -> Self {
        self.set_attr("source_range", Entry::range(range));
        self
    }

    fn with_depth(mut self, depth: usize) -> Self {
        self.set_depth(depth);
        self
    }

    fn with_owner(mut self, owner: impl Into<String>) -> Self {
        self.set_attr("owner", Entry::string(owner.into()));
        self
    }

    fn with_order(mut self, order: usize) -> Self {
        self.set_attr("declaration_order", Entry::usize(order));
        self
    }

    fn with_child(mut self, child: Entry) -> Self {
        match &mut self {
            Entry::Node { attrs, .. } => {
                let children = attrs
                    .entry("children".to_string())
                    .or_insert_with(|| Entry::vec(Vec::new()));
                if let Entry::Vec(v, _) = children {
                    v.push(child);
                }
            }
            _ => {}
        }
        self
    }

    fn with_children(mut self, children: Vec<Entry>) -> Self {
        for child in children {
            self = self.with_child(child);
        }
        self
    }
}

// ============================================================================
// Build Trait Import
// ============================================================================

use crate::db::web::Build;

// ============================================================================
// Function - Data holder implementing Build
// ============================================================================

/// Parameter definition for functions
#[derive(Debug, Clone)]
pub struct Param {
    pub name: String,
    pub type_name: String,
}

impl Param {
    pub fn new(name: impl Into<String>, type_name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            type_name: type_name.into(),
        }
    }
}

impl Build for Param {
    fn to_entry(&self) -> Entry {
        let mut entry = Entry::node("Param", &self.name);
        entry.set_attr("type", Entry::string(&self.type_name));
        entry
    }

    fn kind(&self) -> &str {
        "Param"
    }

    fn name(&self) -> Option<&str> {
        Some(&self.name)
    }

    fn category(&self) -> Option<&str> {
        Some("parameter")
    }
}

/// Function definition that implements Build trait.
#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub return_type: Option<String>,
    pub params: Vec<Param>,
    pub range: Option<Range<usize>>,
    pub is_static: bool,
    pub is_inline: bool,
    pub is_extern: bool,
    pub attrs: HashMap<String, Entry>,
}

impl Function {
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            return_type: None,
            params: Vec::new(),
            range: None,
            is_static: false,
            is_inline: false,
            is_extern: false,
            attrs: HashMap::new(),
        }
    }

    pub fn with_range(mut self, range: Range<usize>) -> Self {
        self.range = Some(range);
        self
    }

    pub fn returns(mut self, return_type: impl Into<String>) -> Self {
        self.return_type = Some(return_type.into());
        self
    }

    pub fn param(mut self, name: impl Into<String>, type_name: impl Into<String>) -> Self {
        self.params.push(Param::new(name, type_name));
        self
    }

    pub fn is_static(mut self) -> Self {
        self.is_static = true;
        self
    }

    pub fn is_inline(mut self) -> Self {
        self.is_inline = true;
        self
    }

    pub fn is_extern(mut self) -> Self {
        self.is_extern = true;
        self
    }

    pub fn attr(mut self, key: impl Into<String>, value: Entry) -> Self {
        self.attrs.insert(key.into(), value);
        self
    }

    /// Legacy: Build to Entry directly
    pub fn build(self) -> Entry {
        self.to_entry()
    }
}

impl Build for Function {
    fn to_entry(&self) -> Entry {
        let mut node = function(&self.name);

        if let Some(ref range) = self.range {
            node = node.with_range(range.clone());
        }
        if let Some(ref rt) = self.return_type {
            node.set_attr("return_type", Entry::string(rt));
        }
        for p in &self.params {
            node = node.with_child(param(&p.name, &p.type_name));
        }
        if self.is_static {
            node.set_attr("is_static", Entry::bool(true));
        }
        if self.is_inline {
            node.set_attr("is_inline", Entry::bool(true));
        }
        if self.is_extern {
            node.set_attr("is_extern", Entry::bool(true));
        }
        for (k, v) in &self.attrs {
            node.set_attr(k, v.clone());
        }
        node
    }

    fn kind(&self) -> &str {
        NodeKind::Function.as_str()
    }
    fn name(&self) -> Option<&str> {
        Some(&self.name)
    }
}

/// Alias for backwards compatibility
pub type FunctionBuilder = Function;

// ============================================================================
// Struct - Data holder implementing Build
// ============================================================================

/// Field definition for structs
#[derive(Debug, Clone)]
pub struct Field {
    pub name: String,
    pub type_name: String,
}

impl Field {
    pub fn new(name: impl Into<String>, type_name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            type_name: type_name.into(),
        }
    }
}

impl Build for Field {
    fn to_entry(&self) -> Entry {
        let mut entry = Entry::node(NodeKind::Field.as_str(), &self.name);
        entry.set_attr("type", Entry::string(&self.type_name));
        entry
    }

    fn kind(&self) -> &str {
        NodeKind::Field.as_str()
    }

    fn name(&self) -> Option<&str> {
        Some(&self.name)
    }

    fn category(&self) -> Option<&str> {
        Some("field")
    }
}

/// Struct definition that implements Build trait.
#[derive(Debug, Clone)]
pub struct Struct {
    pub name: String,
    pub fields: Vec<Field>,
    pub range: Option<Range<usize>>,
    pub is_packed: bool,
    pub attrs: HashMap<String, Entry>,
}

impl Struct {
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            fields: Vec::new(),
            range: None,
            is_packed: false,
            attrs: HashMap::new(),
        }
    }

    pub fn with_range(mut self, range: Range<usize>) -> Self {
        self.range = Some(range);
        self
    }

    pub fn field(mut self, name: impl Into<String>, type_name: impl Into<String>) -> Self {
        self.fields.push(Field::new(name, type_name));
        self
    }

    pub fn is_packed(mut self) -> Self {
        self.is_packed = true;
        self
    }

    pub fn attr(mut self, key: impl Into<String>, value: Entry) -> Self {
        self.attrs.insert(key.into(), value);
        self
    }

    /// Legacy: Build to Entry directly
    pub fn build(self) -> Entry {
        self.to_entry()
    }
}

impl Build for Struct {
    fn to_entry(&self) -> Entry {
        let mut node = struct_node(&self.name);

        if let Some(ref range) = self.range {
            node = node.with_range(range.clone());
        }
        for f in &self.fields {
            node = node.with_child(field(&f.name, &f.type_name));
        }
        if self.is_packed {
            node.set_attr("is_packed", Entry::bool(true));
        }
        for (k, v) in &self.attrs {
            node.set_attr(k, v.clone());
        }
        node
    }

    fn kind(&self) -> &str {
        NodeKind::Struct.as_str()
    }
    fn name(&self) -> Option<&str> {
        Some(&self.name)
    }
}

/// Alias for backwards compatibility
pub type StructBuilder = Struct;

// ============================================================================
// Enum - Data holder implementing Build
// ============================================================================

/// Variant definition for enums
#[derive(Debug, Clone)]
pub struct Variant {
    pub name: String,
    pub value: Option<i64>,
}

impl Variant {
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            value: None,
        }
    }

    pub fn with_value(name: impl Into<String>, value: i64) -> Self {
        Self {
            name: name.into(),
            value: Some(value),
        }
    }
}

impl Build for Variant {
    fn to_entry(&self) -> Entry {
        let mut entry = Entry::node(NodeKind::Variant.as_str(), &self.name);
        if let Some(v) = self.value {
            entry.set_attr("value", Entry::i64(v));
        }
        entry
    }

    fn kind(&self) -> &str {
        NodeKind::Variant.as_str()
    }

    fn name(&self) -> Option<&str> {
        Some(&self.name)
    }

    fn category(&self) -> Option<&str> {
        Some("enum")
    }
}

/// Enum definition that implements Build trait.
#[derive(Debug, Clone)]
pub struct Enum {
    pub name: String,
    pub variants: Vec<Variant>,
    pub range: Option<Range<usize>>,
    pub attrs: HashMap<String, Entry>,
}

impl Enum {
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            variants: Vec::new(),
            range: None,
            attrs: HashMap::new(),
        }
    }

    pub fn with_range(mut self, range: Range<usize>) -> Self {
        self.range = Some(range);
        self
    }

    pub fn variant(mut self, name: impl Into<String>) -> Self {
        self.variants.push(Variant::new(name));
        self
    }

    pub fn variant_value(mut self, name: impl Into<String>, value: i64) -> Self {
        self.variants.push(Variant::with_value(name, value));
        self
    }

    pub fn attr(mut self, key: impl Into<String>, value: Entry) -> Self {
        self.attrs.insert(key.into(), value);
        self
    }

    /// Legacy: Build to Entry directly
    pub fn build(self) -> Entry {
        self.to_entry()
    }
}

impl Build for Enum {
    fn to_entry(&self) -> Entry {
        let mut node = enum_node(&self.name);

        if let Some(ref range) = self.range {
            node = node.with_range(range.clone());
        }
        for v in &self.variants {
            let variant_node = if let Some(val) = v.value {
                variant_with_value(&v.name, val)
            } else {
                variant(&v.name)
            };
            node = node.with_child(variant_node);
        }
        for (k, v) in &self.attrs {
            node.set_attr(k, v.clone());
        }
        node
    }

    fn kind(&self) -> &str {
        NodeKind::Enum.as_str()
    }
    fn name(&self) -> Option<&str> {
        Some(&self.name)
    }
}

/// Alias for backwards compatibility
pub type EnumBuilder = Enum;

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::db::node;
    use crate::db::tree::Tree;
    use crate::db::web::Entry;
    #[test]
    fn test_node_creation() {
        let n = node("Function", "main");
        assert_eq!(n.kind(), Some("Function"));
        assert_eq!(n.name(), Some("main"));
    }

    #[test]
    fn test_node_with_range() {
        let n = node_with_range("Struct", "Point", 10..50);
        assert_eq!(n.source_range(), Some(10..50));
    }

    #[test]
    fn test_function_helper() {
        let f = function_with_return("calculate", "int");
        assert_eq!(f.kind(), Some("Function"));
        assert_eq!(f.get_string_attr("return_type"), Some("int"));
    }

    #[test]
    fn test_param_helper() {
        let p = param("argc", "int");
        assert_eq!(p.kind(), Some("Parameter"));
        assert_eq!(p.name(), Some("argc"));
        assert_eq!(p.get_string_attr("type"), Some("int"));
    }

    #[test]
    fn test_function_builder() {
        let tree = Tree::new().with_function("main", 0..100, |f| {
            f.returns("int")
                .param("argc", "int")
                .param("argv", "char**")
        });

        assert_eq!(tree.len(), 1);
        let funcs = tree.functions();
        assert_eq!(funcs.len(), 1);
        assert_eq!(funcs[0].name(), Some("main"));
    }

    #[test]
    fn test_struct_builder() {
        let tree =
            Tree::new().with_struct("Point", 0..50, |s| s.field("x", "int").field("y", "int"));

        let structs = tree.structs();
        assert_eq!(structs.len(), 1);
    }

    #[test]
    fn test_enum_builder() {
        let tree = Tree::new().with_enum("Color", 0..80, |e| {
            e.variant("Red")
                .variant_value("Green", 1)
                .variant_value("Blue", 2)
        });

        let enums = tree.enums();
        assert_eq!(enums.len(), 1);
    }

    #[test]
    fn test_mixed_builder() {
        let tree = Tree::new()
            .with_include("stdio.h", true, 0..20)
            .with_global("g_count", "int", 21..40)
            .with_function("init", 41..100, |f| f.returns("void"))
            .with_metadata("source", Entry::string("test.c"));

        assert_eq!(tree.includes().len(), 1);
        assert_eq!(tree.globals().len(), 1);
        assert_eq!(tree.functions().len(), 1);
    }
    #[test]
    fn test_node_ext_builder() {
        let n = function("test")
            .with_attr("return_type", Entry::string("void"))
            .with_range(0..100)
            .with_depth(0)
            .with_order(1);

        assert_eq!(n.get_string_attr("return_type"), Some("void"));
        assert_eq!(n.source_range(), Some(0..100));
        assert_eq!(n.depth(), 0);
        assert_eq!(n.declaration_order(), Some(1));
    }

    #[test]
    fn test_node_children() {
        let f = function("main")
            .with_child(param("argc", "int"))
            .with_child(param("argv", "char**"));

        let children = f.get_vec_attr("children");
        assert!(children.is_some());
        assert_eq!(children.unwrap().len(), 2);
    }
    #[test]
    fn test_declaration_order() {
        let mut tree = Tree::new();
        tree.add(function("first"));
        tree.add(function("second"));
        tree.add(function("third"));

        let funcs = tree.functions();

        assert_eq!(funcs[0].declaration_order(), Some(0));
        assert_eq!(funcs[1].declaration_order(), Some(1));
        assert_eq!(funcs[2].declaration_order(), Some(2));
    }
}
