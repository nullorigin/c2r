//! Documentation system for C-to-Rust conversion
//!
//! This module provides comprehensive documentation structures for all converted elements

use crate::global_handler::GlobalKind;

/// Main enum containing documentation information for each element type
#[derive(Debug, Clone)]
pub enum ElementInfo {
    Array(ArrayInfo),
    Comment(CommentInfo),
    ControlFlow(ControlFlowInfo),
    Enum(EnumInfo),
    Expression(ExpressionInfo),
    Function(FunctionInfo),
    Global(GlobalInfo),
    Include(IncludeInfo),
    Macro(MacroInfo),
    Struct(StructInfo),
    Typedef(TypedefInfo),
}
/// Array information extracted for documentation purposes
#[derive(Debug, Clone)]
pub struct ArrayInfo {
    pub name: String,
    pub element_type: String,
    pub size: Option<String>,
    pub is_declaration: bool,
    pub is_static: bool,
    pub is_const: bool,
    pub has_initializer: bool,
    pub complexity: String,
}

/// Comment information extracted for documentation purposes
#[derive(Debug, Clone)]
pub struct CommentInfo {
    pub comment_type: String,
    pub content: String,
    pub is_documentation: bool,
    pub is_multiline: bool,
    pub line_count: usize,
}
/// Control flow information extracted for documentation purposes
#[derive(Debug, Clone)]
pub struct ControlFlowInfo {
    pub flow_type: String,
    pub description: String,
    pub has_condition: bool,
    pub has_body: bool,
    pub has_else_clause: bool,
    pub is_infinite_loop: bool,
    pub is_range_based: bool,
    pub complexity: String,
}

/// Enum information extracted for documentation purposes
#[derive(Debug, Clone)]
pub struct EnumInfo {
    pub name: String,
    pub variant_count: usize,
    pub is_typedef: bool,
    pub is_anonymous: bool,
    pub is_forward_declaration: bool,
    pub has_explicit_values: bool,
    pub typedef_name: Option<String>,
    pub complexity: String,
}

/// Expression information extracted for documentation purposes
#[derive(Debug, Clone)]
pub struct ExpressionInfo {
    pub expression_type: String,
    pub left_operand: String,
    pub operator: String,
    pub right_operand: String,
    pub is_assignment: bool,
    pub is_arithmetic: bool,
    pub is_logical: bool,
    pub complexity: String,
}
/// Function information extracted for documentation purposes
#[derive(Debug, Clone)]
pub struct FunctionInfo {
    pub name: String,
    pub return_type: String,
    pub parameters: Vec<String>,
    pub parameter_count: usize,
    pub is_declaration: bool,
    pub is_definition: bool,
    pub is_static: bool,
    pub is_extern: bool,
    pub is_inline: bool,
    pub has_body: bool,
    pub complexity: String,
    pub(crate) is_unsafe: bool,
}

/// Global variable information extracted for documentation purposes
#[derive(Debug, Clone)]
pub struct GlobalInfo {
    pub name: String,
    pub kind: GlobalKind,
    pub kind_description: String,
    pub storage_class: Option<String>,
    pub type_name: String,
    pub is_const: bool,
    pub is_array: bool,
    pub has_initializer: bool,
    pub complexity: String,
}

/// Include information extracted for documentation purposes
#[derive(Debug, Clone)]
pub struct IncludeInfo {
    pub name: String,
    pub path: String,
    pub is_system_include: bool,
    pub is_local_include: bool,
}

/// Macro information extracted for documentation purposes
#[derive(Debug, Clone)]
pub struct MacroInfo {
    pub name: String,
    pub kind: MacroKind,
    pub kind_description: String,
    pub directive: String,
    pub params: Vec<String>,
    pub is_function_like: bool,
    pub is_conditional: bool,
    pub complexity: String,
}

/// Struct information extracted for documentation purposes
#[derive(Debug, Clone)]
pub struct StructInfo {
    pub name: String,
    pub field_count: usize,
    pub is_typedef: bool,
    pub is_anonymous: bool,
    pub is_forward_declaration: bool,
    pub is_packed: bool,
    pub typedef_name: Option<String>,
    pub complexity: String,
}

/// Typedef information extracted for documentation purposes
#[derive(Debug, Clone)]
pub struct TypedefInfo {
    pub name: String,
    pub kind: TypedefKind,
    pub kind_description: String,
    pub source_type: String,
    pub is_function_pointer: bool,
    pub is_array: bool,
    pub complexity: String,
}

/// Enum representing different kinds of macros for specialized handling
#[derive(Debug, Clone, PartialEq)]
pub enum MacroKind {
    ObjectLike,
    FunctionLike,
    Conditional,
    Pragma,
    Message,
    Unknown,
}

/// Enum representing different kinds of typedefs for specialized handling
#[derive(Debug, Clone, PartialEq)]
pub enum TypedefKind {
    Simple,
    Pointer,
    Array,
    Function,
    Struct,
    Union,
    Enum,
    Unknown,
}
