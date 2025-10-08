//! Information types for different C elements
//! Used by handlers to store extracted data about C constructs

/// Information about a C function
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

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub struct ArrayInfo {
    pub complexity: String,
    pub element_type: String,
    pub has_initializer: bool,
    pub is_const: bool,
    pub is_declaration: bool,
    pub is_static: bool,
    pub name: String,
    pub size: Option<String>,
    pub storage_class: Option<String>,
}

impl ArrayInfo {
    pub fn is_mutable(&self) -> bool {
        !matches!(self.storage_class.as_deref(), Some("const"))
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub struct CommentInfo {
    pub comment_type: String,
    pub content: String,
    pub is_documentation: bool,
    pub is_multiline: bool,
    pub line_count: usize,
    pub raw_comment: String,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub struct ControlFlowInfo {
    pub complexity: String,
    pub description: String,
    pub flow_type: String,
    pub has_body: bool,
    pub has_condition: bool,
    pub has_else_clause: bool,
    pub is_infinite_loop: bool,
    pub is_range_based: bool,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub struct EnumInfo {
    pub complexity: String,
    pub has_body: bool,
    pub has_explicit_values: bool,
    pub is_anonymous: bool,
    pub is_declaration: bool,
    pub is_definition: bool,
    pub is_forward_declaration: bool,
    pub is_typedef: bool,
    pub name: String,
    pub typedef_name: Option<String>,
    pub variants: Vec<(String,Option<String>)>,
    pub variant_count: usize,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub struct ExpressionInfo {
    pub complexity: String,
    pub expression_type: String,
    pub is_arithmetic: bool,
    pub is_assignment: bool,
    pub is_logical: bool,
    pub left_operand: String,
    pub operands: Vec<String>,
    pub operator: String,
    pub operators: Vec<String>,
    pub raw_expression: String,
    pub right_operand: String,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub struct FunctionInfo {
    pub complexity: String,
    pub has_body: bool,
    pub is_declaration: bool,
    pub is_definition: bool,
    pub is_extern: bool,
    pub is_inline: bool,
    pub is_static: bool,
    pub is_unsafe: bool,
    pub is_variadic: bool,
    pub name: String,
    pub parameter_count: usize,
    pub parameters: Vec<String>,
    pub return_type: String,
    pub storage_class: Option<String>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub struct GlobalInfo {
    pub complexity: String,
    pub global_kind: String,
    pub initializer: Option<String>,
    pub is_array: bool,
    pub is_const: bool,
    pub is_extern: bool,
    pub is_static: bool,
    pub is_volatile: bool,
    pub kind_description: String,
    pub name: String,
    pub storage_class: String,
    pub type_name: String,
    pub var_type: String,
    pub dimensions: usize,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub struct IncludeInfo {
    pub is_local_include: bool,
    pub is_system_include: bool,
    pub name: String,
    pub path: String,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub struct MacroInfo {
    pub complexity: String,
    pub condition: String,
    pub directive: String,
    pub directive_type: String,
    pub include_path: String,
    pub is_conditional: bool,
    pub is_function_like: bool,
    pub is_system_include: bool,
    pub kind: MacroKind,
    pub kind_description: String,
    pub name: String,
    pub parameter_count: usize,
    pub parameters: Vec<String>,
    pub params: Vec<String>,
    pub pragma_directive: String,
    pub raw_directive: String,
    pub value: String,
    pub body: String,
}

/// Enum representing different kinds of macros for specialized handling
#[derive(Debug, Default, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum MacroKind {
    #[default]
    ObjectLike,
    FunctionLike,
    Conditional,
    Pragma,
    Message,
    Unknown,
}

#[derive(Debug, Default, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StructInfo {
    pub complexity: String,
    pub field_count: usize,
    pub fields: Vec<(String,String)>,
    pub has_body: bool,
    pub is_anonymous: bool,
    pub is_declaration: bool,
    pub is_definition: bool,
    pub is_forward_declaration: bool,
    pub is_packed: bool,
    pub is_typedef: bool,
    pub name: String,
    pub typedef_name: Option<String>,
}

#[derive(Debug, Default, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypedefInfo {
    pub alias_name: String,
    pub complexity: String,
    pub is_array: bool,
    pub is_declaration: bool,
    pub is_definition: bool,
    pub is_function_pointer: bool,
    pub kind: TypedefKind,
    pub kind_description: String,
    pub name: String,
    pub original_type: String,
    pub source_type: String,
    pub typedef_type: String,
}

/// Enum representing different kinds of typedefs for specialized handling
#[derive(Debug,Default, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TypedefKind {
    #[default]
    Simple,
    Pointer,
    Array,
    Function,
    Struct,
    Union,
    Enum,
    Unknown,
}
