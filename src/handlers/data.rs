use crate::db::web::{Build, Entry};

// ============================================================================
// Generic Handler Patterns
// ============================================================================

/// A generic field for type definitions (structs, unions).
/// Consolidates the common pattern found in StructField and UnionField.
#[derive(Debug, Clone, Default)]
pub struct TypeField {
    /// Field name
    pub name: String,
    /// Field type (C type)
    pub c_type: String,
    /// Field type (Rust type)
    pub rust_type: String,
    /// Is this a pointer field
    pub is_pointer: bool,
    /// Array dimensions if any
    pub array_dims: Vec<usize>,
    /// Bitfield width (if this is a bitfield, struct-specific)
    pub bitfield_width: Option<u8>,
    /// Default value for constructor (if any)
    pub default_value: Option<String>,
}

impl TypeField {
    pub fn new(name: impl Into<String>, c_type: impl Into<String>, rust_type: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            c_type: c_type.into(),
            rust_type: rust_type.into(),
            is_pointer: false,
            array_dims: Vec::new(),
            bitfield_width: None,
            default_value: None,
        }
    }

    pub fn with_pointer(mut self, is_pointer: bool) -> Self {
        self.is_pointer = is_pointer;
        self
    }

    pub fn with_array_dims(mut self, dims: Vec<usize>) -> Self {
        self.array_dims = dims;
        self
    }

    pub fn with_bitfield(mut self, width: u8) -> Self {
        self.bitfield_width = Some(width);
        self
    }

    pub fn with_default(mut self, value: impl Into<String>) -> Self {
        self.default_value = Some(value.into());
        self
    }
}

impl Build for TypeField {
    fn to_entry(&self) -> Entry {
        let mut entry = Entry::node("TypeField", &self.name);
        entry.set_attr("name", Entry::string(&self.name));
        entry.set_attr("c_type", Entry::string(&self.c_type));
        entry.set_attr("rust_type", Entry::string(&self.rust_type));
        entry.set_attr("is_pointer", Entry::bool(self.is_pointer));
        if !self.array_dims.is_empty() {
            entry.set_attr("array_dims", Entry::string(&format!("{:?}", self.array_dims)));
        }
        if let Some(width) = self.bitfield_width {
            entry.set_attr("bitfield_width", Entry::usize(width as usize));
        }
        if let Some(ref default) = self.default_value {
            entry.set_attr("default_value", Entry::string(default));
        }
        entry
    }
    fn kind(&self) -> &str { "TypeField" }
    fn name(&self) -> Option<&str> { if self.name.is_empty() { None } else { Some(&self.name) } }
    fn category(&self) -> Option<&str> { Some("field") }
}

/// A generic type definition for compound types (structs, unions, enums).
/// The `members` field can hold fields (for struct/union) or variants (for enum).
#[derive(Debug, Clone, Default)]
pub struct TypeDef<M> {
    /// Type name
    pub name: String,
    /// Members (fields for struct/union, variants for enum)
    pub members: Vec<M>,
    /// Is this an anonymous type
    pub is_anonymous: bool,
    /// Original nesting depth (0 = top level)
    pub nesting_depth: usize,
    /// Type category (struct, union, enum)
    pub category: String,
}

impl<M> TypeDef<M> {
    pub fn new(name: impl Into<String>, category: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            members: Vec::new(),
            is_anonymous: false,
            nesting_depth: 0,
            category: category.into(),
        }
    }

    pub fn with_member(mut self, member: M) -> Self {
        self.members.push(member);
        self
    }

    pub fn with_members(mut self, members: impl IntoIterator<Item = M>) -> Self {
        self.members.extend(members);
        self
    }

    pub fn with_anonymous(mut self, is_anonymous: bool) -> Self {
        self.is_anonymous = is_anonymous;
        self
    }

    pub fn with_nesting_depth(mut self, depth: usize) -> Self {
        self.nesting_depth = depth;
        self
    }

    pub fn member_count(&self) -> usize {
        self.members.len()
    }
}

impl<M: Build> Build for TypeDef<M> {
    fn to_entry(&self) -> Entry {
        let mut entry = Entry::node("TypeDef", &self.name);
        entry.set_attr("name", Entry::string(&self.name));
        entry.set_attr("category", Entry::string(&self.category));
        entry.set_attr("is_anonymous", Entry::bool(self.is_anonymous));
        entry.set_attr("nesting_depth", Entry::usize(self.nesting_depth));
        entry.set_attr("member_count", Entry::usize(self.members.len()));
        for (i, member) in self.members.iter().enumerate() {
            entry.set_attr(&format!("member_{}", i), member.to_entry());
        }
        entry
    }
    fn kind(&self) -> &str { "TypeDef" }
    fn name(&self) -> Option<&str> { if self.name.is_empty() { None } else { Some(&self.name) } }
    fn category(&self) -> Option<&str> { Some(&self.category) }
}

/// A generic container for type data with nested types and typedef alias.
/// Consolidates StructData, UnionData, EnumData patterns.
#[derive(Debug, Clone, Default)]
pub struct TypeData<D> {
    /// Main type definition
    pub main_def: D,
    /// Nested type definitions that were extracted (flattened)
    pub nested_defs: Vec<D>,
    /// Typedef alias if any
    pub typedef_alias: Option<String>,
    /// Is this a forward declaration
    pub is_forward_declaration: bool,
}

impl<D: Default> TypeData<D> {
    pub fn new() -> Self {
        Self {
            main_def: D::default(),
            nested_defs: Vec::new(),
            typedef_alias: None,
            is_forward_declaration: false,
        }
    }

    pub fn with_main(mut self, main: D) -> Self {
        self.main_def = main;
        self
    }

    pub fn with_nested(mut self, nested: D) -> Self {
        self.nested_defs.push(nested);
        self
    }

    pub fn with_typedef_alias(mut self, alias: impl Into<String>) -> Self {
        self.typedef_alias = Some(alias.into());
        self
    }

    pub fn with_forward_declaration(mut self, is_forward: bool) -> Self {
        self.is_forward_declaration = is_forward;
        self
    }
}

impl<D: Build> Build for TypeData<D> {
    fn to_entry(&self) -> Entry {
        let mut entry = Entry::node("TypeData", "container");
        entry.set_attr("main_def", self.main_def.to_entry());
        entry.set_attr("nested_count", Entry::usize(self.nested_defs.len()));
        entry.set_attr("is_forward_declaration", Entry::bool(self.is_forward_declaration));
        if let Some(ref alias) = self.typedef_alias {
            entry.set_attr("typedef_alias", Entry::string(alias));
        }
        for (i, nested) in self.nested_defs.iter().enumerate() {
            entry.set_attr(&format!("nested_{}", i), nested.to_entry());
        }
        entry
    }
    fn kind(&self) -> &str { "TypeData" }
    fn category(&self) -> Option<&str> { Some("container") }
}

/// Named value with optional explicit value (for enum variants, constants).
#[derive(Debug, Clone, Default)]
pub struct NamedValue {
    /// Name/identifier
    pub name: String,
    /// Explicit numeric value (if any)
    pub value: Option<i64>,
    /// Value expression string (if not a simple integer)
    pub value_expr: Option<String>,
}

impl NamedValue {
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            value: None,
            value_expr: None,
        }
    }

    pub fn with_value(mut self, value: i64) -> Self {
        self.value = Some(value);
        self
    }

    pub fn with_expr(mut self, expr: impl Into<String>) -> Self {
        self.value_expr = Some(expr.into());
        self
    }
}

impl Build for NamedValue {
    fn to_entry(&self) -> Entry {
        let mut entry = Entry::node("NamedValue", &self.name);
        entry.set_attr("name", Entry::string(&self.name));
        if let Some(value) = self.value {
            entry.set_attr("value", Entry::i64(value));
        }
        if let Some(ref expr) = self.value_expr {
            entry.set_attr("value_expr", Entry::string(expr));
        }
        entry
    }
    fn kind(&self) -> &str { "NamedValue" }
    fn name(&self) -> Option<&str> { if self.name.is_empty() { None } else { Some(&self.name) } }
    fn category(&self) -> Option<&str> { Some("value") }
}

/// Variable/parameter information (consolidates common variable patterns).
#[derive(Debug, Clone, Default)]
pub struct VarInfo {
    /// Variable name
    pub name: String,
    /// C type
    pub c_type: String,
    /// Rust type  
    pub rust_type: String,
    /// Initial value if any
    pub initial_value: Option<String>,
    /// Is this a pointer
    pub is_pointer: bool,
    /// Is this const
    pub is_const: bool,
    /// Is this static
    pub is_static: bool,
    /// Array dimensions if any
    pub dimensions: Vec<usize>,
}

impl VarInfo {
    pub fn new(name: impl Into<String>, c_type: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            c_type: c_type.into(),
            rust_type: String::new(),
            initial_value: None,
            is_pointer: false,
            is_const: false,
            is_static: false,
            dimensions: Vec::new(),
        }
    }

    pub fn with_rust_type(mut self, rust_type: impl Into<String>) -> Self {
        self.rust_type = rust_type.into();
        self
    }

    pub fn with_initial_value(mut self, value: impl Into<String>) -> Self {
        self.initial_value = Some(value.into());
        self
    }

    pub fn with_pointer(mut self, is_pointer: bool) -> Self {
        self.is_pointer = is_pointer;
        self
    }

    pub fn with_const(mut self, is_const: bool) -> Self {
        self.is_const = is_const;
        self
    }

    pub fn with_static(mut self, is_static: bool) -> Self {
        self.is_static = is_static;
        self
    }

    pub fn with_dimensions(mut self, dims: Vec<usize>) -> Self {
        self.dimensions = dims;
        self
    }

    pub fn is_array(&self) -> bool {
        !self.dimensions.is_empty()
    }
}

impl Build for VarInfo {
    fn to_entry(&self) -> Entry {
        let mut entry = Entry::node("VarInfo", &self.name);
        entry.set_attr("name", Entry::string(&self.name));
        entry.set_attr("c_type", Entry::string(&self.c_type));
        entry.set_attr("rust_type", Entry::string(&self.rust_type));
        entry.set_attr("is_pointer", Entry::bool(self.is_pointer));
        entry.set_attr("is_const", Entry::bool(self.is_const));
        entry.set_attr("is_static", Entry::bool(self.is_static));
        if let Some(ref value) = self.initial_value {
            entry.set_attr("initial_value", Entry::string(value));
        }
        if !self.dimensions.is_empty() {
            let dims: Vec<Entry> = self.dimensions.iter().map(|d| Entry::usize(*d)).collect();
            entry.set_attr("dimensions", Entry::vec(dims));
        }
        entry
    }
    fn kind(&self) -> &str { "VarInfo" }
    fn name(&self) -> Option<&str> { if self.name.is_empty() { None } else { Some(&self.name) } }
    fn category(&self) -> Option<&str> { Some("variable") }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_type_field() {
        let field = TypeField::new("count", "int", "i32")
            .with_pointer(false)
            .with_array_dims(vec![10])
            .with_default("0");

        assert_eq!(field.name, "count");
        assert_eq!(field.c_type, "int");
        assert_eq!(field.rust_type, "i32");
        assert!(!field.is_pointer);
        assert_eq!(field.array_dims, vec![10]);
        assert_eq!(field.default_value, Some("0".to_string()));
    }

    #[test]
    fn test_type_def() {
        let def: TypeDef<TypeField> = TypeDef::new("MyStruct", "struct")
            .with_member(TypeField::new("x", "int", "i32"))
            .with_member(TypeField::new("y", "int", "i32"))
            .with_anonymous(false)
            .with_nesting_depth(0);

        assert_eq!(def.name, "MyStruct");
        assert_eq!(def.category, "struct");
        assert_eq!(def.member_count(), 2);
        assert!(!def.is_anonymous);
    }

    #[test]
    fn test_type_data() {
        let main_def: TypeDef<TypeField> = TypeDef::new("Point", "struct");
        let data: TypeData<TypeDef<TypeField>> = TypeData::new()
            .with_main(main_def)
            .with_typedef_alias("Point_t")
            .with_forward_declaration(false);

        assert_eq!(data.main_def.name, "Point");
        assert_eq!(data.typedef_alias, Some("Point_t".to_string()));
        assert!(!data.is_forward_declaration);
    }

    #[test]
    fn test_named_value() {
        let variant = NamedValue::new("SUCCESS")
            .with_value(0);

        assert_eq!(variant.name, "SUCCESS");
        assert_eq!(variant.value, Some(0));

        let expr_variant = NamedValue::new("COMPUTED")
            .with_expr("1 << 4");

        assert_eq!(expr_variant.value_expr, Some("1 << 4".to_string()));
    }

    #[test]
    fn test_var_info() {
        let var = VarInfo::new("buffer", "char")
            .with_rust_type("u8")
            .with_dimensions(vec![256])
            .with_const(true);

        assert_eq!(var.name, "buffer");
        assert_eq!(var.c_type, "char");
        assert_eq!(var.rust_type, "u8");
        assert!(var.is_const);
        assert!(var.is_array());
        assert_eq!(var.dimensions, vec![256]);
    }
}