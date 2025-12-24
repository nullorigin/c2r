//! Entry-Based Database System (DB)
//!
//! A unified database system using Entry as the foundation with the Build trait
//! for consistent data storage and reconstruction.
//!
//! ## Architecture
//!
//! ### Core Types (implement Build trait)
//! - **Function, Struct, Enum**: Code structure representations
//! - **Sample**: Pattern samples with elements
//! - **Pattern**: Pattern matching definitions
//! - **Report**: Generic reports with fields and relations
//! - **Annotation**: Code annotations with severity
//! - **Transform**: Transformation records
//!
//! ### Infrastructure
//! - **Web**: Unified database storing all Build implementors
//! - **Entry**: Core data type for all values
//! - **Site**: Arena-based storage with graph connectivity
//! - **Tree**: Ergonomic tree construction wrapper
//!
//! ### Utilities
//! - **Store**: Generic storage traits (HashStore, GroupedStore, PriorityStore)
//! - **Query**: Search and filter interface
//! - **Token**: Tokenization system
//! - **Config**: Runtime configuration
//!
//! ## Usage
//!
//! ```rust
//! use c2r::db::{Web, Build, Function, Sample, Report};
//!
//! let mut web = Web::new();
//!
//! // Add any Build implementor to unified storage
//! let func = Function::new("main").returns("int").param("argc", "int");
//! web.add(&func);
//!
//! // Query by kind, name, or category
//! let functions = web.by_kind("Function");
//! ```

// Core tree structure
pub mod node;
pub mod tree;
pub mod query;
pub mod export;
pub mod format;

// Storage and databases
pub mod store;
pub mod annotation;
pub mod transform;
pub mod convert;
pub mod report;
pub mod config;
pub mod web;
pub mod json;
pub mod table;
pub mod token;
pub mod pattern;
pub mod sample;
pub mod validate;
pub mod keyword;
pub mod handler;
pub mod filter;

// Re-export core types
pub use node::*;
pub use tree::Tree;
pub use web::{BranchOp, Entry, Link, LinkKind, Links, Site, SiteStats};
pub use node::{Function, Struct, Enum, Param, Field, Variant, FunctionBuilder, StructBuilder, EnumBuilder};
// Re-export TreeBuilder as alias for backwards compatibility
pub use query::{Query, QuerySpec, AttrMatch};
pub use export::{to_json, to_rust};

// Re-export storage types
pub use store::{Store, Queryable, Record, HashStore, GroupedStore, PriorityStore, IndexedStore, StoreStats};
pub use annotation::{Annotation, AnnotationType, AnnotationLocation, Severity};
pub use transform::{Transform, TransformType, Relationship, RelationType};
pub use convert::{
    TypeCategory, TypeMetadata, TypeConverter, IdentifierCase,
    convert_identifier, to_snake_case, to_pascal_case, to_screaming_snake_case, to_camel_case,
    sanitize_rust_identifier, is_valid_c_identifier, is_c_keyword,
    is_operator, is_assignment_operator, is_binary_operator, convert_operator,
};
pub use report::{Report, format_report};
pub use format::{OutputFormat, Formatter, FormatConfig, FormatExt, Formatted, TreeChars};
pub use config::Config;
pub use table::{TableStyle, TableConfig, TableGenerator, to_table, tree_to_table, site_to_table};
pub use token::{TokenType, TokenSet, Tokenizer, TokenExt, TOKEN_KIND, token, token_typed, token_number, token_float, token_char, token_consumed, token_whitespace, token_comment, token_literal};
pub use pattern::{
    RuleType, LogicalOp, BranchPattern, BranchOperand, PatternRule, PatternStats,
    PatternCore, Pattern, PatternDef,
    PatternDB, Matcher, Edge, MachineNode, MachinePattern, PatternMatch, PatternMachine, PatternBuilder,
};
pub use sample::{
    SampleKind, SampleEdgeKind, SampleEdge, SampleNode, SamplePattern, SamplePatternStats,
    GenerationResult, MatchQuality, ConfidenceResult, SampleStats, Sample, SampleElement,
};
pub use web::{Build, Web, WebStats};
pub use keyword::{KeywordCategory, Keyword, Order, c_order, is_type_keyword, is_storage_class, is_type_qualifier, is_control_flow, is_preprocessor};
pub use handler::{
    CodeSegment, HandlerResult, HandlerStats, HandlerMetadata, 
    SuccessRecord, FailureRecord, SelectionStrategy, Selection,
    RoutingCondition, RoutingRule, PatternUsage, TokenRange,
};
pub use filter::{
    Kind, Category, KindFilter, CategoryFilter,
    KIND_STRINGS, CATEGORY_STRINGS,
    is_builtin_kind, is_builtin_category, builtin_kinds, builtin_categories,
};
