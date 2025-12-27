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
pub mod export;
pub mod format;
pub mod node;
pub mod query;
pub mod tree;

// Storage and databases
pub mod annotation;
pub mod config;
pub mod convert;
pub mod filter;
pub mod handler;
pub mod json;
pub mod keyword;
pub mod pattern;
pub mod report;
pub mod routing;
pub mod sample;
pub mod store;
pub mod table;
pub mod token;
pub mod transform;
pub mod validate;
pub mod web;

// Re-export TreeBuilder as alias for backwards compatibility
pub use export::{to_json, to_rust};
// Re-export core types
pub use node::*;
pub use node::{
    Enum, EnumBuilder, Field, Function, FunctionBuilder, Param, Struct, StructBuilder, Variant,
};
pub use query::{AttrMatch, Query, QuerySpec};
pub use tree::Tree;
pub use web::{BranchOp, Entry, Link, LinkKind, Links, Site, SiteStats};

// Re-export storage types
pub use annotation::{Annotation, AnnotationLocation, AnnotationType, Severity};
pub use config::Config;
pub use convert::{
    convert_identifier, convert_operator, is_assignment_operator, is_binary_operator, is_c_keyword,
    is_operator, is_valid_c_identifier, sanitize_rust_identifier, to_camel_case, to_pascal_case,
    to_screaming_snake_case, to_snake_case, IdentifierCase, TypeCategory,
    TypeConverter, TypeMetadata,
};
pub use filter::{
    builtin_categories, builtin_kinds, is_builtin_category, is_builtin_kind, Category, CategoryFilter, Kind,
    KindFilter, CATEGORY_STRINGS, KIND_STRINGS,
};
pub use format::{FormatConfig, FormatExt, Formatted, Formatter, OutputFormat, TreeChars};
pub use handler::{
    CodeSegment, FailureRecord, HandlerMetadata, HandlerResult, HandlerStats, PatternUsage,
    RoutingCondition, RoutingRule, Selection, SelectionStrategy, SuccessRecord, TokenRange,
};
pub use keyword::{
    c_order, is_control_flow, is_preprocessor, is_storage_class, is_type_keyword, is_type_qualifier, Keyword,
    KeywordCategory, Order,
};
pub use pattern::{
    Edge, LogicalOp, MachineNode, MachinePattern, MatchResult, MatchSegment, Matcher, Pattern,
    PatternBuilder, PatternMachine, PatternMatch, PatternRule, PatternStats, RuleType,
};
pub use report::{format_report, Report};
pub use routing::{
    create_routing, get_routing_entry, get_routing_result, pending_routings_for, routing_stats, set_routing_failed,
    set_routing_result, RoutingDecision, RoutingResult, RoutingStats,
    RoutingStatus,
};
pub use sample::{
    ConfidenceResult, GenerationResult, MatchQuality, Sample, SampleEdge, SampleEdgeKind,
    SampleElement, SampleKind, SampleNode, SamplePattern, SamplePatternStats, SampleStats,
};
pub use store::{
    GroupedStore, HashStore, IndexedStore, PriorityStore, Queryable, Record, Store, StoreStats,
};
pub use table::{site_to_table, to_table, tree_to_table, TableConfig, TableGenerator, TableStyle};
pub use token::{
    token, token_char, token_comment, token_consumed, token_float, token_literal, token_number, token_typed,
    token_whitespace, Token, TokenSet, TokenType, TokenValue, Tokenizer,
    TOKEN_KIND,
};
pub use transform::{RelationType, Relationship, Transform, TransformType};
pub use web::{Build, Web, WebStats};
