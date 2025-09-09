use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::path::{Path, PathBuf};

// Import all the types that Entry references
use crate::config::HandlerReport;
use crate::handler::Handler;
use crate::lock::Id;
use crate::pattern::Pattern;
use crate::table::{Table, TableCell};

#[derive(Debug, Clone)]
pub enum Entry {
    Id(Id),
    Val(u64),
    Str(String),
    Bool(bool),
    Func(fn(Entry) -> Entry),
    Path(PathBuf),
    Handler(Handler),
    HandlerReport(HandlerReport),
    Table(Table),
    TableCell(TableCell),
    Pair(Box<(Entry, Entry)>),
    List(Vec<Entry>),
    Any(Box<Entry>),
    IdMap(HashMap<Id, Entry>),
    ValMap(HashMap<u64, Entry>),
    StrMap(HashMap<String, Entry>),
    BoolMap(HashMap<bool, Entry>),
    FuncMap(HashMap<fn(Entry) -> Entry, Entry>),
    PathMap(HashMap<PathBuf, Entry>),
    HandlerMap(HashMap<String, Handler>),
    PairMap(HashMap<String, Box<(Entry, Entry)>>),
    AnyMap(HashMap<Entry, Entry>),
    Patternizer(Pattern),
}
impl Default for Entry {
    fn default() -> Self {
        Entry::Id(Id::default())
    }
}
impl Hash for Entry {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Entry::Id(id) => id.hash(state),
            Entry::Val(v) => v.hash(state),
            Entry::Str(s) => s.hash(state),
            Entry::Bool(b) => b.hash(state),
            Entry::Func(f) => f.hash(state),
            Entry::Path(p) => p.hash(state),
            Entry::Handler(h) => h.hash(state),
            Entry::HandlerReport(h) => h.hash(state),
            Entry::Table(t) => t.hash(state),
            Entry::TableCell(c) => c.hash(state),
            Entry::Pair(p) => p.hash(state),
            Entry::List(l) => l.hash(state),
            Entry::Any(a) => a.hash(state),
            Entry::IdMap(m) => m.iter().for_each(|(_k, v)| v.hash(state)),
            Entry::ValMap(m) => m.iter().for_each(|(_k, v)| v.hash(state)),
            Entry::StrMap(m) => m.iter().for_each(|(_k, v)| v.hash(state)),
            Entry::BoolMap(m) => m.iter().for_each(|(_k, v)| v.hash(state)),
            Entry::FuncMap(m) => m.iter().for_each(|(_k, v)| v.hash(state)),
            Entry::PathMap(m) => m.iter().for_each(|(_k, v)| v.hash(state)),
            Entry::HandlerMap(m) => m.iter().for_each(|(_k, v)| v.hash(state)),
            Entry::PairMap(m) => m.iter().for_each(|(_k, v)| v.hash(state)),
            Entry::AnyMap(m) => m.iter().for_each(|(_k, v)| v.hash(state)),
            Entry::Patternizer(p) => p.id.hash(state),
        }
    }
}
impl Entry {
    pub fn none() -> Self {
        Entry::Id(Id::get("none"))
    }
    pub fn get(id: Id) -> Entry {
        Entry::Id(id)
    }
    pub fn val(v: u64) -> Entry {
        Entry::Val(v)
    }
    pub fn str(s: &str) -> Entry {
        Entry::Str(s.to_string())
    }

    /// Create a HandlerReport entry
    pub fn handler_report(report: HandlerReport) -> Entry {
        Entry::HandlerReport(report)
    }
    pub fn bool(b: bool) -> Entry {
        Entry::Bool(b)
    }
    pub fn func(f: fn(Entry) -> Entry) -> Entry {
        Entry::Func(f)
    }
    pub fn path(p: &Path) -> Entry {
        Entry::Path(p.to_path_buf())
    }
    pub fn handler(h: Handler) -> Entry {
        Entry::Handler(h)
    }
    pub fn table(t: Table) -> Entry {
        Entry::Table(t)
    }
    pub fn pair(left: Entry, right: Entry) -> Entry {
        Entry::Pair(Box::new((left, right)))
    }
    pub fn list(entries: Vec<Entry>) -> Entry {
        Entry::List(entries)
    }
    pub fn id_map(entries: HashMap<Id, Entry>) -> Entry {
        Entry::IdMap(entries)
    }
    pub fn val_map(entries: HashMap<u64, Entry>) -> Entry {
        Entry::ValMap(entries)
    }
    pub fn str_map(entries: HashMap<String, Entry>) -> Entry {
        Entry::StrMap(entries)
    }
    pub fn bool_map(entries: HashMap<bool, Entry>) -> Entry {
        Entry::BoolMap(entries)
    }
    pub fn func_map(entries: HashMap<fn(Entry) -> Entry, Entry>) -> Entry {
        Entry::FuncMap(entries)
    }
    pub fn path_map(entries: HashMap<PathBuf, Entry>) -> Entry {
        Entry::PathMap(entries)
    }
    pub fn handler_map(map: HashMap<String, Handler>) -> Entry {
        Entry::HandlerMap(map)
    }
    pub fn pair_map(entries: HashMap<String, Box<(Entry, Entry)>>) -> Entry {
        Entry::PairMap(entries)
    }
    pub fn pattern(entry: Pattern) -> Entry {
        Entry::Patternizer(entry)
    }
    pub fn any(entry: Entry) -> Entry {
        Entry::Any(Box::new(entry))
    }

    /// Get the type name as a string - essential for Table integration
    pub fn type_name(&self) -> String {
        match self {
            Entry::Id(_) => "Id".to_string(),
            Entry::Val(_) => "Val".to_string(),
            Entry::Str(_) => "Str".to_string(),
            Entry::Bool(_) => "Bool".to_string(),
            Entry::Func(_) => "Func".to_string(),
            Entry::Path(_) => "Path".to_string(),
            Entry::Handler(_) => "Handler".to_string(),
            Entry::HandlerReport(_) => "HandlerReport".to_string(),
            Entry::Table(_) => "Table".to_string(),
            Entry::TableCell(_) => "TableCell".to_string(),
            Entry::Pair(_) => "Pair".to_string(),
            Entry::List(_) => "List".to_string(),
            Entry::Any(_) => "Any".to_string(),
            Entry::IdMap(_) => "IdMap".to_string(),
            Entry::ValMap(_) => "ValMap".to_string(),
            Entry::StrMap(_) => "StrMap".to_string(),
            Entry::BoolMap(_) => "BoolMap".to_string(),
            Entry::FuncMap(_) => "FuncMap".to_string(),
            Entry::PathMap(_) => "PathMap".to_string(),
            Entry::HandlerMap(_) => "HandlerMap".to_string(),
            Entry::PairMap(_) => "PairMap".to_string(),
            Entry::AnyMap(_) => "AnyMap".to_string(),
            Entry::Patternizer(_) => "Patternizer".to_string(),
        }
    }

    /// Get a concise display summary - essential for Table integration
    pub fn display_summary(&self) -> String {
        match self {
            Entry::Id(id) => id.name.clone(),
            Entry::Val(v) => v.to_string(),
            Entry::Str(s) => {
                if s.len() > 40 {
                    format!("{}...", &s[..37])
                } else {
                    s.clone()
                }
            }
            Entry::Bool(b) => b.to_string(),
            Entry::Func(_) => "Function".to_string(),
            Entry::Path(p) => format!("{}", p.display()),
            Entry::Handler(h) => format!("{}[{}]", h.role, h.priority),
            Entry::HandlerReport(r) => format!("{}:{}", r.handler_name, r.level),
            Entry::Table(t) => format!("Table({}×{})", t.rows(), t.columns()),
            Entry::TableCell(c) => format!("Cell@({},{})", c.row(), c.column()),
            Entry::Pair(p) => format!("({}, {})", p.0.display_summary(), p.1.display_summary()),
            Entry::List(l) => format!("List[{}]", l.len()),
            Entry::Any(_) => "Any".to_string(),
            Entry::IdMap(m) => format!("IdMap[{}]", m.len()),
            Entry::ValMap(m) => format!("ValMap[{}]", m.len()),
            Entry::StrMap(m) => format!("StrMap[{}]", m.len()),
            Entry::BoolMap(m) => format!("BoolMap[{}]", m.len()),
            Entry::FuncMap(m) => format!("FuncMap[{}]", m.len()),
            Entry::PathMap(m) => format!("PathMap[{}]", m.len()),
            Entry::HandlerMap(m) => format!("HandlerMap[{}]", m.len()),
            Entry::PairMap(m) => format!("PairMap[{}]", m.len()),
            Entry::AnyMap(m) => format!("AnyMap[{}]", m.len()),
            Entry::Patternizer(p) => p.name.clone(),
        }
    }

    /// Get size information for display - essential for Table integration
    pub fn size_info(&self) -> String {
        match self {
            Entry::Id(_) => "id".to_string(),
            Entry::Val(_) => "8B".to_string(),
            Entry::Str(s) => format!("{}B", s.len()),
            Entry::Bool(_) => "1B".to_string(),
            Entry::Func(_) => "8B".to_string(),
            Entry::Path(p) => format!("{}B", p.as_os_str().len()),
            Entry::Handler(_) => "handler".to_string(),
            Entry::HandlerReport(_) => "report".to_string(),
            Entry::Table(t) => format!("{}×{}", t.rows(), t.columns()),
            Entry::TableCell(c) => format!("{}ent", c.entries().len()),
            Entry::Pair(_) => "pair".to_string(),
            Entry::List(l) => format!("{}len", l.len()),
            Entry::Any(_) => "any".to_string(),
            Entry::IdMap(m) => format!("{}ids", m.len()),
            Entry::ValMap(m) => format!("{}vals", m.len()),
            Entry::StrMap(m) => format!("{}strs", m.len()),
            Entry::BoolMap(m) => format!("{}bools", m.len()),
            Entry::FuncMap(m) => format!("{}funcs", m.len()),
            Entry::PathMap(m) => format!("{}hdlrs", m.len()),
            Entry::HandlerMap(m) => format!("{}hdlrs", m.len()),
            Entry::PairMap(m) => format!("{}pairs", m.len()),
            Entry::AnyMap(m) => format!("{}any", m.len()),
            Entry::Patternizer(p) => format!("{}pats", p.token_patterns.len()),
        }
    }
}
impl PartialEq for Entry {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Entry::Id(left), Entry::Id(right)) => left == right,
            (Entry::Val(left), Entry::Val(right)) => left == right,
            (Entry::Str(left), Entry::Str(right)) => left == right,
            (Entry::Bool(left), Entry::Bool(right)) => left == right,
            (Entry::Func(left), Entry::Func(right)) => std::ptr::fn_addr_eq(*left, *right),
            (Entry::Path(left), Entry::Path(right)) => left == right,
            (Entry::Handler(left), Entry::Handler(right)) => left == right,
            (Entry::HandlerReport(left), Entry::HandlerReport(right)) => left == right,
            (Entry::Pair(left), Entry::Pair(right)) => left == right,
            (Entry::List(left), Entry::List(right)) => left == right,
            (Entry::IdMap(left), Entry::IdMap(right)) => left == right,
            (Entry::StrMap(left), Entry::StrMap(right)) => left == right,
            (Entry::BoolMap(left), Entry::BoolMap(right)) => left == right,
            (Entry::FuncMap(left), Entry::FuncMap(right)) => left == right,
            (Entry::PathMap(left), Entry::PathMap(right)) => left == right,
            (Entry::HandlerMap(left), Entry::HandlerMap(right)) => left == right,
            (Entry::PairMap(left), Entry::PairMap(right)) => left == right,
            (Entry::Any(left), Entry::Any(right)) => left == right,
            _ => false,
        }
    }
}
impl Eq for Entry {}

impl std::fmt::Display for Entry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Entry::Id(id) => write!(f, "Id({})", id),
            Entry::Val(v) => write!(f, "Val({})", v),
            Entry::Str(s) => write!(f, "Str({})", s),
            Entry::Bool(b) => write!(f, "Bool({})", b),
            Entry::Func(_) => write!(f, "Func(<function>)"),
            Entry::Path(p) => write!(f, "Path({})", p.display()),
            Entry::Handler(h) => write!(f, "Handler({})", h.id.name),
            Entry::HandlerReport(r) => write!(f, "Report({}: {})", r.handler_name, r.message),
            Entry::Table(t) => write!(
                f,
                "Table(Rows:{}, Columns:{}, Row Offset:{}, Column Offset:{} )",
                t.rows(),
                t.row_offset(),
                t.columns(),
                t.column_offset()
            ),
            Entry::TableCell(c) => write!(f, "TableCell({})", c.name()),
            Entry::Pair(p) => write!(f, "Pair({}, {})", p.0, p.1),
            Entry::List(l) => write!(f, "List[{}]", l.len()),
            Entry::Any(a) => write!(f, "Any({})", a),
            Entry::IdMap(m) => write!(f, "IdMap[{}]", m.len()),
            Entry::ValMap(m) => write!(f, "ValMap[{}]", m.len()),
            Entry::StrMap(m) => write!(f, "StrMap[{}]", m.len()),
            Entry::BoolMap(m) => write!(f, "BoolMap[{}]", m.len()),
            Entry::FuncMap(m) => write!(f, "FuncMap[{}]", m.len()),
            Entry::PathMap(m) => write!(f, "PathMap[{}]", m.len()),
            Entry::HandlerMap(m) => write!(f, "HandlerMap[{}]", m.len()),
            Entry::PairMap(m) => write!(f, "PairMap[{}]", m.len()),
            Entry::AnyMap(m) => write!(f, "AnyMap[{}]", m.len()),
            Entry::Patternizer(p) => write!(f, "Pattern({})", p.name.replace('\n', " | ")),
        }
    }
}
