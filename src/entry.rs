use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::path::{Path, PathBuf};

// Import all the types that Entry references
use crate::config::Report;
use crate::pattern::Pattern;
use crate::table::{Table, TableCell};
use crate::{handler::Handler, lock::Id};

pub enum Entry {
    Id(Id),
    Val(u64),
    Str(String),
    Bool(bool),
    Func(fn(Entry) -> Entry),
    Path(PathBuf),
    Handler(Box<dyn Handler>),
    HandlerReport(Report),
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
    HandlerMap(HashMap<String, Box<dyn Handler>>),
    PairMap(HashMap<String, Box<(Entry, Entry)>>),
    AnyMap(HashMap<Entry, Entry>),
    Pattern(Pattern),
    PatternRule(crate::pattern::PatternRule),
    PatternMetrics(crate::pattern::PatternMetrics),
    TokenPattern(crate::pattern::TokenPattern),
    SamplizerPattern(crate::sample::SamplizerPattern),
    SamplizerFilePair(crate::sample::FilePair),
    SamplizerValidation(crate::sample::ValidationResult),
    TokenList(Vec<crate::Token>),
    SamplizerStats(crate::sample::IntegrationStats),
}

impl std::fmt::Debug for Entry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Entry::Id(id) => f.debug_tuple("Id").field(id).finish(),
            Entry::Val(val) => f.debug_tuple("Val").field(val).finish(),
            Entry::Str(s) => f.debug_tuple("Str").field(s).finish(),
            Entry::Bool(b) => f.debug_tuple("Bool").field(b).finish(),
            Entry::Func(_) => f.debug_tuple("Func").field(&"<function>").finish(),
            Entry::Path(p) => f.debug_tuple("Path").field(p).finish(),
            Entry::Handler(_) => f.debug_tuple("Handler").field(&"<handler>").finish(),
            Entry::HandlerReport(hr) => f.debug_tuple("HandlerReport").field(hr).finish(),
            Entry::Table(t) => f.debug_tuple("Table").field(t).finish(),
            Entry::TableCell(tc) => f.debug_tuple("TableCell").field(tc).finish(),
            Entry::Pair(p) => f.debug_tuple("Pair").field(p).finish(),
            Entry::List(l) => f.debug_tuple("List").field(l).finish(),
            Entry::Any(a) => f.debug_tuple("Any").field(a).finish(),
            Entry::IdMap(im) => f.debug_tuple("IdMap").field(im).finish(),
            Entry::ValMap(vm) => f.debug_tuple("ValMap").field(vm).finish(),
            Entry::StrMap(sm) => f.debug_tuple("StrMap").field(sm).finish(),
            Entry::BoolMap(bm) => f.debug_tuple("BoolMap").field(bm).finish(),
            Entry::FuncMap(_) => f.debug_tuple("FuncMap").field(&"<function_map>").finish(),
            Entry::PathMap(pm) => f.debug_tuple("PathMap").field(pm).finish(),
            Entry::HandlerMap(_) => f.debug_tuple("HandlerMap").field(&"<handler_map>").finish(),
            Entry::PairMap(pm) => f.debug_tuple("PairMap").field(pm).finish(),
            Entry::AnyMap(am) => f.debug_tuple("AnyMap").field(am).finish(),
            Entry::Pattern(p) => f.debug_tuple("Patternizer").field(p).finish(),
            Entry::PatternRule(pr) => f.debug_tuple("PatternRule").field(pr).finish(),
            Entry::PatternMetrics(pm) => f.debug_tuple("PatternMetrics").field(pm).finish(),
            Entry::TokenPattern(tp) => f.debug_tuple("TokenPattern").field(tp).finish(),
            Entry::SamplizerPattern(sp) => f.debug_tuple("SamplizerPattern").field(sp).finish(),
            Entry::SamplizerFilePair(sfp) => f.debug_tuple("SamplizerFilePair").field(sfp).finish(),
            Entry::SamplizerValidation(sv) => {
                f.debug_tuple("SamplizerValidation").field(sv).finish()
            }
            Entry::SamplizerStats(ss) => f.debug_tuple("SamplizerStats").field(ss).finish(),
            Entry::TokenList(tokens) => f.debug_tuple("TokenList").field(&tokens.len()).finish(),
        }
    }
}

impl Clone for Entry {
    fn clone(&self) -> Self {
        match self {
            Entry::Id(id) => Entry::Id(id.clone()),
            Entry::Val(val) => Entry::Val(*val),
            Entry::Str(s) => Entry::Str(s.clone()),
            Entry::Bool(b) => Entry::Bool(*b),
            Entry::Func(f) => Entry::Func(*f),
            Entry::Path(p) => Entry::Path(p.clone()),
            Entry::Handler(_) => panic!("Cannot clone Handler trait objects"),
            Entry::HandlerReport(hr) => Entry::HandlerReport(hr.clone()),
            Entry::Table(t) => Entry::Table(t.clone()),
            Entry::TableCell(tc) => Entry::TableCell(tc.clone()),
            Entry::Pair(p) => Entry::Pair(p.clone()),
            Entry::List(l) => Entry::List(l.clone()),
            Entry::Any(a) => Entry::Any(a.clone()),
            Entry::IdMap(im) => Entry::IdMap(im.clone()),
            Entry::ValMap(vm) => Entry::ValMap(vm.clone()),
            Entry::StrMap(sm) => Entry::StrMap(sm.clone()),
            Entry::BoolMap(bm) => Entry::BoolMap(bm.clone()),
            Entry::FuncMap(fm) => Entry::FuncMap(fm.clone()),
            Entry::PathMap(pm) => Entry::PathMap(pm.clone()),
            Entry::HandlerMap(_) => panic!("Cannot clone HandlerMap with trait objects"),
            Entry::PairMap(pm) => Entry::PairMap(pm.clone()),
            Entry::AnyMap(am) => Entry::AnyMap(am.clone()),
            Entry::Pattern(p) => Entry::Pattern(p.clone()),
            Entry::PatternRule(pr) => Entry::PatternRule(pr.clone()),
            Entry::PatternMetrics(pm) => Entry::PatternMetrics(pm.clone()),
            Entry::TokenPattern(tp) => Entry::TokenPattern(tp.clone()),
            Entry::SamplizerPattern(sp) => Entry::SamplizerPattern(sp.clone()),
            Entry::SamplizerFilePair(sfp) => Entry::SamplizerFilePair(sfp.clone()),
            Entry::SamplizerValidation(sv) => Entry::SamplizerValidation(sv.clone()),
            Entry::SamplizerStats(ss) => Entry::SamplizerStats(ss.clone()),
            Entry::TokenList(tokens) => Entry::TokenList(tokens.clone()),
        }
    }
}

impl Default for Entry {
    fn default() -> Self {
        Entry::Id(Id::default())
    }
}

impl Hash for Entry {
    fn hash<H: Hasher>(&self, state: &mut H) {
        std::mem::discriminant(self).hash(state);
        match self {
            Entry::Id(id) => id.hash(state),
            Entry::Val(v) => v.hash(state),
            Entry::Str(s) => s.hash(state),
            Entry::Bool(b) => b.hash(state),
            Entry::Func(f) => f.hash(state),
            Entry::Path(p) => p.hash(state),
            Entry::Handler(h) => h.id().hash(state),
            Entry::HandlerReport(h) => h.hash(state),
            Entry::Table(t) => t.hash(state),
            Entry::TableCell(c) => c.hash(state),
            Entry::Pair(p) => p.hash(state),
            Entry::List(l) => l.hash(state),
            Entry::Any(a) => a.hash(state),
            Entry::IdMap(m) => {
                m.len().hash(state);
                for (k, v) in m.iter() {
                    k.hash(state);
                    v.hash(state);
                }
            }
            Entry::ValMap(m) => {
                m.len().hash(state);
                for (k, v) in m.iter() {
                    k.hash(state);
                    v.hash(state);
                }
            }
            Entry::StrMap(m) => {
                m.len().hash(state);
                for (k, v) in m.iter() {
                    k.hash(state);
                    v.hash(state);
                }
            }
            Entry::BoolMap(m) => {
                m.len().hash(state);
                for (k, v) in m.iter() {
                    k.hash(state);
                    v.hash(state);
                }
            }
            Entry::FuncMap(m) => {
                m.len().hash(state);
                for (k, v) in m.iter() {
                    k.hash(state);
                    v.hash(state);
                }
            }
            Entry::PathMap(m) => {
                m.len().hash(state);
                for (k, v) in m.iter() {
                    k.hash(state);
                    v.hash(state);
                }
            }
            Entry::HandlerMap(m) => {
                m.len().hash(state);
                for (k, v) in m.iter() {
                    k.hash(state);
                    v.id().hash(state);
                }
            }
            Entry::PairMap(m) => {
                m.len().hash(state);
                for (k, v) in m.iter() {
                    k.hash(state);
                    v.hash(state);
                }
            }
            Entry::AnyMap(m) => {
                m.len().hash(state);
                for (k, v) in m.iter() {
                    k.hash(state);
                    v.hash(state);
                }
            }
            Entry::Pattern(p) => p.id.hash(state),
            Entry::PatternRule(r) => r.pattern.hash(state),
            Entry::PatternMetrics(m) => {
                m.total_matches.hash(state);
                m.total_misses.hash(state);
            }
            Entry::TokenPattern(t) => t.hash(state),
            Entry::SamplizerPattern(p) => p.pattern_id.hash(state),
            Entry::SamplizerFilePair(f) => f.c_file_path.hash(state),
            Entry::SamplizerValidation(v) => v.pattern_id.hash(state),
            Entry::SamplizerStats(s) => s.patterns_generated.hash(state),
            Entry::TokenList(tokens) => tokens.hash(state),
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
    pub fn handler_report(report: Report) -> Entry {
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

    pub fn handler(h: Box<dyn Handler>) -> Entry {
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

    pub fn handler_map(map: HashMap<String, Box<dyn Handler>>) -> Entry {
        Entry::HandlerMap(map)
    }

    pub fn pair_map(entries: HashMap<String, Box<(Entry, Entry)>>) -> Entry {
        Entry::PairMap(entries)
    }

    pub fn pattern(entry: Pattern) -> Entry {
        Entry::Pattern(entry)
    }

    pub fn any(entry: Entry) -> Entry {
        Entry::Any(Box::new(entry))
    }

    /// Get the type name as a string - essential for Table integration
    pub fn type_name(&self) -> &'static str {
        match self {
            Entry::Id(_) => "Id",
            Entry::Val(_) => "Val",
            Entry::Str(_) => "Str",
            Entry::Bool(_) => "Bool",
            Entry::Func(_) => "Func",
            Entry::Path(_) => "Path",
            Entry::Handler(_) => "Handler",
            Entry::HandlerReport(_) => "HandlerReport",
            Entry::Table(_) => "Table",
            Entry::TableCell(_) => "TableCell",
            Entry::Pair(_) => "Pair",
            Entry::List(_) => "List",
            Entry::Any(_) => "Any",
            Entry::IdMap(_) => "IdMap",
            Entry::ValMap(_) => "ValMap",
            Entry::StrMap(_) => "StrMap",
            Entry::BoolMap(_) => "BoolMap",
            Entry::FuncMap(_) => "FuncMap",
            Entry::PathMap(_) => "PathMap",
            Entry::HandlerMap(_) => "HandlerMap",
            Entry::PairMap(_) => "PairMap",
            Entry::AnyMap(_) => "AnyMap",
            Entry::Pattern(_) => "Pattern",
            Entry::PatternRule(_) => "PatternRule",
            Entry::PatternMetrics(_) => "PatternMetrics",
            Entry::TokenPattern(_) => "TokenPattern",
            Entry::SamplizerPattern(_) => "SamplizerPattern",
            Entry::SamplizerFilePair(_) => "SamplizerFilePair",
            Entry::SamplizerValidation(_) => "SamplizerValidation",
            Entry::SamplizerStats(_) => "SamplizerStats",
            Entry::TokenList(_) => "TokenList",
        }
    }

    /// Get a concise display summary - essential for Table integration
    pub fn display_summary(&self) -> String {
        match self {
            Entry::Id(id) => id.name().to_string(),
            Entry::Val(v) => v.to_string(),
            Entry::Str(s) => format!("\"{}\"", s),
            Entry::Bool(b) => b.to_string(),
            Entry::Func(_) => "<function>".to_string(),
            Entry::Path(p) => p.display().to_string(),
            Entry::Handler(h) => h.id().name().to_string(),
            Entry::HandlerReport(r) => format!("{}: {}", r.handler_name(), r.message),
            Entry::Table(t) => format!("{}×{} table", t.rows(), t.columns()),
            Entry::TableCell(c) => c.to_string(),
            Entry::Pair(p) => format!("({}, {})", p.0.display_summary(), p.1.display_summary()),
            Entry::List(l) => match l.len() {
                0 => "[]".to_string(),
                1 => format!("[{}]", l[0].display_summary()),
                n => format!("[{}, ... +{}]", l[0].display_summary(), n - 1),
            },
            Entry::Any(a) => a.display_summary(),
            Entry::IdMap(m) => format!("{} ids", m.len()),
            Entry::ValMap(m) => format!("{} values", m.len()),
            Entry::StrMap(m) => format!("{} strings", m.len()),
            Entry::BoolMap(m) => format!("{} bools", m.len()),
            Entry::FuncMap(m) => format!("{} functions", m.len()),
            Entry::PathMap(m) => format!("{} paths", m.len()),
            Entry::HandlerMap(m) => format!("{} handlers", m.len()),
            Entry::PairMap(m) => format!("{} pairs", m.len()),
            Entry::AnyMap(m) => format!("{} entries", m.len()),
            Entry::Pattern(p) => p.name.replace('\n', " | "),
            Entry::PatternRule(r) => format!("rule: {:?}", r.pattern),
            Entry::PatternMetrics(m) => {
                format!("{} matches, {} misses", m.total_matches, m.total_misses)
            }
            Entry::TokenPattern(t) => format!("pattern: {:?}", t),
            Entry::SamplizerPattern(p) => p.pattern_name.replace('\n', " | "),
            Entry::SamplizerFilePair(f) => f.c_file_path.replace('\n', " | "),
            Entry::SamplizerValidation(v) => format!("validation: {}", v.pattern_id),
            Entry::SamplizerStats(s) => format!("{} patterns generated", s.patterns_generated),
            Entry::TokenList(tokens) => match tokens.len() {
                0 => "no tokens".to_string(),
                n => format!("{} tokens", n),
            },
        }
    }

    /// Get size information for display - essential for Table integration
    pub fn size_info(&self) -> String {
        match self {
            Entry::Id(_) => "ID".to_string(),
            Entry::Val(_) => "8B".to_string(),
            Entry::Str(s) => format!("{}B", s.len()),
            Entry::Bool(_) => "1B".to_string(),
            Entry::Func(_) => "8B".to_string(),
            Entry::Path(p) => format!("{}B", p.as_os_str().len()),
            Entry::Handler(_) => "Handler".to_string(),
            Entry::HandlerReport(_) => "Report".to_string(),
            Entry::Table(t) => format!("{}×{}", t.rows(), t.columns()),
            Entry::TableCell(c) => format!("{} entries", c.entries().len()),
            Entry::Pair(_) => "Pair".to_string(),
            Entry::List(l) => format!("{} items", l.len()),
            Entry::Any(_) => "Any".to_string(),
            Entry::IdMap(m) => format!("{} IDs", m.len()),
            Entry::ValMap(m) => format!("{} values", m.len()),
            Entry::StrMap(m) => format!("{} strings", m.len()),
            Entry::BoolMap(m) => format!("{} bools", m.len()),
            Entry::FuncMap(m) => format!("{} functions", m.len()),
            Entry::PathMap(m) => format!("{} paths", m.len()),
            Entry::HandlerMap(m) => format!("{} handlers", m.len()),
            Entry::PairMap(m) => format!("{} pairs", m.len()),
            Entry::AnyMap(m) => format!("{} entries", m.len()),
            Entry::Pattern(p) => format!("{} patterns", p.token_patterns.len()),
            Entry::PatternRule(_) => "1 rule".to_string(),
            Entry::PatternMetrics(m) => format!("{} total", m.total_matches + m.total_misses),
            Entry::TokenPattern(_) => "1 pattern".to_string(),
            Entry::SamplizerPattern(p) => format!(
                "{} rules",
                p.extraction_pattern.as_ref().map_or(0, |s| s.len())
            ),
            Entry::SamplizerFilePair(_) => "File pair".to_string(),
            Entry::SamplizerValidation(_) => "Validation".to_string(),
            Entry::SamplizerStats(s) => format!("{} generated", s.patterns_generated),
            Entry::TokenList(tokens) => format!("{} tokens", tokens.len()),
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
            (Entry::Handler(left), Entry::Handler(right)) => left.id() == right.id(),
            (Entry::HandlerReport(left), Entry::HandlerReport(right)) => left == right,
            (Entry::Table(left), Entry::Table(right)) => left == right,
            (Entry::TableCell(left), Entry::TableCell(right)) => left == right,
            (Entry::Pair(left), Entry::Pair(right)) => left == right,
            (Entry::List(left), Entry::List(right)) => left == right,
            (Entry::Any(left), Entry::Any(right)) => left == right,
            (Entry::IdMap(left), Entry::IdMap(right)) => left == right,
            (Entry::ValMap(left), Entry::ValMap(right)) => left == right,
            (Entry::StrMap(left), Entry::StrMap(right)) => left == right,
            (Entry::BoolMap(left), Entry::BoolMap(right)) => left == right,
            (Entry::FuncMap(left), Entry::FuncMap(right)) => left == right,
            (Entry::PathMap(left), Entry::PathMap(right)) => left == right,
            (Entry::HandlerMap(left), Entry::HandlerMap(right)) => {
                if left.len() != right.len() {
                    return false;
                }
                left.iter()
                    .all(|(k, v)| right.get(k).map_or(false, |rv| v.id() == rv.id()))
            }
            (Entry::PairMap(left), Entry::PairMap(right)) => left == right,
            (Entry::AnyMap(left), Entry::AnyMap(right)) => left == right,
            (Entry::Pattern(left), Entry::Pattern(right)) => left == right,
            (Entry::PatternRule(left), Entry::PatternRule(right)) => left == right,
            (Entry::PatternMetrics(left), Entry::PatternMetrics(right)) => left == right,
            (Entry::TokenPattern(left), Entry::TokenPattern(right)) => left == right,
            (Entry::SamplizerPattern(left), Entry::SamplizerPattern(right)) => left == right,
            (Entry::SamplizerFilePair(left), Entry::SamplizerFilePair(right)) => left == right,
            (Entry::SamplizerValidation(left), Entry::SamplizerValidation(right)) => left == right,
            (Entry::SamplizerStats(left), Entry::SamplizerStats(right)) => {
                left.patterns_generated == right.patterns_generated
            }
            (Entry::TokenList(left), Entry::TokenList(right)) => left == right,
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
            Entry::Handler(h) => write!(f, "Handler({})", h.id().name()),
            Entry::HandlerReport(r) => write!(f, "Report({}: {})", r.handler_name(), r.message),
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
            Entry::Pattern(p) => write!(f, "Pattern({})", p.name.replace('\n', " | ")),
            Entry::PatternRule(r) => write!(f, "PatternRule({:?})", r.pattern),
            Entry::PatternMetrics(m) => write!(
                f,
                "PatternMetrics(matches:{}, misses:{})",
                m.total_matches, m.total_misses
            ),
            Entry::TokenPattern(t) => write!(f, "TokenPattern({:?})", t),
            Entry::SamplizerPattern(p) => write!(
                f,
                "SamplizerPattern({})",
                p.pattern_name.replace('\n', " | ")
            ),
            Entry::SamplizerFilePair(fp) => write!(
                f,
                "SamplizerFilePair({})",
                fp.c_file_path.replace('\n', " | ")
            ),
            Entry::SamplizerValidation(v) => write!(
                f,
                "SamplizerValidation({})",
                v.pattern_id.name().replace('\n', " | ")
            ),
            Entry::SamplizerStats(s) => write!(f, "SamplizerStats({})", s.patterns_generated),
            Entry::TokenList(tokens) => write!(f, "TokenList[{}]", tokens.len()),
        }
    }
}
