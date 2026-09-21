//! Document and workspace symbols, read off a parse of each file so
//! they survive a failed check; the check adds the types.

use crate::{
    query::{display_type, in_file},
    state::ServerState,
    text::zero_based,
    uri::{path_to_uri, uri_to_path},
};
use arcstr::ArcStr;
use graphix_compiler::{
    SourcePosition,
    expr::{ExprKind, Name, Origin, SigKind, Source, StructurePattern, parser},
    typ::Type,
};
use lsp_types::{
    DocumentSymbol, Location, Position, Range, SymbolInformation, SymbolKind, Uri,
};
use std::path::{Path, PathBuf};

struct Symbol {
    name: ArcStr,
    kind: SymbolKind,
    /// Where the declaration starts, and where its name stands.
    pos: SourcePosition,
    at: SourcePosition,
}

/// The top-level declarations of a `.gx` or `.gxi` text; none when it
/// does not parse.
fn declared(path: &Path, text: ArcStr) -> Vec<Symbol> {
    let ori = Origin { parent: None, source: Source::File(path.to_path_buf()), text };
    let sym = |name: &Name, kind, pos| Symbol {
        name: name.name.clone(),
        kind,
        pos,
        at: name.pos_or(pos),
    };
    if path.extension().is_some_and(|e| e == "gxi") {
        let Ok(sig) = parser::parse_sig(ori) else { return vec![] };
        let item = |si: &graphix_compiler::expr::SigItem| match &si.kind {
            SigKind::Bind(b) if matches!(b.typ, Type::Fn(_)) => {
                Some(sym(&b.name, SymbolKind::FUNCTION, si.pos))
            }
            SigKind::Bind(b) => Some(sym(&b.name, SymbolKind::VARIABLE, si.pos)),
            SigKind::TypeDef(t) => Some(sym(&t.name, SymbolKind::TYPE_PARAMETER, si.pos)),
            SigKind::Module(name) => Some(sym(name, SymbolKind::MODULE, si.pos)),
            SigKind::Trait(t) => Some(sym(&t.name, SymbolKind::INTERFACE, si.pos)),
            SigKind::Use { .. } | SigKind::Impl(_) => None,
        };
        return sig.items.iter().filter_map(item).collect();
    }
    let Ok(exprs) = parser::parse(ori) else { return vec![] };
    let item = |e: &graphix_compiler::expr::Expr| match &e.kind {
        ExprKind::Bind(b) => match (&b.pattern, &b.value.kind) {
            (StructurePattern::Bind(name), ExprKind::Lambda(_)) => {
                Some(sym(name, SymbolKind::FUNCTION, e.pos))
            }
            (StructurePattern::Bind(name), _) => {
                Some(sym(name, SymbolKind::VARIABLE, e.pos))
            }
            (_, _) => None,
        },
        ExprKind::TypeDef(td) => Some(sym(&td.name, SymbolKind::TYPE_PARAMETER, e.pos)),
        ExprKind::Module { name, .. } => Some(sym(name, SymbolKind::MODULE, e.pos)),
        ExprKind::Trait(t) => Some(sym(&t.name, SymbolKind::INTERFACE, e.pos)),
        _ => None,
    };
    exprs.iter().filter_map(item).collect()
}

impl ServerState {
    /// The range of a symbol's name, LSP-encoded.
    fn name_range(&self, text: &str, s: &Symbol) -> Range {
        let start = zero_based(s.at);
        let len = s.name.chars().count() as u32;
        let end = Position { line: start.line, character: start.character + len };
        Range { start: self.encode(text, start), end: self.encode(text, end) }
    }

    pub fn document_symbols(&self, uri: &Uri) -> Vec<DocumentSymbol> {
        let (Some(doc), Some(file)) = (self.documents.get(uri), uri_to_path(uri)) else {
            return vec![];
        };
        let text = ArcStr::from(doc.text.as_str());
        let checked = self.checked_for(&file);
        let typ = |s: &Symbol| {
            let binds = checked.iter().flat_map(|c| c.ide.binds.iter());
            let mut binds = binds
                .filter(|b| in_file(&b.ori, &file) && b.pos == s.at && b.name == *s.name);
            binds.next().map(|b| display_type(&b.typ).to_string())
        };
        let symbols = declared(&file, text.clone());
        symbols
            .iter()
            .map(|s| {
                let selection_range = self.name_range(&text, s);
                let start =
                    self.encode(&text, zero_based(s.pos)).min(selection_range.start);
                #[allow(deprecated)]
                DocumentSymbol {
                    name: s.name.to_string(),
                    detail: typ(s),
                    kind: s.kind,
                    tags: None,
                    deprecated: None,
                    range: Range { start, end: selection_range.end },
                    selection_range,
                    children: None,
                }
            })
            .collect()
    }

    /// Symbols whose name contains `query`, case-insensitively, in the
    /// active document's project, else in every file.
    pub fn workspace_symbols(&self, query: &str) -> Vec<SymbolInformation> {
        let needle = query.to_ascii_lowercase();
        let project = self.last_active.as_ref().and_then(uri_to_path).and_then(|p| {
            let idx = self.workspace.file_to_projects.get(&p)?.first()?;
            Some(&self.workspace.projects[*idx])
        });
        let mut files: Vec<&PathBuf> = match project {
            Some(p) => p.files.iter().collect(),
            None => self.workspace.files.keys().collect(),
        };
        files.sort();
        let mut out = vec![];
        for path in files {
            let Some(uri) = path_to_uri(path) else { continue };
            let text = match self.documents.get(&uri) {
                Some(doc) => ArcStr::from(doc.text.as_str()),
                None => match std::fs::read_to_string(path) {
                    Ok(text) => ArcStr::from(text),
                    Err(_) => continue,
                },
            };
            for s in declared(path, text.clone()) {
                if !s.name.to_ascii_lowercase().contains(&needle) {
                    continue;
                }
                let range = self.name_range(&text, &s);
                #[allow(deprecated)]
                out.push(SymbolInformation {
                    name: s.name.to_string(),
                    kind: s.kind,
                    tags: None,
                    deprecated: None,
                    location: Location { uri: uri.clone(), range },
                    container_name: None,
                });
            }
        }
        out.sort_by(|a, b| a.name.cmp(&b.name));
        out
    }
}
