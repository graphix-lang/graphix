//! Document and workspace symbols, read off a parse of each file so
//! they survive a failed check; the check adds the types.

use crate::{
    query::{display_type, in_file},
    state::ServerState,
    text::zero_based,
    uri::{path_to_uri, uri_to_path},
    workspace::{FileKind, summarize},
};
use arcstr::ArcStr;
use graphix_compiler::{
    SourcePosition,
    expr::{ExprKind, Name, SigKind, StructurePattern},
    typ::Type,
};
use lsp_types::{
    DocumentSymbol, Location, Position, Range, SymbolInformation, SymbolKind, Uri,
};
use std::{
    path::{Path, PathBuf},
    sync::Arc,
};

#[derive(Debug)]
pub(crate) struct Symbol {
    name: ArcStr,
    kind: SymbolKind,
    /// Where the declaration starts, where its name stands, and where it
    /// ends when that was written down (a .gxi item's is not).
    pos: SourcePosition,
    at: SourcePosition,
    end: Option<SourcePosition>,
}

fn sym(
    name: &Name,
    kind: SymbolKind,
    pos: SourcePosition,
    end: Option<SourcePosition>,
) -> Symbol {
    Symbol { name: name.name.clone(), kind, pos, at: name.pos_or(pos), end }
}

/// The top-level declarations of a parsed `.gxi`.
pub(crate) fn of_sig(sig: &graphix_compiler::expr::Sig) -> Vec<Symbol> {
    let item = |si: &graphix_compiler::expr::SigItem| match &si.kind {
        SigKind::Bind(b) if matches!(b.typ, Type::Fn(_)) => {
            Some(sym(&b.name, SymbolKind::FUNCTION, si.pos, None))
        }
        SigKind::Bind(b) => Some(sym(&b.name, SymbolKind::VARIABLE, si.pos, None)),
        SigKind::TypeDef(t) => {
            Some(sym(&t.name, SymbolKind::TYPE_PARAMETER, si.pos, None))
        }
        SigKind::Module(name) => Some(sym(name, SymbolKind::MODULE, si.pos, None)),
        SigKind::Trait(t) => Some(sym(&t.name, SymbolKind::INTERFACE, si.pos, None)),
        SigKind::Use { .. } | SigKind::Impl(_) => None,
    };
    sig.items.iter().filter_map(item).collect()
}

/// The top-level declarations of a parsed `.gx`.
pub(crate) fn of_exprs(exprs: &[graphix_compiler::expr::Expr]) -> Vec<Symbol> {
    let item = |e: &graphix_compiler::expr::Expr| {
        let end = e.end.get();
        match &e.kind {
            ExprKind::Bind(b) => match (&b.pattern, &b.value.kind) {
                (StructurePattern::Bind(name), ExprKind::Lambda(_)) => {
                    Some(sym(name, SymbolKind::FUNCTION, e.pos, end))
                }
                (StructurePattern::Bind(name), _) => {
                    Some(sym(name, SymbolKind::VARIABLE, e.pos, end))
                }
                (_, _) => None,
            },
            ExprKind::TypeDef(td) => {
                Some(sym(&td.name, SymbolKind::TYPE_PARAMETER, e.pos, end))
            }
            ExprKind::Module { name, .. } => {
                Some(sym(name, SymbolKind::MODULE, e.pos, end))
            }
            ExprKind::Trait(t) => Some(sym(&t.name, SymbolKind::INTERFACE, e.pos, end)),
            _ => None,
        }
    };
    exprs.iter().filter_map(item).collect()
}

/// The top-level declarations of an open buffer, a program unless it is
/// an interface; none when it does not parse.
fn declared(path: &Path, text: ArcStr) -> Arc<[Symbol]> {
    summarize(path, FileKind::of(path).unwrap_or(FileKind::Gx), text).1
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
        let (Some(doc), Some(file)) = (self.document(uri), uri_to_path(uri)) else {
            return vec![];
        };
        let text = doc.text.clone();
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
                    // the whole declaration, where its end was written down
                    range: Range {
                        start,
                        end: s
                            .end
                            .map(|e| self.encode(&text, zero_based(e)))
                            .unwrap_or(selection_range.end)
                            .max(selection_range.end),
                    },
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
            // an open buffer is parsed; every other file's symbols are the
            // scan's
            let (uri, text, symbols) = match self.documents.get(path) {
                Some(doc) => {
                    let text = doc.text.clone();
                    let symbols = declared(path, text.clone());
                    (doc.uri.clone(), text, symbols)
                }
                None => {
                    let Some(wf) = self.workspace.files.get(path) else { continue };
                    let Some(uri) = path_to_uri(path) else { continue };
                    (uri, wf.text.clone(), wf.symbols.clone())
                }
            };
            for s in symbols.iter() {
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
