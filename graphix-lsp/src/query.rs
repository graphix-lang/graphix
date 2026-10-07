//! Hover, definition and references. Each resolves what the cursor is
//! on to a [`Target`] within one [`Checked`], then answers from the
//! check's side-channels. Nothing is answered by looking a word up: a
//! cursor on something the check did not record (a field, a tag, a
//! keyword) has no target.

use crate::{
    state::{Checked, ServerState},
    text::{covers, ident_at, zero_based},
    uri::{path_to_uri, uri_to_path},
};
use arcstr::ArcStr;
use compact_str::{CompactString, format_compact};
use graphix_compiler::{
    BindId, SourcePosition,
    env::{Bind, TraitDef, TypeDef},
    expr::{ModPath, Origin, Source},
    ide::ModuleRefSite,
    typ::Type,
};
use lsp_types::{
    Hover, HoverContents, Location, MarkupContent, MarkupKind, Position, Range, Uri,
};
use netidx_core::path::Path as NPath;
use std::path::Path;
use triomphe::Arc;

#[derive(Debug, Clone, PartialEq)]
enum Target {
    Bind(BindId),
    /// By canonical path.
    Module(ModPath),
    /// By the scope that defines it and its name there.
    Type(ModPath, CompactString),
}

/// One query: a cursor in a file, over the check that covers the file.
pub(crate) struct Query<'a> {
    state: &'a ServerState,
    checked: &'a Checked,
    file: &'a Path,
    cursor: Position,
    ident: String,
}

pub(crate) fn in_file(ori: &Origin, file: &Path) -> bool {
    matches!(&ori.source, Source::File(p) if p == file)
}

fn basename(path: &ModPath) -> &str {
    NPath::basename(&path.0).unwrap_or("")
}

/// `path` with its last `n` segments dropped.
fn truncated(path: &ModPath, n: usize) -> ModPath {
    let mut s: &str = &path.0;
    for _ in 0..n {
        s = NPath::dirname(s).unwrap_or("/");
    }
    ModPath(NPath::from(ArcStr::from(s)))
}

/// A binding's type for display. `replace_auto_constrained` folds a
/// function's constraint-table tvars into the surface and must run
/// before `resolve_tvars`, which empties the table.
pub(crate) fn display_type(typ: &Type) -> CompactString {
    let folded = match typ {
        Type::Fn(ft) => Type::Fn(Arc::new(ft.replace_auto_constrained())),
        t => t.clone(),
    };
    format_compact!("{}", folded.resolve_tvars())
}

fn markdown(code: &str, doc: Option<&ArcStr>) -> Hover {
    let mut value = format!("```graphix\n{code}\n```");
    if let Some(doc) = doc {
        value.push_str("\n\n");
        value.push_str(doc);
    }
    let contents = MarkupContent { kind: MarkupKind::Markdown, value };
    Hover { contents: HoverContents::Markup(contents), range: None }
}

impl<'a> Query<'a> {
    pub(crate) fn new(
        state: &'a ServerState,
        uri: &Uri,
        file: &'a Path,
        position: Position,
    ) -> Option<Self> {
        let checked = state.checked_for(file)?;
        let cursor = state.decode(uri, position);
        let ident = ident_at(&state.document(uri)?.text, cursor)?;
        Some(Self { state, checked, file, cursor, ident })
    }

    /// The `len` characters written at `at`.
    fn location(&self, ori: &Origin, at: Position, len: usize) -> Option<Location> {
        let Source::File(path) = &ori.source else { return None };
        let end = Position { line: at.line, character: at.character + len as u32 };
        let range = Range {
            start: self.state.encode(&ori.text, at),
            end: self.state.encode(&ori.text, end),
        };
        Some(Location { uri: path_to_uri(path)?, range })
    }

    /// Each segment of the path `name` written at `pos`: where it stands,
    /// and its length.
    fn segments(pos: SourcePosition, name: &ModPath) -> Vec<(Position, usize)> {
        let start = zero_based(pos);
        let mut col = start.character;
        format_compact!("{name}")
            .split("::")
            .map(|seg| {
                let len = seg.chars().count();
                let at = Position { line: start.line, character: col };
                col += len as u32 + 2;
                (at, len)
            })
            .collect()
    }

    /// Each segment of a `use` item that stands in this statement,
    /// with where it stands and the canonical path it names.
    fn use_segments(
        m: &'a ModuleRefSite,
    ) -> impl Iterator<Item = (&'a str, Position, ModPath)> + 'a {
        let at = m.segments.iter().flat_map(|w| w.0.iter());
        let n = NPath::levels(&m.name.0);
        NPath::parts(&m.name.0).zip(at).enumerate().map(move |(i, (seg, at))| {
            (seg, zero_based(*at), truncated(&m.canonical, n - 1 - i))
        })
    }

    fn bind(&self, id: BindId) -> Option<&'a Bind> {
        let Checked { env, ide } = self.checked;
        env.by_id
            .get(&id)
            .map(|b| &**b)
            .or_else(|| ide.binds.iter().rev().find(|b| b.id == id))
    }

    fn typedef(&self, scope: &ModPath, name: &str) -> Option<&'a TypeDef> {
        self.checked.env.typedefs.get(scope).and_then(|defs| defs.get(name))
    }

    /// The trait whose module-like scope is `path`, if one is.
    fn trait_at(&self, path: &ModPath) -> Option<&'a TraitDef> {
        let env = &self.checked.env;
        let id = env.traits.get(&truncated(path, 1))?.get(basename(path))?;
        env.trait_def(*id).map(|d| &**d)
    }

    fn trait_location(&self, def: &TraitDef) -> Option<Location> {
        self.location(&def.ori, zero_based(def.pos), def.name.chars().count())
    }

    /// What a canonical path names: a value before a type before a
    /// module (`tui::text` is a function and its module).
    fn named(&self, canonical: &ModPath) -> Option<Target> {
        let env = &self.checked.env;
        let scope = truncated(canonical, 1);
        let name = basename(canonical);
        if let Some(id) = env.binds.get(&scope).and_then(|b| b.get(name)) {
            return Some(Target::Bind(*id));
        }
        if self.typedef(&scope, name).is_some() {
            return Some(Target::Type(scope, name.into()));
        }
        env.modules.contains(canonical).then(|| Target::Module(canonical.clone()))
    }

    /// The target under the cursor and the name it is written by.
    fn target(&self) -> Option<(Target, CompactString)> {
        let Checked { env, ide } = self.checked;
        // The path `name`, written at `pos`, when the cursor is on one of its
        // segments: the written path, and how many segments follow the one
        // under the cursor.
        let on = |ori: &Origin, pos: SourcePosition, name: &ModPath| {
            let written = format_compact!("{name}");
            let start = zero_based(pos);
            if !in_file(ori, self.file)
                || !covers(start, written.chars().count(), self.cursor)
            {
                return None;
            }
            let off = (self.cursor.character - start.character) as usize;
            let segs: Vec<&str> = written.split("::").collect();
            let mut at = 0;
            for (i, seg) in segs.iter().enumerate() {
                let len = seg.chars().count();
                if (at..=at + len).contains(&off) {
                    let after = segs.len() - 1 - i;
                    return (*seg == self.ident).then(|| (written.clone(), after));
                }
                at += len + 2;
            }
            None
        };
        // a segment before the last names the module of its prefix: the
        // item's scope less the segments after it
        let module =
            |scope: &ModPath, after: usize| Target::Module(truncated(scope, after - 1));
        for r in ide.references.iter() {
            if let Some((written, after)) = on(&r.ori, r.pos, &r.name) {
                if after == 0 {
                    return Some((Target::Bind(r.bind_id), written));
                }
                let scope = &self.bind(r.bind_id)?.scope;
                return Some((module(scope, after), (&*self.ident).into()));
            }
        }
        for t in ide.type_refs.iter() {
            if let Some((written, after)) = on(&t.ori, t.pos, &t.name) {
                if after == 0 {
                    let name = basename(&t.name).into();
                    return Some((
                        Target::Type(t.canonical_scope.clone(), name),
                        written,
                    ));
                }
                return Some((module(&t.canonical_scope, after), (&*self.ident).into()));
            }
        }
        let here = |at: Position| covers(at, self.ident.chars().count(), self.cursor);
        for m in ide.module_references.iter().filter(|m| in_file(&m.ori, self.file)) {
            if m.segments.is_none() {
                if basename(&m.name) == self.ident && here(zero_based(m.pos)) {
                    let written = (&*self.ident).into();
                    return Some((Target::Module(m.canonical.clone()), written));
                }
                continue;
            }
            for (seg, at, canonical) in Self::use_segments(m) {
                if seg == self.ident && here(at) {
                    return self.named(&canonical).map(|t| (t, seg.into()));
                }
            }
        }
        for b in ide.binds.iter() {
            if b.name == self.ident
                && in_file(&b.ori, self.file)
                && here(zero_based(b.pos))
            {
                return Some((Target::Bind(b.id), b.name.clone()));
            }
        }
        // a trait is the module its methods are in
        for (_, defs) in &env.traits {
            if let Some(id) = defs.get(&*self.ident)
                && let Some(def) = env.trait_def(*id)
                && in_file(&def.ori, self.file)
                && here(zero_based(def.pos))
            {
                return Some((
                    Target::Module(def.path.clone()),
                    def.name.as_str().into(),
                ));
            }
        }
        for (scope, defs) in &env.typedefs {
            if let Some(td) = defs.get(&*self.ident)
                && in_file(td.ori(), self.file)
                && here(zero_based(td.pos()))
            {
                let name: CompactString = (&*self.ident).into();
                return Some((Target::Type(scope.clone(), name.clone()), name));
            }
        }
        None
    }

    /// The field selected under the cursor (`s.f`), with its type.
    fn field(&self) -> Option<Hover> {
        let fields = self.checked.ide.field_refs.iter();
        let mut here = fields.filter(|f| {
            in_file(&f.ori, self.file)
                && f.name == self.ident
                && covers(zero_based(f.pos), self.ident.chars().count(), self.cursor)
        });
        let f = here.next()?;
        Some(markdown(&format_compact!("{}: {}", f.name, display_type(&f.typ)), None))
    }

    pub(crate) fn hover(&self) -> Option<Hover> {
        if let Some(field) = self.field() {
            return Some(field);
        }
        let (target, written) = self.target()?;
        Some(match target {
            Target::Bind(id) => {
                let bind = self.bind(id)?;
                let code = format_compact!("{written}: {}", display_type(&bind.typ));
                markdown(&code, bind.doc.as_ref())
            }
            Target::Type(scope, name) => {
                let td = self.typedef(&scope, &name)?;
                markdown(
                    &format_compact!("type {written} = {}", td.typ()),
                    td.doc.as_ref(),
                )
            }
            Target::Module(canonical) => match self.trait_at(&canonical) {
                Some(def) => {
                    let mut code = format_compact!("trait {} {{\n", def.name);
                    for m in def.methods.iter() {
                        code.push_str(&format_compact!(
                            "    val {}: {};\n",
                            m.name,
                            m.typ
                        ));
                    }
                    code.push('}');
                    markdown(&code, def.doc.as_ref())
                }
                None => markdown(&format_compact!("mod {canonical}"), None),
            },
        })
    }

    /// The other side of a `.gxi` `val` ↔ `.gx` `let` pair.
    fn sig_partner(&self, id: BindId) -> Option<BindId> {
        self.checked.ide.sig_links.iter().find_map(|l| {
            if l.sig_id == id {
                Some(l.impl_id)
            } else if l.impl_id == id {
                Some(l.sig_id)
            } else {
                None
            }
        })
    }

    fn bind_location(&self, id: BindId) -> Option<Location> {
        let b = self.bind(id)?;
        self.location(&b.ori, zero_based(b.pos), b.name.chars().count())
    }

    fn type_location(&self, scope: &ModPath, name: &str) -> Option<Location> {
        let td = self.typedef(scope, name)?;
        self.location(td.ori(), zero_based(td.pos()), name.chars().count())
    }

    /// A reference goes to its declaration; an interface `val` goes to
    /// its implementation.
    pub(crate) fn definition(&self) -> Option<Location> {
        match self.target()?.0 {
            Target::Bind(id) => {
                let decl = self.bind_location(id);
                let here = decl.as_ref().is_some_and(|l| {
                    uri_to_path(&l.uri).as_deref() == Some(self.file)
                        && l.range.start.line == self.cursor.line
                });
                match self.sig_partner(id) {
                    Some(partner) if here => self.bind_location(partner).or(decl),
                    Some(_) | None => decl,
                }
            }
            Target::Type(scope, name) => self.type_location(&scope, &name),
            Target::Module(canonical) => {
                if let Some(def) = self.trait_at(&canonical) {
                    return self.trait_location(def);
                }
                let ide = &self.checked.ide;
                let ori = ide.module_references.iter().find_map(|m| {
                    (m.canonical == canonical).then_some(m.def_ori.as_ref()).flatten()
                })?;
                self.location(ori, Position::default(), 0)
            }
        }
    }

    pub(crate) fn references(&self, include_declaration: bool) -> Vec<Location> {
        let Some((target, _)) = self.target() else { return vec![] };
        let ide = &self.checked.ide;
        let mut out: Vec<Location> = vec![];
        let mut targets = vec![target.clone()];
        if let Target::Bind(id) = target {
            targets.extend(self.sig_partner(id).map(Target::Bind));
        }
        // a path's last segment names its item, each segment before it the
        // module of its prefix: the item's scope less the segments after it
        let mut path =
            |ori: &Origin, pos, name: &ModPath, item: Target, scope: &ModPath| {
                let segs = Self::segments(pos, name);
                let n = segs.len();
                for (i, (at, len)) in segs.into_iter().enumerate() {
                    let t = match n - 1 - i {
                        0 => item.clone(),
                        after => Target::Module(truncated(scope, after - 1)),
                    };
                    if targets.contains(&t) {
                        out.extend(self.location(ori, at, len));
                    }
                }
            };
        for r in ide.references.iter() {
            let Some(b) = self.bind(r.bind_id) else { continue };
            path(&r.ori, r.pos, &r.name, Target::Bind(r.bind_id), &b.scope);
        }
        for t in ide.type_refs.iter() {
            let item = Target::Type(t.canonical_scope.clone(), basename(&t.name).into());
            path(&t.ori, t.pos, &t.name, item, &t.canonical_scope);
        }
        for m in ide.module_references.iter() {
            if m.segments.is_none() {
                if targets.contains(&Target::Module(m.canonical.clone())) {
                    let len = basename(&m.name).chars().count();
                    out.extend(self.location(&m.ori, zero_based(m.pos), len));
                }
                continue;
            }
            for (seg, at, canonical) in Self::use_segments(m) {
                if self.named(&canonical).is_some_and(|t| targets.contains(&t)) {
                    out.extend(self.location(&m.ori, at, seg.chars().count()));
                }
            }
        }
        if include_declaration {
            for t in &targets {
                out.extend(match t {
                    Target::Bind(id) => self.bind_location(*id),
                    Target::Type(scope, name) => self.type_location(scope, name),
                    Target::Module(m) => {
                        self.trait_at(m).and_then(|d| self.trait_location(d))
                    }
                });
            }
        }
        out.sort_by(|a, b| {
            (a.uri.as_str(), a.range.start.line, a.range.start.character).cmp(&(
                b.uri.as_str(),
                b.range.start.line,
                b.range.start.character,
            ))
        });
        out.dedup();
        out
    }
}

impl ServerState {
    pub fn hover(&self, uri: &Uri, position: Position) -> Option<Hover> {
        let file = uri_to_path(uri)?;
        Query::new(self, uri, &file, position)?.hover()
    }

    pub fn definition(&self, uri: &Uri, position: Position) -> Option<Location> {
        let file = uri_to_path(uri)?;
        Query::new(self, uri, &file, position)?.definition()
    }

    pub fn references(
        &self,
        uri: &Uri,
        position: Position,
        include_declaration: bool,
    ) -> Vec<Location> {
        let Some(file) = uri_to_path(uri) else { return vec![] };
        match Query::new(self, uri, &file, position) {
            Some(q) => q.references(include_declaration),
            None => vec![],
        }
    }
}
