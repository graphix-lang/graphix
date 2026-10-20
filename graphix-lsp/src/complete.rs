//! Completion: the labels of the call being written, the fields of the
//! value being read, else every name visible at the cursor.

use crate::{
    query::{display_type, in_file, typedef_decl},
    state::{Checked, ServerState},
    text::{
        Typed, UsePath, call_context, label_start, text_between, typed_before, use_path,
        zero_based,
    },
    uri::uri_to_path,
};
use ahash::AHashSet;
use compact_str::CompactString;
use graphix_compiler::{
    env::{Bind, Env},
    expr::ModPath,
    ide::ScopeMapEntry,
    typ::{FnArgKind, FnType, Type},
};
use lsp_types::{
    CompletionItem, CompletionItemKind, CompletionTextEdit, Documentation,
    InsertTextFormat, Position, Range, TextEdit, Uri,
};
use netidx_core::path::Path as NPath;
use poolshark::local::LPooled;
use std::{cmp::Reverse, fmt::Write, path::Path};

/// A path as typed (`array::ma`, `array::`) for `lookup_matching`; a
/// trailing `::` is an empty last segment.
fn typed_path(s: &str) -> ModPath {
    if s.is_empty() { ModPath::root() } else { s.split("::").collect() }
}

fn is_under(scope: &str, ancestor: &str) -> bool {
    ancestor == "/"
        || scope == ancestor
        || scope.strip_prefix(ancestor).is_some_and(|rest| rest.starts_with('/'))
}

/// A snippet calling `name`: a placeholder per required argument, the
/// cursor after the parens.
fn call_snippet(name: &str, fnt: &FnType) -> String {
    let mut body = format!("{name}(");
    let mut n = 0;
    for arg in fnt.args.iter() {
        let sep = if n == 0 { "" } else { ", " };
        match &arg.kind {
            FnArgKind::Labeled { has_default: true, .. } => continue,
            FnArgKind::Labeled { name, has_default: false } => {
                let _ = write!(body, "{sep}#{name}: ${{{}:{name}}}", n + 1);
            }
            FnArgKind::Positional { name: Some(name) } => {
                let _ = write!(body, "{sep}${{{}:{name}}}", n + 1);
            }
            FnArgKind::Positional { name: None } => {
                let _ = write!(body, "{sep}${{{}:a{n}}}", n + 1);
            }
        }
        n += 1;
    }
    body.push_str(")$0");
    body
}

struct Completer<'a> {
    state: &'a ServerState,
    env: &'a Env,
    checked: Option<&'a Checked>,
    file: &'a Path,
    cursor: Position,
    scope: ModPath,
}

impl<'a> Completer<'a> {
    /// The scope the cursor stands in. An entry's scope is the one its
    /// expression stands in, so inside the innermost expression around
    /// the cursor the statement that last ended before the cursor has
    /// the scope a new one would get; with none, the enclosing
    /// expression's own.
    fn scope_at(checked: Option<&Checked>, file: &Path, cursor: Position) -> ModPath {
        let entries = || {
            let all = checked.iter().flat_map(|c| c.ide.scope_map.iter());
            all.filter(|e| in_file(&e.ori, file))
                .filter_map(|e| Some((zero_based(e.pos), zero_based(e.end?), &e.scope)))
        };
        let around = entries()
            .filter(|(pos, end, _)| *pos <= cursor && cursor <= *end)
            .max_by_key(|(pos, end, _)| (*pos, Reverse(*end)));
        let floor = around.map(|(pos, _, _)| pos).unwrap_or_default();
        // XCR claude for claude: [bug/medium] An entry the parser did not write (every
        // seq-desugared expr) has end (0,0), so it passes `*end <= cursor` wherever it
        // stands, even after the cursor, and is taken as the statement before the
        // cursor. Its scope then hides every block local above a later `seq` in the
        // same function: no `inner_local` at `let r = { let inner_local = q + 1;
        // inner_local }` followed by `seq q { .. }`, and nothing at `l.` in
        // netidx-admin's tui/local.gx. Make ScopeMapEntry::end an Option (as
        // ExprTypeSite::end is), push `spec.end.get()` in compile_inner, and skip
        // end-less entries here and in `defining`. probe:
        // design/review-2026-10-19/repro/x-lsp-probe-seq-hides-block-locals.py
        // (x-lsp-probe-01)
        // 2026-10-19 claude: Left for the main session: the fix is ScopeMapEntry::end
        // as an Option (graphix-types/src/ide.rs) pushed from spec.end.get() in
        // compile_inner (graphix-compiler/src/node/compiler.rs); this agent was scoped
        // out of both crates. The LSP side then skips end-less entries in scope_at and
        // defining.
        // 2026-10-19 claude: ScopeMapEntry::end is an Option, pushed from
        // spec.end.get(); scope_at and defining skip an entry with no written end (a
        // lowering's). Pin: graphix-shell/tests/lsp
        // completion_keeps_block_locals_above_a_later_seq (fails with the end-less
        // entries read as (0,0)).
        let before = entries()
            .filter(|(pos, end, _)| {
                floor <= *pos && *end <= cursor && Some(*pos) != around.map(|a| a.0)
            })
            .max_by_key(|(pos, end, _)| (*end, Reverse(*pos)));
        let scope = before.or(around).map(|(_, _, scope)| scope.clone());
        scope.unwrap_or_else(|| Self::module_of(checked, file))
    }

    /// The module a file is the body of: the shallowest scope anything
    /// in it stands in. An interface has no expressions, only binds.
    fn module_of(checked: Option<&Checked>, file: &Path) -> ModPath {
        let ide = checked.iter().map(|c| &c.ide);
        let scopes = ide.flat_map(|ide| {
            let entries = ide.scope_map.iter().map(|e| (&e.ori, &e.scope));
            entries.chain(ide.binds.iter().map(|b| (&b.ori, &b.scope)))
        });
        let here = scopes.filter(|(ori, _)| in_file(ori, file)).map(|(_, scope)| scope);
        here.min_by_key(|scope| scope.len()).cloned().unwrap_or_else(ModPath::root)
    }

    /// Every binding declared before the cursor in a scope enclosing
    /// it, innermost and latest first.
    fn locals(&self) -> impl Iterator<Item = &'a Bind> {
        let binds = self.checked.into_iter().flat_map(|c| c.ide.binds.iter().rev());
        let defining = self.defining();
        binds.filter(move |b| {
            in_file(&b.ori, self.file)
                && zero_based(b.pos) <= self.cursor
                && is_under(&self.scope, &b.scope)
                && !defining(b)
        })
    }

    /// Whether a binding of this file is not in scope yet at the cursor:
    /// it is in its own statement, which binds its names after its value
    /// (a `let rec` before).
    fn defining(&self) -> impl Fn(&Bind) -> bool + 'a {
        let entries = self.checked.into_iter().flat_map(|c| c.ide.scope_map.iter());
        let cursor = self.cursor;
        let around: LPooled<Vec<&'a ScopeMapEntry>> = entries
            .filter(|e| {
                in_file(&e.ori, self.file)
                    && zero_based(e.pos) <= cursor
                    && e.end.is_some_and(|end| cursor <= zero_based(end))
            })
            .collect();
        move |b| {
            let at = zero_based(b.pos);
            around.iter().any(|e| {
                let start = zero_based(e.pos);
                e.scope == b.scope
                    && start <= at
                    && !text_between(&e.ori.text, start, at)
                        .split_whitespace()
                        .any(|w| w == "rec")
            })
        }
    }

    fn lookup(&self, name: &str) -> Option<&'a Bind> {
        self.locals().find(|b| b.name == name).or_else(|| {
            let found = self.env.lookup_bind(&self.scope, &typed_path(name));
            found.ok().flatten().map(|(_, b)| b)
        })
    }

    fn labels(&self, callee: &str, replace: Option<Range>) -> Vec<CompletionItem> {
        let Some(Type::Fn(fnt)) = self.lookup(callee).map(|b| &b.typ) else {
            return vec![];
        };
        let labels = fnt.args.iter().filter_map(|arg| Some((arg.label()?, arg)));
        labels
            .map(|(label, arg)| {
                let insert = format!("#{label}: ");
                CompletionItem {
                    label: format!("#{label}"),
                    kind: Some(CompletionItemKind::FIELD),
                    detail: Some(display_type(&arg.typ).into()),
                    insert_text: replace.is_none().then(|| insert.clone()),
                    text_edit: replace.map(|range| {
                        CompletionTextEdit::Edit(TextEdit { range, new_text: insert })
                    }),
                    ..Default::default()
                }
            })
            .collect()
    }

    fn fields(&self, typed: &Typed) -> Vec<CompletionItem> {
        let Some((first, rest)) = typed.receiver.split_first() else { return vec![] };
        let Some(bind) = self.lookup(first) else { return vec![] };
        let fields_of = |typ: &Type| match &typ.resolve_tvars() {
            Type::Struct(fields) => Some(fields.clone()),
            t @ Type::Ref(_) => match &t.lookup_ref(self.env) {
                Ok(Type::Struct(fields)) => Some(fields.clone()),
                Ok(_) | Err(_) => None,
            },
            _ => None,
        };
        let mut fields = fields_of(&bind.typ);
        for name in rest {
            let field = fields.iter().flat_map(|f| f.iter()).find(|(n, _, _)| n == name);
            fields = field.and_then(|(_, typ, _)| fields_of(typ));
        }
        let fields = fields.iter().flat_map(|f| f.iter());
        fields
            .filter(|(name, _, _)| name.starts_with(&typed.path))
            .map(|(name, typ, _)| CompletionItem {
                label: name.to_string(),
                kind: Some(CompletionItemKind::FIELD),
                detail: Some(display_type(typ).into()),
                ..Default::default()
            })
            .collect()
    }

    fn bind_item(&self, name: &str, bind: &Bind) -> CompletionItem {
        let snippet = match &bind.typ {
            Type::Fn(fnt) if self.state.snippet_support => Some(call_snippet(name, fnt)),
            _ => None,
        };
        CompletionItem {
            label: name.to_string(),
            kind: Some(match &bind.typ {
                Type::Fn(_) => CompletionItemKind::FUNCTION,
                _ => CompletionItemKind::VARIABLE,
            }),
            detail: Some(display_type(&bind.typ).into()),
            documentation: bind
                .doc
                .as_ref()
                .map(|d| Documentation::String(d.to_string())),
            insert_text_format: snippet.as_ref().map(|_| InsertTextFormat::SNIPPET),
            insert_text: snippet,
            ..Default::default()
        }
    }

    /// What the module a `use` path stands under exports; at the head
    /// of a path, the modules in reach and the path keywords.
    fn imports(&self, at: &UsePath) -> Vec<CompletionItem> {
        let item = |label: &str, kind, detail: Option<CompactString>| CompletionItem {
            label: label.to_string(),
            kind: Some(kind),
            detail: detail.map(Into::into),
            ..Default::default()
        };
        let mut items = vec![];
        let mut seen: AHashSet<&str> = AHashSet::default();
        let prefix: Vec<&str> = at.prefix.iter().map(|s| s.as_str()).collect();
        let module = match self.env.use_anchor(&self.scope, &prefix) {
            Ok(Some(anchor)) => anchor.path(),
            Ok(None) => {
                for kw in ["self", "super", "package"] {
                    items.push(item(kw, CompletionItemKind::KEYWORD, None));
                }
                let part = typed_path(&at.partial);
                let modules = self.env.lookup_matching_modules(&self.scope, &part);
                let modules = modules.iter().map(|m| m.to_string());
                let roots = self.env.package_roots.iter().map(|r| r.to_string());
                let mut names: Vec<String> = modules.chain(roots).collect();
                names.sort();
                names.dedup();
                let names = names.iter().filter(|n| !n.contains("::"));
                items.extend(names.map(|n| item(n, CompletionItemKind::MODULE, None)));
                items.retain(|i| i.label.starts_with(&at.partial));
                return items;
            }
            Err(_) => return vec![],
        };
        let env = self.env;
        let binds = env.binds.get(&module).into_iter().flat_map(|b| b.into_iter());
        for (name, id) in binds {
            if let Some(b) = env.by_id.get(id)
                && seen.insert(name)
            {
                let kind = match &b.typ {
                    Type::Fn(_) => CompletionItemKind::FUNCTION,
                    _ => CompletionItemKind::VARIABLE,
                };
                items.push(item(name, kind, Some(display_type(&b.typ))));
            }
        }
        let types = env.typedefs.get(&module).into_iter().flat_map(|t| t.into_iter());
        for (name, td) in types {
            if seen.insert(name) {
                let detail = typedef_decl(name, td);
                items.push(item(name, CompletionItemKind::STRUCT, Some(detail)));
            }
        }
        let traits = env.traits.get(&module).into_iter().flat_map(|t| t.into_iter());
        for (name, _) in traits {
            if seen.insert(name) {
                items.push(item(name, CompletionItemKind::INTERFACE, None));
            }
        }
        for m in env.modules.into_iter() {
            let child = NPath::dirname(&m.0) == Some(&*module.0)
                || (&*module.0 == "/" && NPath::levels(&m.0) == 1);
            if let Some(name) = NPath::basename(&m.0).filter(|_| child)
                && seen.insert(name)
            {
                items.push(item(name, CompletionItemKind::MODULE, None));
            }
        }
        if at.in_group {
            items.push(item("self", CompletionItemKind::KEYWORD, None));
        }
        items.retain(|i| i.label.starts_with(&at.partial));
        items.sort_by(|a, b| a.label.cmp(&b.label));
        items
    }

    /// Locals shadow the environment; a name that is a function and its
    /// module (`use foo;`) shows the function.
    fn names(&self, typed: &Typed) -> Vec<CompletionItem> {
        let mut items = vec![];
        let mut seen: AHashSet<CompactString> = AHashSet::default();
        if !typed.path.contains("::") {
            for b in self.locals().filter(|b| b.name.starts_with(&typed.path)) {
                if seen.insert(b.name.clone()) {
                    items.push(self.bind_item(&b.name, b));
                }
            }
        }
        let part = typed_path(&typed.path);
        // the env is the last check's, which holds every binding of this file
        let defining = self.defining();
        // an interface's `val` is bound where the `let` implementing it is
        // XCR claude for claude: [bug/low] In a module with an interface, the env's
        // binding for a name is the interface's `val` (ori is the .gxi), so in_file is
        // false and this filter never applies. Inside `let f = |..| { .. }` completion
        // then offers `f`, which the check refuses there ("f not defined"). Map a sig
        // bind to its implementation through ide.sig_links before testing the file and
        // position. probe:
        // design/review-2026-10-19/repro/x-lsp-probe-own-name-via-interface.py
        // (x-lsp-probe-06)
        // 2026-10-19 claude: Completer::names maps an interface val to its
        // implementing let through ide.sig_links before the file and position test.
        // Fails if undone: graphix-shell/tests/lsp
        // completion_offers_no_function_in_its_own_body (the interface case).
        let implementation = |b: &'a Bind| -> &'a Bind {
            let links = self.checked.into_iter().flat_map(|c| c.ide.sig_links.iter());
            links
                .filter(|l| l.sig_id == b.id)
                .find_map(|l| self.env.by_id.get(&l.impl_id))
                .map_or(b, |i| &**i)
        };
        let unbound = |b: &'a Bind| {
            let b = implementation(b);
            in_file(&b.ori, self.file) && (zero_based(b.pos) > self.cursor || defining(b))
        };
        for (name, id) in self.env.lookup_matching(&self.scope, &part) {
            if let Some(bind) = self.env.by_id.get(&id)
                && !unbound(bind)
                && seen.insert(name.clone())
            {
                items.push(self.bind_item(&name, bind));
            }
        }
        for module in self.env.lookup_matching_modules(&self.scope, &part) {
            let label = module.to_string();
            if seen.insert(label.as_str().into()) {
                let kind = Some(CompletionItemKind::MODULE);
                items.push(CompletionItem { label, kind, ..Default::default() });
            }
        }
        items
    }
}

impl ServerState {
    pub fn completions(&self, uri: &Uri, position: Position) -> Vec<CompletionItem> {
        let (Some(doc), Some(file)) = (self.document(uri), uri_to_path(uri)) else {
            return vec![];
        };
        let cursor = self.decode(uri, position);
        let checked = self.checked_for(&file);
        let c = Completer {
            state: self,
            env: checked.map(|c| &c.env).unwrap_or(&self.base_env),
            checked,
            file: &file,
            cursor,
            scope: Completer::scope_at(checked, &file, cursor),
        };
        if let Some(at) = use_path(&doc.text, cursor) {
            return c.imports(&at);
        }
        let callee = call_context(&doc.text, cursor);
        if let Some(start) = label_start(&doc.text, cursor) {
            let replace = Range { start: self.encode(&doc.text, start), end: position };
            return callee.map(|f| c.labels(&f, Some(replace))).unwrap_or_default();
        }
        let Some(typed) = typed_before(&doc.text, cursor) else { return vec![] };
        if !typed.receiver.is_empty() {
            return c.fields(&typed);
        }
        let mut items = callee.map(|f| c.labels(&f, None)).unwrap_or_default();
        items.extend(c.names(&typed));
        items
    }
}
