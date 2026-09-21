//! Completion: the labels of the call being written, the fields of the
//! value being read, else every name visible at the cursor.

use crate::{
    query::{display_type, in_file},
    state::{Checked, ServerState},
    text::{Typed, call_context, label_start, typed_before, zero_based},
    uri::uri_to_path,
};
use ahash::AHashSet;
use compact_str::CompactString;
use graphix_compiler::{
    env::{Bind, Env},
    expr::ModPath,
    typ::{FnArgKind, FnType, Type},
};
use lsp_types::{
    CompletionItem, CompletionItemKind, CompletionTextEdit, Documentation,
    InsertTextFormat, Position, Range, TextEdit, Uri,
};
use std::{fmt::Write, path::Path};

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
    /// The scope of the last expression the check entered at or before
    /// the cursor.
    fn scope_at(checked: Option<&Checked>, file: &Path, cursor: Position) -> ModPath {
        let entries = checked.iter().flat_map(|c| c.ide.scope_map.iter());
        entries
            .filter(|e| in_file(&e.ori, file) && zero_based(e.pos) <= cursor)
            .max_by_key(|e| zero_based(e.pos))
            .map(|e| e.scope.lexical.clone())
            .unwrap_or_else(ModPath::root)
    }

    /// Every binding declared before the cursor in a scope enclosing
    /// it, innermost and latest first.
    fn locals(&self) -> impl Iterator<Item = &'a Bind> {
        let binds = self.checked.into_iter().flat_map(|c| c.ide.binds.iter().rev());
        binds.filter(|b| {
            in_file(&b.ori, self.file)
                && zero_based(b.pos) <= self.cursor
                && is_under(&self.scope, &b.scope)
        })
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
        let fields_of = |typ: &Type| match typ.resolve_tvars() {
            Type::Struct(fields) => Some(fields),
            t @ Type::Ref(_) => match t.lookup_ref(self.env) {
                Ok(Type::Struct(fields)) => Some(fields),
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
        for (name, id) in self.env.lookup_matching(&self.scope, &part) {
            if let Some(bind) = self.env.by_id.get(&id)
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
        let (Some(doc), Some(file)) = (self.documents.get(uri), uri_to_path(uri)) else {
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
