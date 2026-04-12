use arcstr::ArcStr;
use graphix_compiler::{
    env::Env,
    expr::{parser, ModPath, Origin, Source},
    typ::Type,
};
use lsp_types::Uri;
use std::{collections::HashMap, path::PathBuf, str::FromStr};

/// An open document tracked by the language server.
pub struct Document {
    pub version: i32,
    pub text: String,
}

impl Document {
    pub fn new(text: String, version: i32) -> Self {
        Self { version, text }
    }
}

/// Core language intelligence state.
///
/// Holds the compiler environment and open documents, and provides
/// completion, hover, go-to-definition, and diagnostics.
pub struct ServerState {
    pub env: Env,
    pub documents: HashMap<Uri, Document>,
}

impl ServerState {
    pub fn new() -> Self {
        Self { env: Env::default(), documents: HashMap::new() }
    }

    pub fn with_env(env: Env) -> Self {
        Self { env, documents: HashMap::new() }
    }

    /// Track a newly opened document.
    pub fn open_document(&mut self, uri: Uri, text: String, version: i32) {
        self.documents.insert(uri, Document::new(text, version));
    }

    /// Update the text of an already-opened document.
    pub fn update_document(&mut self, uri: &Uri, text: String, version: i32) {
        if let Some(doc) = self.documents.get_mut(uri) {
            doc.text = text;
            doc.version = version;
        } else {
            self.documents.insert(uri.clone(), Document::new(text, version));
        }
    }

    /// Stop tracking a document.
    pub fn close_document(&mut self, uri: &Uri) {
        self.documents.remove(uri);
    }

    /// Parse the document and return any diagnostics.
    pub fn check_document(&mut self, uri: &Uri) -> Vec<lsp_types::Diagnostic> {
        let Some(doc) = self.documents.get(uri) else {
            return Vec::new();
        };

        let source = uri_to_source(uri);

        let origin = Origin {
            parent: None,
            source,
            text: ArcStr::from(doc.text.as_str()),
        };

        match parser::parse(origin) {
            Ok(_exprs) => {
                // Parsing succeeded
                // TODO: Full type checking requires runtime integration
                Vec::new()
            }
            Err(e) => {
                let range = lsp_types::Range::default();

                vec![lsp_types::Diagnostic {
                    range,
                    severity: Some(lsp_types::DiagnosticSeverity::ERROR),
                    message: format!("{}", e),
                    ..Default::default()
                }]
            }
        }
    }

    /// Return completion items at the given position.
    pub fn completions(
        &self,
        uri: &Uri,
        _position: lsp_types::Position,
    ) -> Vec<lsp_types::CompletionItem> {
        let Some(_doc) = self.documents.get(uri) else {
            return Vec::new();
        };

        let scope = ModPath::root();

        let mut items = Vec::new();

        for (name, bind_id) in self.env.lookup_matching(&scope, &scope) {
            if let Some(bind) = self.env.by_id.get(&bind_id) {
                let kind = if matches!(&bind.typ, Type::Fn(_)) {
                    lsp_types::CompletionItemKind::FUNCTION
                } else {
                    lsp_types::CompletionItemKind::VARIABLE
                };

                items.push(lsp_types::CompletionItem {
                    label: name.to_string(),
                    kind: Some(kind),
                    detail: Some(format!("{}", bind.typ)),
                    documentation: bind.doc.as_ref().map(|d| {
                        lsp_types::Documentation::String(d.to_string())
                    }),
                    ..Default::default()
                });
            }
        }

        for module in self.env.lookup_matching_modules(&scope, &scope) {
            items.push(lsp_types::CompletionItem {
                label: module.to_string(),
                kind: Some(lsp_types::CompletionItemKind::MODULE),
                ..Default::default()
            });
        }

        items
    }

    /// Return hover information for the symbol at the given position.
    pub fn hover(
        &self,
        uri: &Uri,
        position: lsp_types::Position,
    ) -> Option<lsp_types::Hover> {
        let doc = self.documents.get(uri)?;
        let word = get_word_at_position(&doc.text, position)?;

        let scope = ModPath::root();
        let name = ModPath::from([word.as_str()]);

        if let Some((_, bind)) = self.env.lookup_bind(&scope, &name) {
            let mut contents = format!("```graphix\n{}: {}\n```", word, bind.typ);
            if let Some(doc) = &bind.doc {
                contents.push_str("\n\n");
                contents.push_str(doc);
            }
            return Some(lsp_types::Hover {
                contents: lsp_types::HoverContents::Markup(lsp_types::MarkupContent {
                    kind: lsp_types::MarkupKind::Markdown,
                    value: contents,
                }),
                range: None,
            });
        }

        if let Some(typedef) = self.env.lookup_typedef(&scope, &name) {
            let mut contents = format!("```graphix\ntype {} = {}\n```", word, typedef.typ);
            if let Some(doc) = &typedef.doc {
                contents.push_str("\n\n");
                contents.push_str(doc);
            }
            return Some(lsp_types::Hover {
                contents: lsp_types::HoverContents::Markup(lsp_types::MarkupContent {
                    kind: lsp_types::MarkupKind::Markdown,
                    value: contents,
                }),
                range: None,
            });
        }

        None
    }

    /// Return the definition location for the symbol at the given position.
    ///
    /// Currently a stub — `Bind` does not yet carry source position
    /// information on this branch. Will be enabled once `pos` and `ori`
    /// fields are added to `Bind`.
    pub fn definition(
        &self,
        _uri: &Uri,
        _position: lsp_types::Position,
    ) -> Option<lsp_types::Location> {
        // TODO: Bind does not have pos/ori fields on data-table branch yet.
        None
    }
}

pub fn uri_to_source(uri: &Uri) -> Source {
    if let Some(path) = uri_to_path(uri) {
        Source::File(path)
    } else {
        Source::Unspecified
    }
}

pub fn uri_to_path(uri: &Uri) -> Option<PathBuf> {
    let s = uri.as_str();
    if s.starts_with("file://") {
        Some(PathBuf::from(&s[7..]))
    } else {
        None
    }
}

pub fn path_to_uri(path: &PathBuf) -> Option<Uri> {
    let uri_str = format!("file://{}", path.display());
    Uri::from_str(&uri_str).ok()
}

pub fn get_word_at_position(text: &str, position: lsp_types::Position) -> Option<String> {
    let lines: Vec<&str> = text.lines().collect();
    let line = lines.get(position.line as usize)?;
    let col = position.character as usize;

    if col > line.len() {
        return None;
    }

    let chars: Vec<char> = line.chars().collect();
    let mut start = col;
    let mut end = col;

    while start > 0 {
        let c = chars[start - 1];
        if !c.is_alphanumeric() && c != '_' {
            break;
        }
        start -= 1;
    }

    while end < chars.len() {
        let c = chars[end];
        if !c.is_alphanumeric() && c != '_' {
            break;
        }
        end += 1;
    }

    if start == end {
        return None;
    }

    Some(chars[start..end].iter().collect())
}
