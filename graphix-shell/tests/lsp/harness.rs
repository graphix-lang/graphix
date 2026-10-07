//! An LSP client over `Connection::memory()` against the shell's real
//! backend. The server answers in order, so a request sent after a
//! notification is a barrier: every diagnostic the notification caused
//! has arrived when the response does.
//!
//! Positions are written as markers: `"let y = |x + 1"` is the first
//! occurrence of `let y = x + 1` in the file's current text, cursor at
//! the `|`; a marker that needs a literal `|` marks the cursor with `^`
//! (`"|^acc, x| acc"`). Columns count UTF-16 units, the server's default.
//!
//! A fresh root's directory holds a space, brackets and a colon, and the
//! client spells URIs as VS Code does, which is not the server's own
//! spelling: diagnostics for an open document must come back under the
//! client's.

use graphix_lsp::uri::uri_to_path;
use lsp_server::{Connection, Message, Notification, Request, RequestId, Response};
use lsp_types::{
    notification::{self as notif, Notification as _},
    request as req, *,
};
use std::{
    collections::HashMap,
    fs,
    path::{Path, PathBuf},
    str::FromStr,
    thread::{self, JoinHandle},
    time::Duration,
};
use tempfile::TempDir;

const TIMEOUT: Duration = Duration::from_secs(120);

/// A place in the project: file (relative to the root), line, column.
pub type Site = (String, u32, u32);

pub struct Client {
    root: PathBuf,
    _dir: Option<TempDir>,
    conn: Connection,
    server: Option<JoinHandle<anyhow::Result<()>>>,
    next_id: i32,
    versions: HashMap<String, i32>,
    buffers: HashMap<String, String>,
    diagnostics: HashMap<String, Vec<Diagnostic>>,
}

impl Client {
    /// Write `files` under a fresh root and start a server on it.
    pub fn start(files: &[(&str, &str)]) -> Self {
        let dir = tempfile::tempdir().unwrap();
        let root = dir.path().join("proj [x] (copy) a:b");
        for (name, text) in files {
            let path = root.join(name);
            fs::create_dir_all(path.parent().unwrap()).unwrap();
            fs::write(path, text).unwrap();
        }
        Self::start_at(root, Some(dir))
    }

    /// Start a server on a directory of this repository.
    pub fn start_in_repo(dir: &str) -> Self {
        let root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("..").join(dir);
        Self::start_at(root.canonicalize().unwrap(), None)
    }

    fn start_at(root: PathBuf, dir: Option<TempDir>) -> Self {
        let (server_conn, conn) = Connection::memory();
        let server =
            thread::spawn(move || graphix_shell::lsp_backend::serve(server_conn));
        let mut t = Self {
            root,
            _dir: dir,
            conn,
            server: Some(server),
            next_id: 0,
            versions: HashMap::new(),
            buffers: HashMap::new(),
            diagnostics: HashMap::new(),
        };
        let root = client_uri(&t.root);
        #[allow(deprecated)]
        let params = InitializeParams {
            workspace_folders: Some(vec![WorkspaceFolder {
                uri: root,
                name: "test".into(),
            }]),
            ..Default::default()
        };
        t.request::<req::Initialize>(params);
        t.notify::<notif::Initialized>(InitializedParams {});
        t
    }

    pub fn path(&self, file: &str) -> PathBuf {
        self.root.join(file)
    }

    fn uri(&self, file: &str) -> Uri {
        client_uri(&self.path(file))
    }

    fn file_of(&self, uri: &Uri) -> String {
        let path = uri_to_path(uri).unwrap();
        match path.strip_prefix(&self.root) {
            Ok(p) => p.to_str().unwrap().to_string(),
            Err(_) => path.to_str().unwrap().to_string(),
        }
    }

    fn text(&self, file: &str) -> String {
        match self.buffers.get(file) {
            Some(t) => t.clone(),
            None => fs::read_to_string(self.path(file)).unwrap(),
        }
    }

    /// The position of `marker`'s `|` in the current text of `file`.
    pub fn at(&self, file: &str, marker: &str) -> Position {
        let mark = if marker.contains('^') { '^' } else { '|' };
        let cursor = marker.find(mark).expect("a marker has a cursor");
        let needle = marker.replacen(mark, "", 1);
        let text = self.text(file);
        let start =
            text.find(&needle).unwrap_or_else(|| panic!("`{needle}` is not in {file}"));
        let before = &text[..start + cursor];
        let line = before.matches('\n').count() as u32;
        let character = before.rsplit('\n').next().unwrap().encode_utf16().count() as u32;
        Position { line, character }
    }

    /// `at` as an expected result.
    pub fn site(&self, file: &str, marker: &str) -> Site {
        let p = self.at(file, marker);
        (file.to_string(), p.line, p.character)
    }

    fn notify<N: notif::Notification>(&mut self, params: N::Params) {
        let n = Notification::new(N::METHOD.to_string(), params);
        self.conn.sender.send(Message::Notification(n)).unwrap();
    }

    /// Send a request and wait for its response, recording the
    /// diagnostics published meanwhile.
    fn exchange(&mut self, method: &str, params: serde_json::Value) -> Response {
        self.next_id += 1;
        let id = RequestId::from(self.next_id);
        let r = Request { id: id.clone(), method: method.into(), params };
        self.conn.sender.send(Message::Request(r)).unwrap();
        loop {
            let msg = self
                .conn
                .receiver
                .recv_timeout(TIMEOUT)
                .unwrap_or_else(|e| panic!("no response to {method}: {e}"));
            match msg {
                Message::Response(r) if r.id == id => return r,
                Message::Notification(n)
                    if n.method == notif::PublishDiagnostics::METHOD =>
                {
                    let p: PublishDiagnosticsParams =
                        serde_json::from_value(n.params).unwrap();
                    let file = self.file_of(&p.uri);
                    if self.buffers.contains_key(&file) {
                        assert_eq!(
                            p.uri,
                            self.uri(&file),
                            "published under another spelling"
                        );
                    }
                    self.diagnostics.insert(file, p.diagnostics);
                }
                _ => (),
            }
        }
    }

    pub fn request<R: req::Request>(&mut self, params: R::Params) -> R::Result {
        let r = self.exchange(R::METHOD, serde_json::to_value(params).unwrap());
        if let Some(e) = r.error {
            panic!("{} failed: {}", R::METHOD, e.message)
        }
        serde_json::from_value(r.result.unwrap_or(serde_json::Value::Null)).unwrap()
    }

    /// A request with arbitrary params: the error message, if refused.
    pub fn raw_request(
        &mut self,
        method: &str,
        params: serde_json::Value,
    ) -> Option<String> {
        self.exchange(method, params).error.map(|e| e.message)
    }

    pub fn open(&mut self, file: &str) {
        let text = self.text(file);
        self.versions.insert(file.to_string(), 1);
        self.buffers.insert(file.to_string(), text.clone());
        self.notify::<notif::DidOpenTextDocument>(DidOpenTextDocumentParams {
            text_document: TextDocumentItem {
                uri: self.uri(file),
                language_id: "graphix".into(),
                version: 1,
                text,
            },
        });
    }

    /// Replace the open buffer of `file`; disk is untouched.
    pub fn edit(&mut self, file: &str, text: &str) {
        let version = self.versions.get_mut(file).expect("edit an open file");
        *version += 1;
        let version = *version;
        self.buffers.insert(file.to_string(), text.to_string());
        self.notify::<notif::DidChangeTextDocument>(DidChangeTextDocumentParams {
            text_document: VersionedTextDocumentIdentifier {
                uri: self.uri(file),
                version,
            },
            content_changes: vec![TextDocumentContentChangeEvent {
                range: None,
                range_length: None,
                text: text.to_string(),
            }],
        });
    }

    /// `edit` with the first `from` replaced by `to`.
    pub fn replace(&mut self, file: &str, from: &str, to: &str) {
        let text = self.text(file);
        assert!(text.contains(from), "`{from}` is not in {file}");
        self.edit(file, &text.replacen(from, to, 1));
    }

    /// Write the buffer to disk and tell the server.
    pub fn save(&mut self, file: &str) {
        fs::write(self.path(file), self.text(file)).unwrap();
        self.notify::<notif::DidSaveTextDocument>(DidSaveTextDocumentParams {
            text_document: TextDocumentIdentifier { uri: self.uri(file) },
            text: None,
        });
    }

    /// Write `text` to `file` on disk behind the editor's back, and tell the
    /// server the way a file watcher does.
    pub fn disk_changed(&mut self, file: &str, text: &str) {
        fs::write(self.path(file), text).unwrap();
        self.notify::<notif::DidChangeWatchedFiles>(DidChangeWatchedFilesParams {
            changes: vec![FileEvent {
                uri: self.uri(file),
                typ: FileChangeType::CHANGED,
            }],
        });
    }

    pub fn close(&mut self, file: &str) {
        self.buffers.remove(file);
        self.versions.remove(file);
        self.notify::<notif::DidCloseTextDocument>(DidCloseTextDocumentParams {
            text_document: TextDocumentIdentifier { uri: self.uri(file) },
        });
    }

    fn sync(&mut self) {
        self.request::<req::WorkspaceSymbolRequest>(WorkspaceSymbolParams {
            query: "\u{0}".into(),
            ..Default::default()
        });
    }

    fn standing(&mut self, file: &str, severity: DiagnosticSeverity) -> Vec<Diagnostic> {
        self.sync();
        let diags = self.diagnostics.get(file).map(|d| d.as_slice()).unwrap_or(&[]);
        diags.iter().filter(|d| d.severity == Some(severity)).cloned().collect()
    }

    /// The errors standing on `file`: (line, column, message).
    pub fn diagnostics(&mut self, file: &str) -> Vec<(u32, u32, String)> {
        let errors = self.standing(file, DiagnosticSeverity::ERROR);
        errors
            .iter()
            .map(|d| (d.range.start.line, d.range.start.character, d.message.clone()))
            .collect()
    }

    /// The warnings standing on `file`: (underlined text, message).
    pub fn warnings(&mut self, file: &str) -> Vec<(String, String)> {
        let text = self.text(file);
        let warnings = self.standing(file, DiagnosticSeverity::WARNING);
        warnings
            .iter()
            .map(|d| {
                let under =
                    &text[offset(&text, d.range.start)..offset(&text, d.range.end)];
                (under.to_string(), d.message.clone())
            })
            .collect()
    }

    /// The text each diagnostic on `file` underlines.
    pub fn underlined(&mut self, file: &str) -> Vec<String> {
        self.sync();
        let text = self.text(file);
        let diags = self.diagnostics.get(file).map(|d| d.as_slice()).unwrap_or(&[]);
        diags
            .iter()
            .map(|d| {
                text[offset(&text, d.range.start)..offset(&text, d.range.end)].to_string()
            })
            .collect()
    }

    /// Every file with a standing error.
    pub fn files_with_diagnostics(&mut self) -> Vec<String> {
        self.sync();
        let is_error = |d: &Diagnostic| d.severity == Some(DiagnosticSeverity::ERROR);
        let mut files: Vec<String> = self
            .diagnostics
            .iter()
            .filter(|(_, d)| d.iter().any(is_error))
            .map(|(f, _)| f.clone())
            .collect();
        files.sort();
        files
    }

    fn doc_pos(&self, file: &str, marker: &str) -> TextDocumentPositionParams {
        TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri: self.uri(file) },
            position: self.at(file, marker),
        }
    }

    fn site_of(&self, l: &Location) -> Site {
        (self.file_of(&l.uri), l.range.start.line, l.range.start.character)
    }

    /// The hover text at `marker`.
    pub fn hover(&mut self, file: &str, marker: &str) -> Option<String> {
        let params = HoverParams {
            text_document_position_params: self.doc_pos(file, marker),
            work_done_progress_params: Default::default(),
        };
        match self.request::<req::HoverRequest>(params)?.contents {
            HoverContents::Markup(m) => Some(m.value),
            c => panic!("unexpected hover contents {c:?}"),
        }
    }

    pub fn definition(&mut self, file: &str, marker: &str) -> Option<Site> {
        let params = GotoDefinitionParams {
            text_document_position_params: self.doc_pos(file, marker),
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
        };
        match self.request::<req::GotoDefinition>(params)? {
            GotoDefinitionResponse::Scalar(l) => Some(self.site_of(&l)),
            r => panic!("unexpected definition response {r:?}"),
        }
    }

    /// Reference sites, the declaration included, sorted.
    pub fn references(&mut self, file: &str, marker: &str) -> Vec<Site> {
        let params = ReferenceParams {
            text_document_position: self.doc_pos(file, marker),
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
            context: ReferenceContext { include_declaration: true },
        };
        let locs = self.request::<req::References>(params).unwrap_or_default();
        let mut sites: Vec<Site> = locs.iter().map(|l| self.site_of(l)).collect();
        sites.sort();
        sites
    }

    /// Completion labels at `marker`.
    pub fn completions(&mut self, file: &str, marker: &str) -> Vec<String> {
        let params = CompletionParams {
            text_document_position: self.doc_pos(file, marker),
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
            context: None,
        };
        match self.request::<req::Completion>(params) {
            None => vec![],
            Some(CompletionResponse::Array(items)) => {
                items.into_iter().map(|i| i.label).collect()
            }
            Some(CompletionResponse::List(l)) => {
                l.items.into_iter().map(|i| i.label).collect()
            }
        }
    }

    /// Document symbols: (name, the text its range spans).
    pub fn symbol_spans(&mut self, file: &str) -> Vec<(String, String)> {
        let params = DocumentSymbolParams {
            text_document: TextDocumentIdentifier { uri: self.uri(file) },
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
        };
        let text = self.text(file);
        match self.request::<req::DocumentSymbolRequest>(params) {
            Some(DocumentSymbolResponse::Nested(s)) => s
                .into_iter()
                .map(|s| {
                    let span =
                        &text[offset(&text, s.range.start)..offset(&text, s.range.end)];
                    (s.name, span.to_string())
                })
                .collect(),
            r => panic!("unexpected symbols {r:?}"),
        }
    }

    /// Document symbols: (name, detail).
    pub fn symbols(&mut self, file: &str) -> Vec<(String, Option<String>)> {
        let params = DocumentSymbolParams {
            text_document: TextDocumentIdentifier { uri: self.uri(file) },
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
        };
        match self.request::<req::DocumentSymbolRequest>(params) {
            None => vec![],
            Some(DocumentSymbolResponse::Nested(s)) => {
                s.into_iter().map(|s| (s.name, s.detail)).collect()
            }
            Some(DocumentSymbolResponse::Flat(s)) => {
                s.into_iter().map(|s| (s.name, None)).collect()
            }
        }
    }
}

impl Drop for Client {
    fn drop(&mut self) {
        if thread::panicking() {
            return;
        }
        self.request::<req::Shutdown>(());
        self.notify::<notif::Exit>(());
        self.server.take().unwrap().join().unwrap().unwrap();
    }
}

/// The byte offset of `p` (UTF-16 columns) in `text`.
fn offset(text: &str, p: Position) -> usize {
    let mut lines = text.split_inclusive('\n');
    let start: usize = lines.by_ref().take(p.line as usize).map(|l| l.len()).sum();
    let mut units = 0;
    let mut bytes = 0;
    for c in lines.next().unwrap_or("").chars() {
        if units >= p.character as usize {
            break;
        }
        units += c.len_utf16();
        bytes += c.len_utf8();
    }
    start + bytes
}

/// `path` as VS Code spells a file URI: every byte but `A-Za-z0-9-._~/`
/// percent-encoded.
fn client_uri(path: &Path) -> Uri {
    let mut s = String::from("file://");
    for b in path.as_os_str().as_encoded_bytes() {
        match b {
            b'A'..=b'Z'
            | b'a'..=b'z'
            | b'0'..=b'9'
            | b'-'
            | b'.'
            | b'_'
            | b'~'
            | b'/' => s.push(*b as char),
            _ => s.push_str(&format!("%{b:02X}")),
        }
    }
    Uri::from_str(&s).unwrap()
}
