use arcstr::ArcStr;
use graphix_compiler::{env::Env, expr::ModPath, typ::Type};
use graphix_lsp::{display_type, text};
use log::debug;
use lsp_types::Position;
use netidx::path::Path;
use reedline::{Completer, Span, Suggestion};

#[derive(Debug)]
enum CompletionContext {
    Bind(Span, String),
    ArgLbl { span: Span, function: String, arg: String },
}

impl CompletionContext {
    /// The name being typed at the end of `s`: a label after `#`, of the
    /// call whose `(` is open there, or else a binding (the language
    /// server's reading of the text, over one line).
    fn from_str(s: &str) -> Option<Self> {
        let cursor = Position { line: 0, character: s.chars().count() as u32 };
        let path = text::typed_before(s, cursor)?.path;
        let span = Span { start: s.len() - path.len(), end: s.len() };
        Some(match text::label_start(s, cursor) {
            Some(_) => {
                Self::ArgLbl { span, function: text::call_context(s, cursor)?, arg: path }
            }
            None => Self::Bind(span, path),
        })
    }
}

pub(super) struct BComplete(pub Env);

impl Completer for BComplete {
    fn complete(&mut self, line: &str, pos: usize) -> Vec<Suggestion> {
        debug!("{line}: {pos}");
        let mut res = vec![];
        let s = line.get(0..pos);
        debug!("{s:?}");
        if let Some(s) = s {
            let cc = CompletionContext::from_str(s);
            debug!("{cc:?}");
            match cc {
                None => (),
                Some(CompletionContext::Bind(span, s)) => {
                    let part = ModPath::from_iter(s.split("::"));
                    for m in self.0.lookup_matching_modules(&ModPath::root(), &part) {
                        let value = format!("{m}");
                        res.push(Suggestion {
                            span,
                            value,
                            description: Some("module".into()),
                            style: None,
                            extra: None,
                            append_whitespace: false,
                            match_indices: None,
                            display_override: None,
                        })
                    }
                    for (value, id) in self.0.lookup_matching(&ModPath::root(), &part) {
                        let description = match self.0.by_id.get(&id) {
                            None => format!("_"),
                            Some(b) => format!(
                                "{} {}",
                                display_type(&b.typ),
                                b.doc.as_deref().unwrap_or("")
                            ),
                        };
                        let value = match Path::dirname(&part.0) {
                            None => String::from(value.as_str()),
                            Some(dir) => {
                                let path = Path::from(ArcStr::from(dir)).append(&*value);
                                format!("{}", ModPath(path))
                            }
                        };
                        res.push(Suggestion {
                            span,
                            value,
                            description: Some(description),
                            style: None,
                            extra: None,
                            append_whitespace: false,
                            match_indices: None,
                            display_override: None,
                        })
                    }
                }
                Some(CompletionContext::ArgLbl { span, function, arg: part }) => {
                    let function = ModPath::from_iter(function.split("::"));
                    if let Some((_, b)) =
                        self.0.lookup_bind(&ModPath::root(), &function).ok().flatten()
                    {
                        if let Type::Fn(ft) = &b.typ {
                            for arg in ft.args.iter() {
                                if let Some(lbl) = arg.label() {
                                    if lbl.starts_with(part.as_str()) {
                                        let description =
                                            Some(display_type(&arg.typ).to_string());
                                        res.push(Suggestion {
                                            span,
                                            value: lbl.as_str().into(),
                                            description,
                                            style: None,
                                            extra: None,
                                            append_whitespace: false,
                                            match_indices: None,
                                            display_override: None,
                                        })
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        res
    }
}

#[cfg(test)]
mod tests {
    use super::CompletionContext::{self, *};

    fn ctx(s: &str) -> String {
        match CompletionContext::from_str(s) {
            None => "none".into(),
            Some(Bind(span, b)) => format!("bind {b:?} at {}", span.start),
            Some(ArgLbl { span, function, arg }) => {
                format!("label {arg:?} of {function:?} at {}", span.start)
            }
        }
    }

    #[test]
    fn the_name_at_the_cursor() {
        assert_eq!(ctx("str::jo"), r#"bind "str::jo" at 0"#);
        assert_eq!(ctx("let x = "), r#"bind "" at 8"#);
        assert_eq!(ctx("f(a"), r#"bind "a" at 2"#);
        assert_eq!(ctx("str::join(#se"), r#"label "se" of "str::join" at 11"#);
        assert_eq!(ctx("f(g(1), #"), r#"label "" of "f" at 9"#);
        assert_eq!(ctx("f(g(#a"), r#"label "a" of "g" at 5"#);
        // a label outside a call's parentheses, or in a nested bracket,
        // names nothing; `?#` is a fn type's
        assert_eq!(ctx("#a"), "none");
        assert_eq!(ctx("f([a, #"), "none");
        assert_eq!(ctx("let t: fn(?#"), r#"bind "" at 12"#);
    }

    /// A completion describes a binding, or a label, as a hover does: a
    /// variable a call decided as its type, one instance decided at its
    /// bound with no header.
    #[tokio::test(flavor = "multi_thread")]
    async fn a_completion_describes_a_binding_as_a_hover_does() -> anyhow::Result<()> {
        use super::BComplete;
        use graphix_rt::{GXConfig, GXRt, NoExt};
        use reedline::Completer;
        use tokio::sync::mpsc;
        let mut ctx = GXRt::<NoExt>::new_state()?;
        let packages = crate::stdlib_packages::<NoExt>();
        let (vfs, root) =
            graphix_package::register_packages(&mut ctx, packages.iter().map(|p| &**p))?;
        let (tx, mut rx) = mpsc::channel(100);
        tokio::spawn(async move { while rx.recv().await.is_some() {} });
        let gx = GXConfig::builder(ctx, tx)
            .root(root)
            .resolvers(vec![vfs])
            .build()?
            .start()
            .await?;
        let src = "let h = 'a: Number |x: 'a| -> 'a x;\n\
                   let decided = |q: i64| h(q);\n\
                   let mk = |z: i64| 'a: Number |x: 'a| -> 'a x;\n\
                   let one = mk(0);\n\
                   let labeled = |#w, v: i64| h(v) + w";
        let env = gx.compile(src.into()).await?.env;
        let mut c = BComplete(env);
        let mut described = |line: &str, name: &str| {
            let found = c.complete(line, line.len());
            found
                .into_iter()
                .find(|s| s.value == name)
                .and_then(|s| s.description)
                .unwrap_or_else(|| panic!("no completion of {name} after {line}"))
        };
        assert_eq!(described("decided", "decided"), "fn(q: i64) -> i64 ");
        assert_eq!(described("one", "one"), "fn(x: Number) -> Number ");
        assert_eq!(described("labeled", "labeled"), "fn(#w: i64, v: i64) -> i64 ");
        assert_eq!(described("labeled(#", "w"), "i64");
        Ok(())
    }
}
