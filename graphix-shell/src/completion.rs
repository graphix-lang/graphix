use arcstr::ArcStr;
use graphix_compiler::{env::Env, expr::ModPath, typ::Type};
use graphix_lsp::text;
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
                            Some(b) => {
                                use std::fmt::Write;
                                let mut res = String::new();
                                match &b.typ {
                                    Type::Fn(ft) => {
                                        let ft = ft.replace_auto_constrained();
                                        write!(res, "{} ", ft).unwrap()
                                    }
                                    t => write!(res, "{} ", t).unwrap(),
                                }
                                if let Some(doc) = &b.doc {
                                    write!(res, "{doc}").unwrap();
                                };
                                res
                            }
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
                                        let description = Some(format!("{}", arg.typ));
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
}
