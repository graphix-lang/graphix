use arcstr::ArcStr;
use graphix_compiler::{env::Env, expr::ModPath, typ::Type};
use log::debug;
use netidx::path::Path;
use reedline::{Completer, Span, Suggestion};

#[derive(Debug)]
enum CompletionContext<'a> {
    Bind(Span, &'a str),
    ArgLbl { span: Span, function: &'a str, arg: &'a str },
}

/// Where the name ending at the end of `s` starts.
fn name_start(s: &str) -> usize {
    s.char_indices()
        .rev()
        .take_while(|(_, c)| c.is_alphanumeric() || *c == '_' || *c == ':')
        .last()
        .map_or(s.len(), |(i, _)| i)
}

impl<'a> CompletionContext<'a> {
    /// The name being typed at the end of `s`: a label after `#`, of the
    /// call whose `(` is open there, or else a binding.
    fn from_str(s: &'a str) -> Self {
        let start = name_start(s);
        let span = Span { start, end: s.len() };
        let name = &s[start..];
        if let Some(before) = s[..start].strip_suffix('#') {
            let mut depth = 0usize;
            for (i, c) in before.char_indices().rev() {
                match c {
                    ')' => depth += 1,
                    '(' if depth > 0 => depth -= 1,
                    '(' => {
                        let f = &before[..i];
                        let function = &f[name_start(f)..];
                        return Self::ArgLbl { span, function, arg: name };
                    }
                    _ => (),
                }
            }
        }
        Self::Bind(span, name)
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
                CompletionContext::Bind(span, s) => {
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
                CompletionContext::ArgLbl { span, function, arg: part } => {
                    let function = ModPath::from_iter(function.split("::"));
                    if let Some((_, b)) =
                        self.0.lookup_bind(&ModPath::root(), &function).ok().flatten()
                    {
                        if let Type::Fn(ft) = &b.typ {
                            for arg in ft.args.iter() {
                                if let Some(lbl) = arg.label() {
                                    if lbl.starts_with(part) {
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
            Bind(span, b) => format!("bind {b:?} at {}", span.start),
            ArgLbl { span, function, arg } => {
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
        assert_eq!(ctx("#a"), r#"bind "a" at 1"#);
    }
}
