use anyhow::{anyhow, Result};
use arcstr::ArcStr;
use graphix_compiler::{env::Env, expr::ModPath, typ::Type};
use graphix_rt::{GXExt, GXRt};
use log::debug;
use netidx::path::Path;
use reedline::{Completer, Span, Suggestion};

#[derive(Debug)]
enum CompletionContext<'a> {
    Bind(Span, &'a str),
    ArgLbl { span: Span, function: &'a str, arg: &'a str },
}

impl<'a> CompletionContext<'a> {
    fn from_str(s: &'a str) -> Result<Self> {
        let mut arg_lbl = 0;
        let mut fend = 0;
        let mut prev = 0;
        for (i, c) in s.char_indices().rev() {
            if c == '#' {
                arg_lbl = prev;
            }
            if c == '(' && arg_lbl != 0 {
                fend = i;
            }
            if c.is_whitespace() || c == '(' || c == '{' {
                if arg_lbl > 0 && fend > 0 {
                    let function =
                        s.get(prev..fend).ok_or_else(|| anyhow!("invalid function"))?;
                    let arg = s.get(arg_lbl..).ok_or_else(|| anyhow!("invalid arg"))?;
                    return Ok(Self::ArgLbl {
                        span: Span { start: arg_lbl, end: s.len() },
                        function,
                        arg,
                    });
                } else {
                    return Ok(Self::Bind(
                        Span { start: prev, end: s.len() },
                        s.get(prev..).ok_or_else(|| anyhow!("invalid bind"))?,
                    ));
                }
            }
            prev = i;
        }
        Ok(Self::Bind(Span { start: 0, end: s.len() }, s))
    }
}

pub(super) struct BComplete<X: GXExt>(pub Env<GXRt<X>, X::UserEvent>);

impl<X: GXExt> Completer for BComplete<X> {
    fn complete(&mut self, line: &str, pos: usize) -> Vec<Suggestion> {
        debug!("{line}: {pos}");
        let mut res = vec![];
        let s = line.get(0..pos);
        debug!("{s:?}");
        if let Some(s) = s {
            let cc = CompletionContext::from_str(s);
            debug!("{cc:?}");
            if let Ok(cc) = cc {
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
                            })
                        }
                        for (value, id) in self.0.lookup_matching(&ModPath::root(), &part)
                        {
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
                                    let path =
                                        Path::from(ArcStr::from(dir)).append(&*value);
                                    format!("{}", ModPath(path))
                                }
                            };
                            res.push(Suggestion {
                                span,
                                value,
                                description: Some(description),
                                style: None,
                                extra: Some(vec!["hello world!".into()]),
                                append_whitespace: false,
                            })
                        }
                    }
                    CompletionContext::ArgLbl { span, function, arg: part } => {
                        let function = ModPath::from_iter(function.split("::"));
                        if let Some((_, b)) =
                            self.0.lookup_bind(&ModPath::root(), &function)
                        {
                            if let Type::Fn(ft) = &b.typ {
                                for arg in ft.args.iter() {
                                    if let Some((lbl, _)) = &arg.label {
                                        if lbl.starts_with(part) {
                                            let description =
                                                Some(format!("{}", arg.typ));
                                            res.push(Suggestion {
                                                span,
                                                value: lbl.as_str().into(),
                                                description,
                                                style: None,
                                                extra: None,
                                                append_whitespace: false,
                                            })
                                        }
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
