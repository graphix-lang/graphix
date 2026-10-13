//! Review t-parser-a: spans the expr_spans pin cannot see. That pin
//! checks `[pos, end)` parses back to the node, which a span with
//! leading or trailing whitespace still does, and its corpus has no
//! lambda as an operator's operand and no `&|x| ..`.

use arcstr::ArcStr;
use graphix_types::expr::{Expr, ExprKind, Origin, Source, parser};
use std::{
    collections::BTreeMap,
    fs,
    path::{Path, PathBuf},
};

fn offset(text: &str, line: i32, column: i32) -> usize {
    let start: usize =
        text.split_inclusive('\n').take(line as usize - 1).map(|l| l.len()).sum();
    let rest = &text[start..];
    start + rest.char_indices().nth(column as usize - 1).map_or(rest.len(), |(i, _)| i)
}

fn kind_name(kind: &ExprKind) -> String {
    let s = format!("{kind:?}");
    s.chars().take_while(|c| c.is_alphanumeric()).collect()
}

fn walk(text: &str, e: &Expr, out: &mut BTreeMap<String, Vec<String>>) {
    let why = match e.end.get() {
        None => Some("no end".to_string()),
        Some(end) if !matches!(e.kind, ExprKind::NoOp) => {
            let (from, to) = (offset(text, e.pos.line, e.pos.column), offset(text, end.line, end.column));
            let slice = &text[from..to.max(from)];
            if slice.starts_with(char::is_whitespace) || slice.ends_with(char::is_whitespace) {
                Some(format!("whitespace at an edge: {slice:?}"))
            } else {
                None
            }
        }
        Some(_) => None,
    };
    if let Some(why) = why {
        out.entry(kind_name(&e.kind)).or_default().push(format!("{} {why}", e.pos));
    }
    e.for_each_child(&mut |c| walk(text, c, out));
}

fn parse(text: &str) -> Vec<Expr> {
    let ori = Origin {
        parent: None,
        source: Source::File(PathBuf::from("fixture.gx")),
        text: ArcStr::from(text),
    };
    parser::parse(ori).unwrap().to_vec()
}

fn report(text: &str) -> BTreeMap<String, Vec<String>> {
    let mut out = BTreeMap::new();
    for e in parse(text) {
        walk(text, &e, &mut out);
    }
    out
}

#[test]
fn lambda_operand_span() {
    let r = report("let f = t ~   |x| x + 1");
    eprintln!("lambda operand: {r:#?}");
    let r = report("let f = &|x| x + 1");
    eprintln!("byref lambda: {r:#?}");
    let r = report("let f = seq t ~ |x| x { 1 }");
    eprintln!("seq trigger lambda: {r:#?}");
    let r = report("let v = [`A   , `B  ]");
    eprintln!("variant: {r:#?}");
}

fn sources(dir: &Path, out: &mut Vec<PathBuf>) {
    for entry in fs::read_dir(dir).unwrap() {
        let path = entry.unwrap().path();
        if path.is_dir() {
            sources(&path, out);
        } else if path.extension().is_some_and(|e| e == "gx") {
            out.push(path);
        }
    }
}

#[test]
fn corpus_span_edges() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("..");
    let mut files = vec![];
    for dir in ["book/src/examples", "stdlib"] {
        sources(&root.join(dir), &mut files);
    }
    let mut all: BTreeMap<String, Vec<String>> = BTreeMap::new();
    for f in files {
        let text = fs::read_to_string(&f).unwrap();
        let ori = Origin {
            parent: None,
            source: Source::File(f.clone()),
            text: ArcStr::from(text.as_str()),
        };
        let Ok(exprs) = parser::parse(ori) else { continue };
        let mut out = BTreeMap::new();
        for e in exprs.iter() {
            walk(&text, e, &mut out);
        }
        for (k, v) in out {
            all.entry(k).or_default().push(format!("{}: {} (x{})", f.display(), v[0], v.len()));
        }
    }
    for (k, v) in &all {
        eprintln!("{k}: {} files; first {}", v.len(), v[0]);
    }
}
