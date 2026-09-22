//! Every expression the parser builds knows where its text is: over
//! the book's examples and the stdlib, the slice `[pos, end)` of each
//! parsed node parses back to that node.

use arcstr::ArcStr;
use graphix_compiler::expr::{Expr, ExprKind, Origin, Source, WrittenAt, parser};
use std::{
    collections::BTreeMap,
    fs,
    path::{Path, PathBuf},
};

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

/// Byte offset of a 1-based (line, char column) position.
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

/// `None` when the node's span is right. An empty statement is no
/// text; a run of text inside an interpolated string is text, and
/// only has to lie inside the string.
fn wrong(text: &str, e: &Expr, parent: Option<&Expr>) -> Option<String> {
    if e.end.0 == WrittenAt::NOWHERE.0 {
        return Some("no end".into());
    }
    let at = |p: graphix_compiler::SourcePosition| offset(text, p.line, p.column);
    let (from, to) = (at(e.pos), at(e.end.0));
    if matches!(e.kind, ExprKind::NoOp) {
        return (from != to).then(|| "an empty statement has text".into());
    }
    if from >= to {
        return Some(format!("empty span {}..{}", e.pos, e.end.0));
    }
    if let (ExprKind::Constant(_), Some(p)) = (&e.kind, parent)
        && matches!(p.kind, ExprKind::StringInterpolate { .. })
    {
        let inside = at(p.pos) < from && to < at(p.end.0);
        return (!inside).then(|| "a text run outside its string".into());
    }
    let slice = &text[from..to];
    match parser::parse_one(slice) {
        Ok(back) if back == *e => None,
        Ok(_) => Some(format!("reads back as something else: `{slice}`")),
        Err(_) => Some(format!("does not parse: `{slice}`")),
    }
}

fn check(
    text: &str,
    e: &Expr,
    parent: Option<&Expr>,
    failures: &mut Vec<(String, String)>,
) -> usize {
    if let Some(why) = wrong(text, e, parent) {
        failures.push((kind_name(&e.kind), format!("{} {why}", e.pos)));
    }
    let mut n = 1;
    e.for_each_child(&mut |c| n += check(text, c, Some(e), failures));
    n
}

#[test]
fn every_span_reads_back_as_its_node() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("..");
    let mut files = vec![];
    for dir in ["book/src/examples", "stdlib", "bench"] {
        sources(&root.join(dir), &mut files);
    }
    assert!(files.len() > 100, "{}", files.len());
    let mut failures: BTreeMap<String, Vec<String>> = BTreeMap::new();
    let mut checked = 0usize;
    for file in files {
        let text = ArcStr::from(fs::read_to_string(&file).unwrap());
        let ori = Origin {
            parent: None,
            source: Source::File(file.clone()),
            text: text.clone(),
        };
        // examples may be snippets that do not parse on their own
        let Ok(exprs) = parser::parse(ori) else { continue };
        let mut found = vec![];
        for top in exprs.iter() {
            checked += check(&text, top, None, &mut found);
        }
        for (kind, why) in found {
            failures.entry(kind).or_default().push(format!("{}:{why}", file.display()));
        }
    }
    assert!(checked > 10_000, "{checked}");
    let report: Vec<String> = failures
        .iter()
        .map(|(kind, at)| format!("{kind} x{}: {}", at.len(), &at[0]))
        .collect();
    assert!(failures.is_empty(), "{checked} checked\n{}", report.join("\n"));
}
