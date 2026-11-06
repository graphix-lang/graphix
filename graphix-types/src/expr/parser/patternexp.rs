use super::{
    ONE_TUPLE, choice, duration_unit_note, expr, finite, fldname, fname_noting, ident,
    input::{Input, R},
    int_range_note,
    interpolateexp::interpolated,
    is_reserved_binding, name, not_prefix, raw_string, sep_by_tok, sep_by1_tok,
    sort_unique,
    typexp::{typ, typath},
};
use crate::expr::{
    Expr, ExprKind, Name, Pattern, StructurePattern, WrittenAt, WrittenForm,
};
use arcstr::ArcStr;
use netidx_value::{Value, parser as value_parser};
use poolshark::local::LPooled;
use triomphe::Arc;

/// One element of a slice pattern: a pattern, or the rest `..` / `name..`.
enum SliceItem {
    Pat(StructurePattern),
    Rest(Option<Name>),
}

fn slice_item(i: &mut Input) -> R<SliceItem> {
    i.spaces();
    choice!(
        i,
        |i| i.string("..").map(|_| SliceItem::Rest(None)),
        |i| i.attempt(|i| {
            let n = name(i)?;
            i.spstring("..")?;
            Ok(SliceItem::Rest(Some(n)))
        }),
        |i| structure_pattern_or(i).map(SliceItem::Pat)
    )
}

/// Classify a slice-shaped pattern's element/rest mix into Slice /
/// SlicePrefix / SliceSuffix. `list` selects the native-list flavor
/// `[<..>]`, which refuses the suffix form (a list's front is an O(n) walk).
fn slice_pattern(i: &mut Input, list: bool, all: &Option<Name>) -> R<StructurePattern> {
    let (open, close) = if list { ("[<", ">]") } else { ("[", "]") };
    i.attempt(|i| i.string(open))?;
    let mut items =
        sep_by_tok(i, &slice_item, &|i| i.attempt(|i| i.spstring(close)).map(drop))?;
    i.spstring(close)?;
    let end = i.pos();
    let mut rest: Option<(usize, Option<Name>)> = None;
    let mut pats: LPooled<Vec<StructurePattern>> = LPooled::take();
    for (n, item) in items.drain(..).enumerate() {
        match item {
            SliceItem::Pat(p) => pats.push(p),
            SliceItem::Rest(r) if rest.is_none() => rest = Some((n, r)),
            SliceItem::Rest(_) => {
                return Err(i.refuse(end, "a slice pattern has one rest (`..`)"));
            }
        }
    }
    let n = pats.len();
    let pats = Arc::from_iter(pats.drain(..));
    let all = all.clone();
    Ok(match rest {
        None => StructurePattern::Slice { list, all, binds: pats },
        Some((0, _)) if n == 0 => {
            return Err(i.refuse(end, "a rest (`..`) alone is not a slice pattern"));
        }
        Some((0, _)) if list => {
            return Err(i.refuse(
                end,
                "list patterns have no suffix form (the tail is O(1), the front is not)",
            ));
        }
        Some((0, head)) => StructurePattern::SliceSuffix { all, head, suffix: pats },
        Some((k, tail)) if k == n => {
            StructurePattern::SlicePrefix { list, all, tail, prefix: pats }
        }
        Some(_) => {
            return Err(i.refuse(end, "a slice pattern's rest (`..`) is first or last"));
        }
    })
}

/// `( p, .. )` after `open`: one or more alternatives patterns.
fn bracketed_patterns(i: &mut Input) -> R<LPooled<Vec<StructurePattern>>> {
    i.token('(')?;
    let binds = sep_by1_tok(i, &structure_pattern_or, &|i| i.token(')').map(drop))?;
    i.sptoken(')')?;
    Ok(binds)
}

fn tuple_pattern(i: &mut Input, all: &Option<Name>) -> R<StructurePattern> {
    let mut binds = bracketed_patterns(i)?;
    let end = i.pos();
    match binds.len() < 2 {
        true => Err(i.refuse(end, ONE_TUPLE)),
        false => Ok(StructurePattern::Tuple {
            all: all.clone(),
            binds: Arc::from_iter(binds.drain(..)),
        }),
    }
}

fn variant_pattern(i: &mut Input, all: &Option<Name>) -> R<StructurePattern> {
    i.token('`')?;
    let tag = ident(i, true)?;
    let mut binds = i.optional(bracketed_patterns)?.unwrap_or_else(LPooled::take);
    Ok(StructurePattern::Variant {
        all: all.clone(),
        tag,
        binds: Arc::from_iter(binds.drain(..)),
    })
}

fn abstract_pattern(i: &mut Input, all: &Option<Name>) -> R<StructurePattern> {
    let (name, path_at) = i.attempt(|i| {
        let p = typath(i)?;
        i.sptoken('(')?;
        Ok(p)
    })?;
    let bind = structure_pattern_or(i)?;
    i.sptoken(')')?;
    Ok(StructurePattern::Abstract {
        all: all.clone(),
        name,
        at: path_at.0.first().map_or(WrittenAt::NOWHERE, |p| WrittenAt(*p)),
        bind: Arc::new(bind),
    })
}

/// One entry between a struct pattern's braces.
enum Field {
    /// `name: pattern`, or the shorthand `name` binding the field
    Named(ArcStr, StructurePattern, WrittenAt),
    /// `..`: the pattern is not exhaustive
    Rest,
}

fn struct_pattern_field(i: &mut Input) -> R<Field> {
    choice!(i, |i| i.string("..").map(|_| Field::Rest), |i| {
        let start = i.mark();
        let name = fldname(i)?;
        i.spaces();
        let pat = i.optional(|i| {
            i.token(':')?;
            structure_pattern_or(i)
        })?;
        let at = WrittenAt(start.pos);
        match pat {
            Some(pat) => Ok(Field::Named(name, pat, at)),
            None if is_reserved_binding(&name) => Err(i.refuse_from(
                start,
                start.pos,
                "a reserved word field needs the explicit `name: pattern` form",
            )),
            None => {
                let bind = StructurePattern::Bind(Name::written(name.clone(), start.pos));
                Ok(Field::Named(name, bind, at))
            }
        }
    })
}

fn struct_pattern(i: &mut Input, all: &Option<Name>) -> R<StructurePattern> {
    i.token('{')?;
    i.spaces();
    let mut fields = sep_by1_tok(i, &struct_pattern_field, &|i| i.token('}').map(drop))?;
    i.sptoken('}')?;
    let end = i.pos();
    // one rest, and it ends the fields
    let rests = fields.iter().filter(|f| matches!(f, Field::Rest)).count();
    if rests > 1 || (rests == 1 && !matches!(fields.last(), Some(Field::Rest))) {
        return Err(i.refuse(end, "`..` must be a struct pattern's last field"));
    }
    let mut binds: LPooled<Vec<(ArcStr, StructurePattern, WrittenAt)>> = fields
        .drain(..)
        .filter_map(|f| match f {
            Field::Named(n, p, at) => Some((n, p, at)),
            Field::Rest => None,
        })
        .collect();
    match sort_unique(&mut binds, |(n, _, _)| n) {
        true => Ok(StructurePattern::Struct {
            all: all.clone(),
            exhaustive: rests == 0,
            binds: Arc::from_iter(binds.drain(..)),
        }),
        false => Err(i.refuse(end, "struct fields must be unique")),
    }
}

/// A string pattern, lexed as an expression's string is, and how it was
/// delimited.
fn string_pattern(i: &mut Input) -> R<(Value, WrittenForm)> {
    let e: Expr = choice!(i, raw_string, interpolated)?;
    let end = i.pos();
    match &e.kind {
        ExprKind::Constant(v @ Value::String(_)) => {
            Ok((v.clone(), WrittenForm(e.str_form)))
        }
        _ => Err(i.refuse(end, "a string pattern cannot interpolate")),
    }
}

// XCR claude for claude: [bug/low] A value literal failing inside a pattern
// or an index still loses what its parser said: `select x { error:"\q" =>
// 1, _ => 0 }` reports "Unexpected `:` Expected `|` or =>" at the colon, and
// `x.99999999999999999999999` "Unexpected `.`", where the expression
// `error:"\q"` names the escapes (value-literal-labels-01). The report stands
// short of the literal's failure, so State's literal fallback never meets
// it; unlike the expression's, this value_part is in no knot, which may be
// why. Main 968fa9bc0 says the same. Found by review r14 of lane K. probes:
// ~/ptmp/cr29/scratch/review-r14/K-r1-2/pat_errq.gx, lit/idx_big.gx
// (literal-pattern-labels-01)
// 2026-11-05 claude: A pattern's value literal (Input::literal_part, so a
// spaced label stays quiet) and a tuple index (value_part of the usize) now
// read inside a knot, as the expression's literal does, so the report stands at
// the literal's failure and State's literal fallback says it: select x {
// error:"\q" => 1 } names the escapes at the q, x.99999999999999999999999
// 'invalid signed integer' at its end, u8:300 as a pattern carries the range
// note. Pin:
// parser::test::a_value_literal_failure_is_said_in_a_pattern_and_an_index
// (fails without the fix). Census: 2942 files, one message change (the u8:300
// pattern repro, now the expression's generic text plus note); no verdict
// changes. Diagnostics only, no soak.
// 2026-11-05 claude: Repair (review r17): (1) the knot around a pattern's
// literal moved the report to where a reserved word, read as a literal's head,
// failed: `let bytes = 1;` and `select x { bytes => 1 }` lost "can't use
// keyword as a function or variable name". A pattern literal whose parser fails
// having read no more than a word and its colon (Input::fails_before_payload)
// is read without the knot, as before. (2) The tuple index's knot moved `x.`,
// `x.0 + x.`, `(x).` and `5..q` to generic text a column on; it is entered only
// where a digit or `-` follows the dot. The usize index pin asserts the
// position only (netidx says "invalid signed integer" for it). `select x {
// bytes: n => 1 }` and `(bytes: n, 1)` now say the bytes literal's failure at
// its payload, as the expression `f(bytes: x)` does (judged: the colon makes it
// a literal). Pin that fails without the repair:
// parser::test::a_word_short_of_a_literal_is_a_name.
// 2026-11-05 claude: Repair (review r18): an annotated binder named bytes (let
// bytes: i64 = 1, |bytes: bytes| .., |bytes: i64| ..) and the pattern forms
// bytes: n, (bytes: n, 1), [bytes: n] and let (bytes: n, m) entered the bytes
// literal, which read the name after the colon as base64 and said its failure,
// where main names the keyword. Input::fails_before_payload now also takes a
// word, its colon and a name after it (a letter or _ first) for a name, so
// those read without the knot and keep the reserved-word refusal; a payload
// that starts as a literal's (u8:300) still enters it. The tuple index enters
// its knot only where a digit follows the dot, so x.-1 says Unexpected . at the
// dot as main does. Pin that fails without the repair:
// parser::test::a_word_short_of_a_literal_is_a_name (its new rows). The
// judgment in the r17 note that bytes: n and (bytes: n, 1) say the literal's
// failure is reversed; the expression f(bytes: x) still does. Left: `A(u8:300)
// and {a: u8:300} in a pattern say the generic text with the range note, where
// main says Expected , or ): the literal failure said in a pattern is this CR's
// change.
fn literal_pattern(i: &mut Input) -> R<StructurePattern> {
    choice!(
        i,
        |i| {
            let start = i.mark();
            let (v, form) = choice!(i, string_pattern, |i| {
                let literal = |i: &mut Input| i.literal_part(value_parser::value);
                match i.fails_before_payload(value_parser::value) {
                    true => i.attempt(literal),
                    false => i.attempt(|i| i.knot(literal)),
                }
                .map(|v| (v, WrittenForm::default()))
            })?;
            not_prefix(i)?;
            finite(i, start, v).map(|v| StructurePattern::Literal(v, form))
        },
        |i| i.attempt(int_range_note),
        |i| i.attempt(|i| i.knot(duration_unit_note))
    )
}

fn all_pattern(i: &mut Input) -> R<Name> {
    // a probe at every pattern: a reserved word here is a literal's
    let pos = i.pos();
    let n = fname_noting(i, false)?;
    i.sptoken('@')?;
    i.spaces();
    Ok(Name::written(n, pos))
}

pub(super) fn structure_pattern(i: &mut Input) -> R<StructurePattern> {
    i.knot(|i| {
        i.spaces();
        let all = i.optional(|i| i.attempt(all_pattern))?;
        let all = &all;
        let pat = choice!(
            i,
            |i| slice_pattern(i, true, all),
            |i| slice_pattern(i, false, all),
            |i| tuple_pattern(i, all),
            |i| struct_pattern(i, all),
            |i| variant_pattern(i, all),
            |i| abstract_pattern(i, all),
            |i| i.token('_').map(|_| StructurePattern::Ignore),
            literal_pattern,
            |i| name(i).map(StructurePattern::Bind)
        )?;
        let end = i.pos();
        match pat {
            StructurePattern::Ignore
            | StructurePattern::Literal(..)
            | StructurePattern::Bind(_)
                if all.is_some() =>
            {
                Err(i.refuse(end, "a capture `name@` takes a structure pattern"))
            }
            pat => Ok(pat),
        }
    })
}

/// One or more `|`-separated alternatives. Legal in select arms and every
/// bracketed element position, not at the top level of `let` or lambda
/// params. `|` binds loosest; an `@`-capture is per-alternative; flat.
pub(super) fn structure_pattern_or(i: &mut Input) -> R<StructurePattern> {
    let first = structure_pattern(i)?;
    let mut alts: LPooled<Vec<StructurePattern>> = LPooled::take();
    i.many(
        |i| {
            i.attempt(|i| {
                i.sptoken('|')?;
                structure_pattern(i)
            })
        },
        |p| alts.push(p),
    )?;
    Ok(match alts.is_empty() {
        true => first,
        false => StructurePattern::Or(Arc::from_iter(
            std::iter::once(first).chain(alts.drain(..)),
        )),
    })
}

pub(super) fn pattern(i: &mut Input) -> R<Pattern> {
    let pos = i.pos();
    let type_predicate = i.optional(|i| {
        i.attempt(|i| {
            let t = typ(i)?;
            i.spaces1()?;
            i.word_space("as")?;
            i.spaces();
            Ok(t)
        })
    })?;
    let structure_predicate = structure_pattern_or(i)?;
    let end = i.pos();
    let guard = i.optional(|i| {
        i.attempt(|i| {
            i.spaces1()?;
            i.word_space("if")?;
            i.spaces();
            expr(i)
        })
    })?;
    Ok(Pattern {
        type_predicate,
        structure_predicate,
        guard,
        pos: WrittenAt(pos),
        end: WrittenAt(end),
    })
}
