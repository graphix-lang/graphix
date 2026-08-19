//! Metamorphic twin generation — stateful handler modules whose state
//! is written through SEVERAL equivalent routes (a `&` reference
//! parameter, a capture, a reference passed through a nested call),
//! with an in-program verdict that settles on `` `TwinDiverged `` when
//! the routes disagree (the reserved [`crate::TWIN_TAG`] contract).
//!
//! The point is symmetric-bug coverage: a reference-plumbing bug that
//! breaks every engine and route IDENTICALLY (the ConnectDeref
//! silent-write class, 9f9e01d0) agrees with itself in every pairwise
//! comparison — only a program that carries its own invariant can see
//! it, in a single run of a single mode.
//!
//! Each generated program is one twin MODULE (a `file-v1` section)
//! plus a driver, emitted in one of two forms:
//!
//! - **schedule form**: a `schedule-v1` header injects the handler's
//!   arguments; the body calls the handler in-language. Exercises the
//!   pure-language reference paths (this shape, with the write inside
//!   a select arm the init call leaves asleep, reproduces 9f9e01d0's
//!   pure-language face).
//! - **callable form**: a `callable-v1` header dispatches the handler
//!   through `GXHandle::compile_callable` as well (see
//!   [`crate::callable`]); the twin scan then covers the embedder
//!   dispatch route, and the route/engine pairs run too.
//!
//! Every template quiesces by construction: state only moves when a
//! dispatch arrives, and both twins of a pair perform the SAME update
//! from the SAME dispatch cycle, so the verdict settles by the epoch's
//! quiescence wait (transient skew within a cycle never reaches a
//! final).

use netidx::publisher::Value;

use crate::callable::CallSpec;
use crate::mutate::Rng;
use crate::schedule::Schedule;

fn chance(rng: &mut Rng, pct: usize) -> bool {
    rng.below(100) < pct
}

/// One generated state field: name and an update expression over the
/// old field value `{old}` and the dispatch argument `{arg}`.
struct Field {
    name: &'static str,
    update: String,
}

/// The generated twin shape: the module text, the handler path, the
/// argument names/types, and the dispatch values per epoch.
pub struct TwinShape {
    pub module: String,
    pub args: Vec<(&'static str, &'static str)>,
    pub epochs: Vec<Vec<Value>>,
}

const FIELDS: [&str; 3] = ["a", "b", "c"];

fn gen_update(rng: &mut Rng, field: &str, arg: &str) -> String {
    // Wrapping-total i64 arithmetic over the old value and the arg —
    // both twins evaluate the identical expression, so even an
    // overflow-to-bottom (unchecked ops log and bottom) hits both
    // sides alike and the verdict stays quiet.
    let old = format!("s.{field}");
    match rng.below(5) {
        0 => format!("{old} + {arg}"),
        1 => format!("{old} - {arg}"),
        2 => format!("{old} + {arg} * i64:2"),
        3 => format!("{arg} - {old}"),
        _ => format!("{old} + i64:1"),
    }
}

/// The body of one inner update fn: a select over the dispatch arg
/// with a quiet arm and a writing arm, the write built from `fields`.
/// `write` renders the connect target for this twin's route (`*st` or
/// the captured binding name).
fn gen_select_body(
    rng: &mut Rng,
    fields: &[Field],
    read: &str,
    target: &str,
    arg: &str,
) -> String {
    let upd = fields
        .iter()
        .map(|f| format!("{}: {}", f.name, f.update))
        .collect::<Vec<_>>()
        .join(", ");
    let write = format!("let s = n ~ {read};\n    {target} <- {{ {upd} }};\n    null");
    // The quiet arm decides the bug GEOMETRY: an arm matching the
    // canonical default leaves the writing arm asleep through the
    // driver's init call — the lazy-wake shape 9f9e01d0 needed. A
    // wildcard-only select updates on the init call too (both twins
    // alike). Generate both.
    if chance(rng, 70) {
        format!("select {arg} {{\n  i64:0 => null,\n  n => {{\n    {write}\n  }}\n}}")
    } else {
        format!("select {arg} {{\n  n => {{\n    {write}\n  }}\n}}")
    }
}

/// Generate one twin module + its dispatch plan. `nfields` state
/// fields, 2 or 3 twin routes, 1-3 dispatch epochs.
pub fn gen_twin_shape(rng: &mut Rng) -> TwinShape {
    let nfields = 1 + rng.below(3);
    let fields: Vec<Field> = FIELDS[..nfields]
        .iter()
        .map(|name| Field { name, update: gen_update(rng, name, "n") })
        .collect();
    let st_ty =
        fields.iter().map(|f| format!("{}: i64", f.name)).collect::<Vec<_>>().join(", ");
    let init = fields
        .iter()
        .map(|f| format!("{}: i64:0", f.name))
        .collect::<Vec<_>>()
        .join(", ");
    let three = chance(rng, 40);
    let mut m = String::new();
    m.push_str(&format!("type St = {{ {st_ty} }};\n"));
    m.push_str(&format!("let sa: St = {{ {init} }};\n"));
    m.push_str(&format!("let sb: St = {{ {init} }};\n"));
    if three {
        m.push_str(&format!("let sc: St = {{ {init} }};\n"));
    }
    // Route 1: write through a & parameter.
    let body_ref = gen_select_body(rng, &fields, "*st", "*st", "x");
    m.push_str(&format!("let inner_ref = |st: &St, x: i64| -> null {body_ref};\n"));
    // Route 2: write through a capture. The SAME rng state must not
    // desync the twins' select shape, so reuse route 1's body with the
    // targets swapped rather than re-generating.
    let body_cap = body_ref.replace("*st", "sb");
    m.push_str(&format!("let inner_cap = |x: i64| -> null {body_cap};\n"));
    // Route 3: the & parameter passed through a nested call.
    if three {
        m.push_str(&format!("let inner_deep0 = |st: &St, x: i64| -> null {body_ref};\n"));
        m.push_str("let inner_deep = |st: &St, x: i64| -> null inner_deep0(st, x);\n");
    }
    let calls = if three {
        "let ra = inner_ref(&sa, x);\n  let rb = inner_cap(x);\n  \
         let rc = inner_deep(&sc, x);\n  null"
    } else {
        "let ra = inner_ref(&sa, x);\n  let rb = inner_cap(x);\n  null"
    };
    m.push_str(&format!("let handler = |x: i64| -> null {{\n  {calls}\n}};\n"));
    let verdict = if three {
        "select (sa, sb, sc) {\n  (a, b, c) if a == b && b == c => `Ok(a),\n  \
         (a, b, c) => `TwinDiverged((a, b, c))\n}"
    } else {
        "select (sa, sb) {\n  (a, b) if a == b => `Ok(a),\n  \
         (a, b) => `TwinDiverged((a, b))\n}"
    };
    m.push_str(&format!("let verdict = {verdict}\n"));
    let nepochs = 1 + rng.below(3);
    let epochs =
        (0..nepochs).map(|_| vec![Value::I64((rng.below(37) as i64) - 5)]).collect();
    TwinShape { module: m, args: vec![("cx0", "i64")], epochs }
}

/// Render a twin shape as a SCHEDULE-form wrapper: injections drive
/// the in-language call.
pub fn render_schedule_form(shape: &TwinShape) -> String {
    let sched = Schedule {
        epochs: shape
            .epochs
            .iter()
            .map(|vals| {
                vals.iter()
                    .enumerate()
                    .map(|(i, v)| (format!("in{i}"), v.clone()))
                    .collect()
            })
            .collect(),
        ..Schedule::default()
    };
    let params =
        (0..shape.args.len()).map(|i| format!("in{i}")).collect::<Vec<_>>().join(", ");
    let body = format!(
        "{{ let r = m0::handler({params}); m0::verdict }}\n// file-v1: m0.gx\n{}",
        shape.module
    );
    sched.render(&body)
}

/// Render a twin shape as a CALLABLE-form wrapper: the harness
/// synthesizes the driver and dispatches through both routes.
pub fn render_callable_form(shape: &TwinShape) -> String {
    let spec = CallSpec {
        handler: "m0::handler".into(),
        epochs: shape
            .epochs
            .iter()
            .map(|vals| {
                vals.iter()
                    .zip(shape.args.iter())
                    .map(|(v, (name, _))| (name.to_string(), v.clone()))
                    .collect()
            })
            .collect(),
    };
    let body =
        format!("{{ let o = m0::verdict; o }}\n// file-v1: m0.gx\n{}", shape.module);
    spec.render(&body)
}

/// Generate one twin program: schedule form or callable form.
pub fn gen_twin_program(rng: &mut Rng) -> String {
    let shape = gen_twin_shape(rng);
    if chance(rng, 50) {
        render_schedule_form(&shape)
    } else {
        render_callable_form(&shape)
    }
}
