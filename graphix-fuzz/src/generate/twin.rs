//! Metamorphic twin generation: stateful handler modules whose state is
//! written through several equivalent routes (a `&` parameter, a
//! capture, a reference passed through a nested call), with an
//! in-program verdict that settles on `` `TwinDiverged `` when the
//! routes disagree ([`crate::TWIN_TAG`]). A reference-plumbing bug that
//! breaks every engine and route identically agrees with itself in
//! every pairwise comparison; only a program carrying its own invariant
//! can see it. Each program is one twin module plus a driver, in
//! schedule form (a `schedule-v1` header, in-language call) or callable
//! form (a `callable-v1` header, both routes). Every template quiesces
//! by construction.

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
    // both twins evaluate the identical expression, so an
    // overflow-to-bottom hits both sides alike
    let old = format!("s.{field}");
    match rng.below(5) {
        0 => format!("{old} + {arg}"),
        1 => format!("{old} - {arg}"),
        2 => format!("{old} + {arg} * i64:2"),
        3 => format!("{arg} - {old}"),
        _ => format!("{old} + i64:1"),
    }
}

/// The body of one inner update fn: a select over the dispatch arg with
/// a quiet arm and a writing arm. `target` is this twin's connect target.
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
    // The quiet arm decides the geometry: an arm matching the canonical
    // default leaves the writing arm asleep through the driver's init
    // call; a wildcard-only select updates on the init call too.
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
    // route 1: write through a & parameter
    let body_ref = gen_select_body(rng, &fields, "*st", "*st", "x");
    m.push_str(&format!("let inner_ref = |st: &St, x: i64| -> null {body_ref};\n"));
    // route 2: write through a capture; reuse route 1's body with the
    // targets swapped so the twins' select shapes stay identical
    let body_cap = body_ref.replace("*st", "sb");
    m.push_str(&format!("let inner_cap = |x: i64| -> null {body_cap};\n"));
    // route 3: the & parameter passed through a nested call
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

/// Render a twin shape as a schedule-form wrapper.
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

/// Render a twin shape as a callable-form wrapper.
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
