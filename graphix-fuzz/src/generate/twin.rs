//! Metamorphic twin generation: stateful handler modules whose state is
//! written through several equivalent routes (a `&mut` parameter, a
//! capture, a reference passed through a nested call), with an
//! in-program verdict that settles on `` `TwinDiverged `` when the
//! routes disagree ([`crate::TWIN_TAG`]). A reference-plumbing bug that
//! breaks every engine and route identically agrees with itself in
//! every pairwise comparison; only a program carrying its own invariant
//! can see it. Each program is one twin module plus a driver, in
//! schedule form (a `schedule-v1` header, in-language call) or callable
//! form (a `callable-v1` header, both routes). Every template quiesces
//! by construction.

use super::chance;
use crate::callable::CallSpec;
use crate::mutate::Rng;
use crate::schedule::{Lit, Schedule};

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
    pub epochs: Vec<Vec<Lit>>,
}

const FIELDS: [&str; 3] = ["a", "b", "c"];

fn gen_update(rng: &mut Rng, field: &str, arg: &str) -> String {
    // both twins evaluate the identical expression
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
    if chance(rng, 0.7) {
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
    let three = chance(rng, 0.4);
    let by_field = chance(rng, 0.5);
    let mut m = String::new();
    m.push_str(&format!("type St = {{ {st_ty} }};\n"));
    let mut states = vec!["sa", "sb"];
    if three {
        states.push("sc");
    }
    if by_field {
        states.push("sd");
    }
    for st in &states {
        m.push_str(&format!("let {st}: St = {{ {init} }};\n"));
    }
    // route 1: write through a &mut parameter
    let body_ref = gen_select_body(rng, &fields, "*st", "*st", "x");
    m.push_str(&format!("let inner_ref = |st: &mut St, x: i64| -> null {body_ref};\n"));
    // route 2: write through a capture; reuse route 1's body with the
    // targets swapped so the twins' select shapes stay identical
    let body_cap = body_ref.replace("*st", "sb");
    m.push_str(&format!("let inner_cap = |x: i64| -> null {body_cap};\n"));
    let mut calls = vec!["let ra = inner_ref(&mut sa, x)", "let rb = inner_cap(x)"];
    // route 3: the &mut parameter passed through a nested call
    if three {
        m.push_str(&format!(
            "let inner_deep0 = |st: &mut St, x: i64| -> null {body_ref};\n"
        ));
        m.push_str(
            "let inner_deep = |st: &mut St, x: i64| -> null inner_deep0(st, x);\n",
        );
        calls.push("let rc = inner_deep(&mut sc, x)");
    }
    // route 4: field by field through place references into sd, each
    // write patching the root at delivery
    if by_field {
        for f in &fields {
            m.push_str(&format!("let pf_{0} = &mut sd.{0};\n", f.name));
        }
        let writes = fields
            .iter()
            .map(|f| format!("*pf_{} <- {}", f.name, f.update))
            .collect::<Vec<_>>()
            .join(";\n    ");
        let write = format!("let s = n ~ sd;\n    {writes};\n    null");
        let body = if body_ref.contains("i64:0 => null") {
            format!("select x {{\n  i64:0 => null,\n  n => {{\n    {write}\n  }}\n}}")
        } else {
            format!("select x {{\n  n => {{\n    {write}\n  }}\n}}")
        };
        m.push_str(&format!("let inner_fields = |x: i64| -> null {body};\n"));
        calls.push("let rd = inner_fields(x)");
    }
    m.push_str(&format!(
        "let handler = |x: i64| -> null {{\n  {};\n  null\n}};\n",
        calls.join(";\n  ")
    ));
    let names: Vec<String> = (0..states.len()).map(|i| format!("t{i}")).collect();
    let agree =
        names.windows(2).map(|w| format!("{} == {}", w[0], w[1])).collect::<Vec<_>>();
    let verdict = format!(
        "select ({st}) {{\n  ({n}) if {agree} => `Ok(t0),\n  ({n}) => `TwinDiverged(({n}))\n}}",
        st = states.join(", "),
        n = names.join(", "),
        agree = agree.join(" && ")
    );
    m.push_str(&format!("let verdict = {verdict}\n"));
    let nepochs = 1 + rng.below(3);
    let epochs =
        (0..nepochs).map(|_| vec![Lit::I64((rng.below(37) as i64) - 5)]).collect();
    TwinShape { module: m, args: vec![("cx0", "i64")], epochs }
}

/// Render a twin shape as a schedule-form wrapper.
pub fn render_schedule_form(shape: &TwinShape) -> String {
    let sched = Schedule {
        epochs: shape
            .epochs
            .iter()
            .map(|vals| {
                vals.iter().enumerate().map(|(i, v)| (format!("in{i}"), *v)).collect()
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
                    .map(|(v, (name, _))| (name.to_string(), *v))
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
    if chance(rng, 0.5) {
        render_schedule_form(&shape)
    } else {
        render_callable_form(&shape)
    }
}

