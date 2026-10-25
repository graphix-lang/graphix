use graphix_compiler::rep;
use netidx_value::Value;

fn fc_is_some(args: &[Value]) -> Option<Value> {
    match &args[0] {
        Value::Null => Some(Value::Bool(false)),
        _ => Some(Value::Bool(true)),
    }
}

crate::fast_builtin!(pub(crate) IsSome, IsSomeEv, "core_opt_is_some", fc_is_some);

fn fc_is_none(args: &[Value]) -> Option<Value> {
    match &args[0] {
        Value::Null => Some(Value::Bool(true)),
        _ => Some(Value::Bool(false)),
    }
}

crate::fast_builtin!(pub(crate) IsNone, IsNoneEv, "core_opt_is_none", fc_is_none);

/// `a`, or `b` when `a` is null: `or` and `or_default` alike.
fn fc_or(args: &[Value]) -> Option<Value> {
    match &args[..] {
        [Value::Null, b] => Some(b.clone()),
        [a, _] => Some(a.clone()),
        _ => None,
    }
}

crate::fast_builtin!(pub(crate) OrDefault, OrDefaultEv, "core_opt_or_default", fc_or);
crate::fast_builtin!(pub(crate) Or, OrEv, "core_opt_or", fc_or);

fn fc_and(args: &[Value]) -> Option<Value> {
    match &args[..] {
        [Value::Null, _] => Some(Value::Null),
        [_, b] => Some(b.clone()),
        _ => None,
    }
}

crate::fast_builtin!(pub(crate) And, AndEv, "core_opt_and", fc_and);

fn fc_xor(args: &[Value]) -> Option<Value> {
    let (a, b) = (&args[0], &args[1]);
    let a_some = !matches!(a, Value::Null);
    let b_some = !matches!(b, Value::Null);
    Some(if a_some && !b_some {
        a.clone()
    } else if !a_some && b_some {
        b.clone()
    } else {
        Value::Null
    })
}

crate::fast_builtin!(pub(crate) Xor, XorEv, "core_opt_xor", fc_xor);

fn fc_zip(args: &[Value]) -> Option<Value> {
    match &args[..] {
        [Value::Null, _] | [_, Value::Null] => Some(Value::Null),
        [a, b] => Some(rep::tuple([a.clone(), b.clone()])),
        _ => None,
    }
}

crate::fast_builtin!(pub(crate) Zip, ZipEv, "core_opt_zip", fc_zip);

fn fc_unzip(args: &[Value]) -> Option<Value> {
    match &args[0] {
        Value::Null => Some(rep::tuple([Value::Null, Value::Null])),
        p if rep::tuple_elems(p).is_some_and(|e| e.len() == 2) => Some(p.clone()),
        _ => None,
    }
}

crate::fast_builtin!(pub(crate) Unzip, UnzipEv, "core_opt_unzip", fc_unzip);

fn fc_ok_or(args: &[Value]) -> Option<Value> {
    match &args[..] {
        [Value::Null, e] => Some(Value::Error(e.clone().into())),
        [v, _] => Some(v.clone()),
        _ => None,
    }
}

crate::fast_builtin!(pub(crate) OkOr, OkOrEv, "core_opt_ok_or", fc_ok_or);
