//! A lambda definition under an image is data: the source body, the
//! environment snapshot, the scope, the checked scheme and the
//! analysis facts. The `init` closure is rebuilt from that data by the
//! same function `Lambda::compile` uses, and a builtin's retained check
//! `Apply` is rebuilt on first use.

use super::{
    env::{lexical_decode, lexical_encode, lexical_len},
    flags_decode, flags_encode, flags_len, scope_decode, scope_encode, scope_len,
};
use crate::{
    ExecCtx, LambdaId, Rt, UserEvent,
    expr::{Arg, Expr},
    node::lambda::{DefBody, DefOrigin, LambdaDef, make_init},
    typ::FnType,
};
use bytes::{Buf, BufMut};
use netidx_core::pack::{Pack, PackError};
use parking_lot::Mutex;
use std::sync::atomic::{AtomicBool, Ordering};
use triomphe::Arc;

fn body_len(body: &DefBody) -> usize {
    1 + match body {
        DefBody::Expr(e) => e.encoded_len(),
        DefBody::BuiltIn(name) => name.encoded_len(),
        DefBody::Collection(intrinsic) => intrinsic.encoded_len(),
    }
}

fn body_encode(body: &DefBody, buf: &mut impl BufMut) -> Result<(), PackError> {
    match body {
        DefBody::Expr(e) => {
            buf.put_u8(0);
            e.encode(buf)
        }
        DefBody::BuiltIn(name) => {
            buf.put_u8(1);
            name.encode(buf)
        }
        DefBody::Collection(intrinsic) => {
            buf.put_u8(2);
            intrinsic.encode(buf)
        }
    }
}

fn body_decode(buf: &mut impl Buf) -> Result<DefBody, PackError> {
    if !buf.has_remaining() {
        return Err(PackError::BufferShort);
    }
    match buf.get_u8() {
        0 => Ok(DefBody::Expr(Pack::decode(buf)?)),
        1 => Ok(DefBody::BuiltIn(Pack::decode(buf)?)),
        2 => Ok(DefBody::Collection(Pack::decode(buf)?)),
        _ => Err(PackError::UnknownTag),
    }
}

pub(crate) fn def_len<R: Rt, E: UserEvent>(def: &LambdaDef<R, E>) -> usize {
    let LambdaDef {
        id,
        env,
        scope,
        argspec,
        typ,
        init: _,
        check: _,
        intrinsic_effect,
        stateless,
        recursion,
        source,
        origin,
    } = def;
    let DefOrigin::Source { body, flags, spec } = origin else { return 0 };
    id.encoded_len()
        + lexical_len(env)
        + scope_len(scope)
        + argspec.encoded_len()
        + typ.encoded_len()
        + intrinsic_effect.lock().encoded_len()
        + stateless.load(Ordering::Relaxed).encoded_len()
        + recursion.lock().encoded_len()
        + source.encoded_len()
        + body_len(body)
        + flags_len(*flags)
        + spec.encoded_len()
}

pub(crate) fn def_encode<R: Rt, E: UserEvent>(
    def: &LambdaDef<R, E>,
    buf: &mut impl BufMut,
) -> Result<(), PackError> {
    let LambdaDef {
        id,
        env,
        scope,
        argspec,
        typ,
        init: _,
        check: _,
        intrinsic_effect,
        stateless,
        recursion,
        source,
        origin,
    } = def;
    // a runtime-built definition exists only once a cycle ran
    let DefOrigin::Source { body, flags, spec } = origin else {
        return Err(PackError::Application(super::NOT_QUIESCENT));
    };
    id.encode(buf)?;
    lexical_encode(env, buf)?;
    scope_encode(scope, buf)?;
    argspec.encode(buf)?;
    typ.encode(buf)?;
    intrinsic_effect.lock().encode(buf)?;
    stateless.load(Ordering::Relaxed).encode(buf)?;
    recursion.lock().encode(buf)?;
    source.encode(buf)?;
    body_encode(body, buf)?;
    flags_encode(*flags, buf)?;
    spec.encode(buf)
}

/// Rebuild a definition and register it in `ctx.lambda_defs`.
pub(crate) fn def_decode<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
    buf: &mut impl Buf,
) -> Result<LambdaId, PackError> {
    let id = LambdaId::decode(buf)?;
    let env = lexical_decode(buf)?;
    let scope = scope_decode(buf)?;
    let argspec: Arc<[Arg]> = Pack::decode(buf)?;
    let typ: Arc<FnType> = Pack::decode(buf)?;
    let intrinsic_effect = Pack::decode(buf)?;
    let stateless = bool::decode(buf)?;
    let recursion = Pack::decode(buf)?;
    let source = Pack::decode(buf)?;
    let body = body_decode(buf)?;
    let flags = flags_decode(buf)?;
    let spec: Expr = Pack::decode(buf)?;
    let init = make_init(
        id,
        flags,
        env.clone(),
        &scope,
        typ.clone(),
        argspec.clone(),
        spec.clone(),
        body.clone(),
    );
    ctx.wrap_lambda(LambdaDef {
        id,
        env,
        scope,
        argspec,
        typ,
        init,
        check: Mutex::new(None),
        intrinsic_effect: Mutex::new(intrinsic_effect),
        stateless: AtomicBool::new(stateless),
        recursion: Mutex::new(recursion),
        source,
        origin: DefOrigin::Source { body, flags, spec },
    });
    Ok(id)
}
