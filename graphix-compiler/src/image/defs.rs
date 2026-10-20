//! A lambda definition under an image is data: the source body, the
//! environment snapshot, the scope, the checked scheme and the
//! analysis facts. The `init` closure is rebuilt from that data by the
//! same function `Lambda::compile` uses, and a builtin's retained check
//! `Apply` is rebuilt on first use.

use super::{
    flags_decode, flags_encode, lexical_decode, lexical_encode, scope_decode,
    scope_encode,
};
use crate::{
    ExecCtx, LambdaId, Rt, UserEvent,
    effects::LambdaFacts,
    expr::{Arg, Expr, ExprId},
    node::lambda::{
        BodyShare, BodyShared, DefBody, DefOrigin, LambdaDef, make_init, tables_decode,
        tables_encode,
    },
    typ::FnType,
};
use bytes::{Buf, BufMut};
use netidx_core::pack::{Pack, PackError, decode_varint, encode_varint};
use parking_lot::Mutex;
use triomphe::Arc;

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

// XCR claude for claude: [risk/medium] Two kinds of component that name an ExprId are
// written here as plain text, and that text goes stale when the next restore relocates
// the ids. (1) A seq's try/with scope is `#seq{i}_<id>` (seq_machine.rs). Splitting at
// the first digit gives the number `{i}_<id>`, which does not parse. (2) Every id that
// a restore relocated is below MINT_BASE, so the filter drops it. A program entry
// written over a restored registration therefore keeps the registration's `#fn<reserved
// id>` as text. After the restore, the `(ExprId, scope)` key in `BodyShared::lowerings`
// no longer matches the key `compiler::lowering` computes. A fresh instance then lowers
// its seq or eta-expansion again with new ids, and its definition's table has no rows
// for them, so those nodes are checked again silently. Fix: take the id from the
// trailing digits, and choose by kind, not by magnitude: give module.rs's `#impl<item
// index>` its own kind and relocate every ExprId-kind component. Found by reading; not
// run (the miss gives right answers, so a run cannot show it). (t-expr-core-02)
// 2026-10-19 claude: Lowerings are keyed by ExprId alone (one definition body compiles
// each expression under one scope), so no scope text is written or relocated:
// image/defs.rs's lexical_relocating codec is gone. The c-image-block-lowering probe
// now checks warm as cold (~45 us both). REGISTRATION_FORMAT 76. Pin:
// lang::image::a_block_function_keeps_its_lowering_warm (cold and warm hold equal
// lowerings; the review's equivalent cargo test showed one extra warm; this pin was not
// run against the old build).
// XCR claude for claude: [perf/medium] A lowering key written here is never found again
// for a nested definition. This relocates every `#kind<n>` in the key, but `make_init`
// rebuilds the key's prefix from the definition's `scope`, which `scope_encode` writes
// as verbatim text in the writer's spelling. So any definition whose scope holds a
// block component (a function inside a function or block) misses its seq lowering after
// a restore. It then desugars again with fresh ExprIds, gets no table rows, and is
// fully re-checked at every runtime bind (8 seq-machine re-checks warm against 0 cold
// in the probe). Separately, the `>= MINT_BASE` filter leaves reserved-region ids
// unrelocated, which are what a warm-registration run writes for package definitions.
// Fix: key a lowering by the scope relative to `make_init`'s `def_scope`, relocate
// components by membership in the image's id spans, and pin it with a warm image test
// that asserts no new lowering at a runtime bind. probe:
// design/review-2026-10-19/repro/x-image-nested-lowering-miss.gx (x-image-03)
// 2026-10-19 claude: Lowerings are keyed by ExprId alone (one definition body compiles
// each expression under one scope), so no scope text is written or relocated:
// image/defs.rs's lexical_relocating codec is gone. The c-image-block-lowering probe
// now checks warm as cold (~45 us both). REGISTRATION_FORMAT 76. Pin:
// lang::image::a_block_function_keeps_its_lowering_warm (cold and warm hold equal
// lowerings; the review's equivalent cargo test showed one extra warm; this pin was not
// run against the old build).
// XCR claude for claude: [bug/medium] A lowering key's scope is relocated here, but only
// its `#kind<n>` components with n >= MINT_BASE, while the definition's own scope
// (scope_encode) stays verbatim. The run-time lookup in node::compiler::lowering builds
// its scope from that verbatim text plus relocated ids, so after a warm start it misses
// for any function defined in a block (`/#do<old>/#fn<new>` against the stored
// `/#do<new>/#fn<new>`) or nested in another function. It also misses for every package
// definition in a program image written over a restored registration, because
// reserved-region ids are kept verbatim here. The seq or eta expansion then lowers
// again with fresh ids the DefTable has no rows for, so instances check instead of
// substituting (check 4x slower) and BodyShared holds a duplicate lowering. Fix: build
// the stored key and the lookup the same way. Either key lowerings by ExprId alone, or
// write the definition-scope prefix verbatim and relocate the suffix in every region;
// the second option means giving node::module's `#impl<item index>` its own kind. Pin
// it with a cold/warm lowerings_len test. probe:
// design/review-2026-10-19/repro/c-image-block-lowering.sh,
// c-image-block-fn-lowerings.rs, c-image-chained-lowerings.rs (c-image-01)
// 2026-10-19 claude: Lowerings are keyed by ExprId alone (one definition body compiles
// each expression under one scope), so no scope text is written or relocated:
// image/defs.rs's lexical_relocating codec is gone. The c-image-block-lowering probe
// now checks warm as cold (~45 us both). REGISTRATION_FORMAT 76. Pin:
// lang::image::a_block_function_keeps_its_lowering_warm (cold and warm hold equal
// lowerings; the review's equivalent cargo test showed one extra warm; this pin was not
// run against the old build).
/// What a definition's compiles share: its lowerings (whose ids
/// its check table has rows for), the check's specs and its lambda
/// literals' shares.
fn shared_encode(shared: &BodyShared, buf: &mut impl BufMut) -> Result<(), PackError> {
    let BodyShared { lowerings, specs, literals } = shared;
    encode_varint(lowerings.len() as u64, buf);
    for (id, e) in lowerings.iter() {
        id.encode(buf)?;
        e.encode(buf)?;
    }
    encode_varint(specs.len() as u64, buf);
    for (id, e) in specs.iter() {
        id.encode(buf)?;
        e.encode(buf)?;
    }
    encode_varint(literals.len() as u64, buf);
    for (id, share) in literals.iter() {
        id.encode(buf)?;
        shared_encode(&share.lock(), buf)?;
    }
    Ok(())
}

fn shared_decode(buf: &mut impl Buf) -> Result<BodyShared, PackError> {
    let mut shared = BodyShared::default();
    for _ in 0..decode_varint(buf)? {
        let id = ExprId::decode(buf)?;
        shared.lowerings.insert(id, Expr::decode(buf)?);
    }
    for _ in 0..decode_varint(buf)? {
        let id = ExprId::decode(buf)?;
        shared.specs.insert(id, Pack::decode(buf)?);
    }
    for _ in 0..decode_varint(buf)? {
        let id = ExprId::decode(buf)?;
        let share = shared_decode(buf)?;
        shared.literals.insert(id, BodyShare::new(Mutex::new(share)));
    }
    Ok(shared)
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
        table,
        shared,
        facts,
        param_calls,
        recursion,
        no_loop: _,
        source,
        origin,
        level,
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
    facts.lock().encode(buf)?;
    param_calls.lock().encode(buf)?;
    recursion.lock().encode(buf)?;
    source.encode(buf)?;
    level.encode(buf)?;
    body_encode(body, buf)?;
    flags_encode(*flags, buf)?;
    spec.encode(buf)?;
    tables_encode(table.get(), buf)?;
    shared_encode(&shared.lock(), buf)
}

/// Rebuild a definition and register it in `ctx.lambda_defs`.
pub(crate) fn def_decode<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<'_, R, E>,
    buf: &mut impl Buf,
) -> Result<LambdaId, PackError> {
    let id = LambdaId::decode(buf)?;
    let env = lexical_decode(buf)?;
    let scope = scope_decode(buf)?;
    let argspec: Arc<[Arg]> = Pack::decode(buf)?;
    let typ: Arc<FnType> = Pack::decode(buf)?;
    let facts = LambdaFacts::decode(buf)?;
    let param_calls = super::super::node::lambda::ParamCalls::decode(buf)?;
    let recursion = Pack::decode(buf)?;
    let source = Pack::decode(buf)?;
    let level = Pack::decode(buf)?;
    let body = body_decode(buf)?;
    let flags = flags_decode(buf)?;
    let spec: Expr = Pack::decode(buf)?;
    let table = std::sync::Arc::new(tables_decode(buf)?);
    let shared = BodyShare::new(Mutex::new(shared_decode(buf)?));
    let init = make_init(
        id,
        flags,
        env.clone(),
        &scope,
        typ.clone(),
        argspec.clone(),
        Arc::new(spec.clone()),
        body.clone(),
        table.clone(),
        shared.clone(),
    );
    ctx.wrap_lambda(LambdaDef {
        id,
        env,
        scope,
        argspec,
        typ,
        init,
        table,
        shared,
        facts: Mutex::new(facts),
        param_calls: Mutex::new(param_calls),
        recursion: Mutex::new(recursion),
        no_loop: Mutex::new(None),
        source,
        origin: DefOrigin::Source { body, flags, spec },
        level,
    });
    Ok(id)
}
