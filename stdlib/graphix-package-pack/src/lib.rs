#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
use anyhow::{bail, Result};
use arcstr::ArcStr;
use bytes::Bytes;
use graphix_compiler::{
    errf, typ::FnType, typ::Type, ExecCtx, Node, Rt, Scope, TypecheckPhase, UserEvent,
};
use graphix_package_core::{
    extract_cast_type, CachedArgs, CachedArgsAsync, CachedVals, EvalCached,
    EvalCachedAsync,
};
use graphix_package_sys::{get_stream, StreamKind};
use netidx_core::pack::Pack;
use netidx_value::{PBytes, Value};
use poolshark::local::LPooled;
use std::sync::Arc;
use tokio::{io::AsyncReadExt, io::AsyncWriteExt, sync::Mutex};

// ── ReadInput ────────────────────────────────────────────────

#[derive(Debug)]
enum ReadInput {
    Bytes(Bytes),
    Stream(Arc<Mutex<Option<StreamKind>>>),
}

// ── PackRead (async) ─────────────────────────────────────────

#[derive(Debug, Default)]
struct PackReadEv {
    cast_typ: Option<Type>,
}

impl EvalCachedAsync for PackReadEv {
    const NAME: &str = "pack_read";
    const NEEDS_CALLSITE: bool = true;
    type Args = ReadInput;

    fn init<R: Rt, E: UserEvent>(
        _ctx: &mut ExecCtx<R, E>,
        _typ: &FnType,
        resolved: Option<&FnType>,
        _scope: &Scope,
        _from: &[Node<R, E>],
        _top_id: graphix_compiler::expr::ExprId,
    ) -> Self {
        Self { cast_typ: extract_cast_type(resolved) }
    }

    fn typecheck<R: Rt, E: UserEvent>(
        &mut self,
        _ctx: &mut ExecCtx<R, E>,
        _from: &mut [Node<R, E>],
        phase: TypecheckPhase<'_>,
    ) -> Result<()> {
        match phase {
            TypecheckPhase::Lambda => Ok(()),
            TypecheckPhase::CallSite(resolved) => {
                self.cast_typ = extract_cast_type(Some(resolved));
                if self.cast_typ.is_none() {
                    bail!("pack::read requires a concrete return type")
                }
                Ok(())
            }
        }
    }

    fn map_value<R: Rt, E: UserEvent>(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        v: Value,
    ) -> Option<Value> {
        match &self.cast_typ {
            Some(typ) => Some(typ.cast_value(&ctx.env, v)),
            None => Some(errf!("PackErr", "no concrete return type found")),
        }
    }

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        let v = cached.0.first()?.as_ref()?;
        match v {
            Value::Bytes(b) => Some(ReadInput::Bytes((**b).clone())),
            Value::Abstract(_) => Some(ReadInput::Stream(get_stream(cached, 0)?)),
            _ => None,
        }
    }

    fn eval(input: Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            match input {
                ReadInput::Bytes(b) => match Value::decode(&mut b.as_ref()) {
                    Ok(v) => v,
                    Err(e) => errf!("PackErr", "{e}"),
                },
                ReadInput::Stream(stream) => {
                    let mut guard = stream.lock().await;
                    let s = match guard.as_mut() {
                        Some(s) => s,
                        None => return errf!("IOErr", "stream unavailable"),
                    };
                    let mut buf: LPooled<Vec<u8>> = LPooled::take();
                    if let Err(e) = s.read_to_end(&mut buf).await {
                        return errf!("IOErr", "read failed: {e}");
                    }
                    match Value::decode(&mut buf.as_slice()) {
                        Ok(v) => v,
                        Err(e) => errf!("PackErr", "{e}"),
                    }
                }
            }
        }
    }
}

type PackRead = CachedArgsAsync<PackReadEv>;

// ── PackWriteBytes (sync) ────────────────────────────────────

#[derive(Debug, Default)]
struct PackWriteBytesEv;

impl<R: Rt, E: UserEvent> EvalCached<R, E> for PackWriteBytesEv {
    const NAME: &str = "pack_write_bytes";
    const NEEDS_CALLSITE: bool = false;

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, cached: &CachedVals) -> Option<Value> {
        let v = cached.0.first()?.as_ref()?;
        let len = v.encoded_len();
        let mut buf = Vec::with_capacity(len);
        Some(match v.encode(&mut buf) {
            Ok(()) => Value::Bytes(PBytes::new(Bytes::from(buf))),
            Err(e) => errf!("PackErr", "{e}"),
        })
    }
}

type PackWriteBytes = CachedArgs<PackWriteBytesEv>;

// ── PackWriteStream (async) ──────────────────────────────────

#[derive(Debug, Default)]
struct PackWriteStreamEv;

impl EvalCachedAsync for PackWriteStreamEv {
    const NAME: &str = "pack_write_stream";
    const NEEDS_CALLSITE: bool = false;
    type Args = (Arc<Mutex<Option<StreamKind>>>, Vec<u8>);

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        let stream = get_stream(cached, 0)?;
        let v = cached.0.get(1)?.as_ref()?;
        let len = v.encoded_len();
        let mut buf = Vec::with_capacity(len);
        v.encode(&mut buf).ok()?;
        Some((stream, buf))
    }

    fn eval((stream, buf): Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            let mut guard = stream.lock().await;
            let s = match guard.as_mut() {
                Some(s) => s,
                None => return errf!("IOErr", "stream unavailable"),
            };
            match s.write_all(&buf).await {
                Ok(()) => Value::Null,
                Err(e) => errf!("IOErr", "write failed: {e}"),
            }
        }
    }
}

type PackWriteStream = CachedArgsAsync<PackWriteStreamEv>;

// ── Package registration ─────────────────────────────────────

graphix_derive::defpackage! {
    builtins => [
        PackRead,
        PackWriteBytes,
        PackWriteStream,
    ],
}
