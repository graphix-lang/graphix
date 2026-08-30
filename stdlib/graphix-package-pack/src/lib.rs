#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
use anyhow::{Result, bail};
use arcstr::ArcStr;
use bytes::Bytes;
use graphix_compiler::{
    ExecCtx, Node, Rt, Scope, UserEvent,
    effects::EffectKind,
    errf,
    typ::{FnType, Type},
};
use graphix_package_core::{
    CachedArgs, CachedArgsAsync, CachedVals, EvalCached, EvalCachedAsync,
    extract_cast_type,
};
use netidx_core::pack::Pack;
use netidx_value::{PBytes, Value};

// ── PackRead (async) ─────────────────────────────────────────

#[derive(Debug, Default)]
struct PackReadEv {
    cast_typ: Option<Type>,
}

impl EvalCachedAsync for PackReadEv {
    type Args = Bytes;

    const NAME: &str = "pack_read";

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

    fn typecheck0<R: Rt, E: UserEvent>(
        &mut self,
        _ctx: &mut ExecCtx<R, E>,
        _from: &mut [Node<R, E>],
    ) -> Result<()> {
        Ok(())
    }

    fn typecheck1<R: Rt, E: UserEvent>(
        &mut self,
        _ctx: &mut ExecCtx<R, E>,
        _from: &mut [Node<R, E>],
        resolved: &FnType,
    ) -> Result<()> {
        self.cast_typ = extract_cast_type(Some(resolved));
        if self.cast_typ.is_none() {
            bail!("pack::read requires a concrete return type")
        }
        Ok(())
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
        cached.get::<Bytes>(0)
    }

    fn eval(b: Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            match Value::decode(&mut b.as_ref()) {
                Ok(v) => v,
                Err(e) => errf!("PackErr", "{e}"),
            }
        }
    }
}

type PackRead = CachedArgsAsync<PackReadEv>;

// ── PackWriteBytes (sync) ────────────────────────────────────

#[derive(Debug, Default)]
struct PackWriteBytesEv;

fn fc_write_bytes(args: &[Value]) -> Option<Value> {
    let v = args.first()?;
    let len = v.encoded_len();
    let mut buf = Vec::with_capacity(len);
    Some(match v.encode(&mut buf) {
        Ok(()) => Value::Bytes(PBytes::new(Bytes::from(buf))),
        Err(e) => errf!("PackErr", "{e}"),
    })
}

// pack::write_bytes is a pure Value→bytes conversion. Sync.
impl<R: Rt, E: UserEvent> EvalCached<R, E> for PackWriteBytesEv {
    const EFFECT: EffectKind = EffectKind::Sync;
    const STATELESS: bool = true;
    const NAME: &str = "pack_write_bytes";
    const FASTCALL: Option<graphix_compiler::FastFn> = Some(fc_write_bytes);

    fn eval(&mut self, _ctx: &mut ExecCtx<R, E>, cached: &CachedVals) -> Option<Value> {
        graphix_package_core::fast_eval(fc_write_bytes, cached)
    }
}

type PackWriteBytes = CachedArgs<PackWriteBytesEv>;

// ── Package registration ─────────────────────────────────────

graphix_derive::defpackage! {
    builtins => [
        PackRead,
        PackWriteBytes,
    ],
}
