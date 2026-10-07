#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
use arcstr::{ArcStr, literal};
use bytes::Bytes;
use graphix_compiler::{
    ExecCtx, FastCall, Rt, UserEvent, effects::Effect, env::Env, errf,
};
use graphix_package_core::{
    CachedArgs, CachedArgsAsync, CachedVals, CastTarget, EvalCached, ReadFormat,
    TypedRead,
};
use netidx_core::pack::Pack;
use netidx_value::{PBytes, ValArray, Value};

#[derive(Debug)]
struct Packed;

impl ReadFormat for Packed {
    const NAME: &str = "pack_read";
    const TAG: ArcStr = literal!("PackErr");
    type Args = Bytes;

    fn prepare_args(cached: &CachedVals) -> Option<Bytes> {
        cached.get::<Bytes>(0)
    }

    /// A decoded value comes back as the one element of an array, so an
    /// error value decoded from the bytes is data, never the reader's own.
    fn parse(b: Bytes) -> impl Future<Output = Value> + Send {
        async move {
            match Value::decode(&mut b.as_ref()) {
                Ok(v) => Value::Array(ValArray::from_iter([v])),
                Err(e) => errf!(Self::TAG, "{e}"),
            }
        }
    }

    fn read(target: &CastTarget, env: &Env, v: Value) -> Value {
        match v {
            Value::Array(a) if a.len() == 1 => {
                target.read_data(env, &Self::TAG, a[0].clone())
            }
            v => target.read(env, &Self::TAG, v),
        }
    }
}

type PackRead = CachedArgsAsync<TypedRead<Packed>>;

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

impl<R: Rt, E: UserEvent> EvalCached<R, E> for PackWriteBytesEv {
    const EFFECT: Effect = Effect::Stateless(Some(FastCall::Plain(fc_write_bytes)));
    const NAME: &str = "pack_write_bytes";

    fn eval(
        &mut self,
        ctx: &mut ExecCtx<'_, R, E>,
        cached: &CachedVals,
    ) -> Option<Value> {
        graphix_package_core::fast_eval(ctx, fc_write_bytes, cached)
    }
}

type PackWriteBytes = CachedArgs<PackWriteBytesEv>;

graphix_package_core::unit_image_state!(PackWriteBytesEv);

graphix_derive::defpackage! {
    builtins => [
        PackRead,
        PackWriteBytes,
    ],
}
