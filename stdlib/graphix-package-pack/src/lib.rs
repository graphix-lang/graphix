#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
use arcstr::{ArcStr, literal};
use bytes::Bytes;
use graphix_compiler::{abstract_value::to_data, env::Env, errf};
use graphix_package_core::{
    CachedArgsAsync, CachedVals, CastTarget, ReadFormat, TypedRead,
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
            // CR claude for claude: [bug/low] The decode never checks that it used the
            // whole buffer, so a value followed by more bytes (two messages from
            // Read::read_all, or garbage) reads as the first value: [6, 0x00 x7, 42,
            // 99, 99, 99] read as i64 is 42. json::read and toml::read refuse trailing
            // input. Keep the slice, and answer PackErr when anything remains after the
            // value. Pin it. probe:
            // design/review-2026-10-19/repro/small-pkgs-pack-trailing-bytes.gx
            // (small-pkgs-05)
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

fn fc_write_bytes(args: &[Value]) -> Option<Value> {
    let v = match to_data(args.first()?) {
        Ok(v) => v,
        Err(e) => return Some(errf!("PackErr", "{e}")),
    };
    let len = v.encoded_len();
    let mut buf = Vec::with_capacity(len);
    Some(match v.encode(&mut buf) {
        Ok(()) => Value::Bytes(PBytes::new(Bytes::from(buf))),
        Err(e) => errf!("PackErr", "{e}"),
    })
}

graphix_package_core::fast_builtin!(
    PackWriteBytes,
    PackWriteBytesEv,
    "pack_write_bytes",
    fc_write_bytes
);

graphix_derive::defpackage! {
    builtins => [
        PackRead,
        PackWriteBytes,
    ],
}
