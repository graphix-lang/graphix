#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
use arcstr::{ArcStr, literal};
use bytes::Bytes;
use graphix_compiler::{env::Env, errf};
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
            // CR claude for claude: [bug/high] pack::read passes untrusted bytes to
            // netidx-value's Value::decode. That decoder reserves each array or map
            // header's claimed element count before any element arrives, and checks the
            // claim against the remaining input one container at a time, ignoring
            // frames already open. Nested headers therefore stack reservations. An 8.7
            // MB file of 70000 nested array headers, or a 3 MB file of shrinking ones,
            // aborts the whole runtime with "memory allocation of ... bytes failed"
            // (SIGABRT) instead of returning a PackErr. A 500 KB file passes a GB
            // resident before it fails. Fix it in Value::decode: reserve min(elts, a
            // small bound) and let the Vec grow, or bound the sum of the open frames'
            // outstanding lengths by buf.remaining(). The comment there that claims
            // memory is bounded by the per-container checks is wrong. This is the same
            // defect as small-pkgs-09. probe:
            // design/review-2026-10-12/repro/x-rerun-5-02.py (x-rerun-5-02)
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
    let v = args.first()?;
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
