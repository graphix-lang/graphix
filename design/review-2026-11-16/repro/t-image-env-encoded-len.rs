// Env::encoded_len is not what Env::encode writes once the image's
// ordinals cross a varint width: `package_roots` is packed as a
// SharedSet over `TrackedSet::to_set()`, a set rebuilt at each call, so
// the measure and the write each meet a fresh root node and take two
// ordinals. With 127 objects seen first, the measure's root is ordinal
// 127 (one varint byte) and the write's is 128 (two).
//
// Run: copy to graphix-types/tests/review_t_image_1.rs, then
//   cargo test -p graphix-types --test review_t_image_1 -- --nocapture
// Expected: the write is as long as the measure.
// Observed: "measured 17 written 18", the assertion fails.

use arcstr::ArcStr;
use graphix_types::{
    env::Env,
    expr::parser::parse_type,
    image::{EncodeImage, ImageBuf, ImageEncoder},
};
use netidx_core::pack::Pack;

#[test]
fn env_measures_what_it_writes() {
    let mut env = Env::default();
    env.package_roots.insert(ArcStr::from("core"));
    let types: Vec<_> =
        (0..127).map(|i| parse_type(&format!("`T{i}")).unwrap()).collect();
    let mut enc = ImageEncoder::new();
    let (measured, written) = EncodeImage::with(&mut enc, || {
        for t in &types {
            t.encoded_len();
        }
        let measured = env.encoded_len();
        let mut buf = ImageBuf::with_capacity(0);
        env.encode(&mut buf).unwrap();
        (measured, buf.len())
    });
    eprintln!("measured {measured} written {written}");
    assert_eq!(written, measured);
}
