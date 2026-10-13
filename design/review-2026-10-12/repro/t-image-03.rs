// t-image-03: a failed decode leaves half-built objects in the decoder's
// store; a later read on the same decoder takes them as built.
//
// Run: copy to graphix-types/tests/review_t_image_03.rs, then
//   cargo test -p graphix-types --test review_t_image_03 -- --nocapture
// Expected: every successful read of tvar `a` carries its one constraint.
// Observed: the first read (union first) fails with InvalidFormat (the
// third-entry refusal, see t-image-third-entry.gx); the second read of the
// same bytes on the same decoder succeeds, and `a` and `b` come back with
// 0 constraints (the placeholder cells tvar_decode entered); the assertion
// fails.
//
// Program-level: the same happens to a restored definition's tables. With
// XDG_CACHE_HOME pointed at a scratch dir, run this twice (cold, then warm):
//   let g = |y: i64| -> i64 {
//     let f = 'a: [i64, ('a, 'b)], 'b: [i64, ('a, 'b)] |x: [i64, ('a, 'b)]| -> i64 1;
//     f(y) + y
//   };
//   let t = sys::time::timer(duration:30.ms, false);
//   let h = |z: i64| g(z * 2);
//   let r1 = g(3);
//   let r2 = g(10);
//   sys::exit(sys::time::after_idle(duration:200.ms, 0));
//   h(t ~ 5)
// The warm run logs "a restored definition's tables did not decode:
// InvalidFormat" for g(3) and g(10), then g(z * 2)'s fallback reads the
// same table with no error and binds from it.
use arcstr::literal;
use graphix_types::{
    image::{DecodeImage, EncodeImage, ImageBuf, ImageDecoder, ImageEncoder},
    typ::{TVar, Type},
};
use netidx_core::pack::Pack;
use netidx_value::Typ;
use triomphe::Arc;

#[test]
fn review_t_image_03() {
    let a = TVar::empty_named(literal!("a"));
    let b = TVar::empty_named(literal!("b"));
    let tup = Type::Tuple(Arc::from_iter([Type::TVar(a.clone()), Type::TVar(b.clone())]));
    let t = Type::Set(Arc::from_iter([Type::Primitive(Typ::I64.into()), tup]));
    a.add_cell_constraint(t.clone());
    b.add_cell_constraint(t.clone());
    eprintln!("original a constraints: {:?}", a.cell_constraints().len());
    eprintln!("original b constraints: {:?}", b.cell_constraints().len());
    let items = [t.clone(), Type::TVar(a.clone())];
    let mut enc = ImageEncoder::new();
    let lens: Vec<usize> =
        EncodeImage::with(&mut enc, || items.iter().map(|i| i.encoded_len()).collect());
    let mut buf = EncodeImage::with(&mut enc, || {
        let mut buf = ImageBuf::with_capacity(0);
        for (i, l) in items.iter().zip(&lens) {
            let before = buf.len();
            i.encode(&mut buf).unwrap();
            assert_eq!(buf.len() - before, *l);
        }
        buf
    });
    let body = buf.len();
    let offsets = enc.finish(&mut buf);
    let image = buf.freeze();
    let t_at = 0..lens[0];
    let a_at = lens[0]..body;
    let read = |dec: &mut ImageDecoder, r: std::ops::Range<usize>| {
        DecodeImage::with(dec, || Type::decode(&mut &image[r]))
    };
    let show = |what: &str, r: &Result<Type, _>| match r {
        Err(e) => eprintln!("{what}: Err({e:?})"),
        Ok(t) => {
            let mut tvs = vec![];
            fn walk(t: &Type, out: &mut Vec<TVar>) {
                match t {
                    Type::TVar(tv) => out.push(tv.clone()),
                    Type::Set(ts) | Type::Tuple(ts) => ts.iter().for_each(|t| walk(t, out)),
                    _ => (),
                }
            }
            walk(t, &mut tvs);
            for tv in tvs {
                eprintln!(
                    "{what}: Ok, tvar {} has {} constraints",
                    tv.name,
                    tv.cell_constraints().len()
                );
            }
        }
    };
    // a fresh decoder that reaches the cycle through a tvar first
    let mut fresh = ImageDecoder::new(enc.counts(), image.clone(), offsets.clone()).unwrap();
    show("fresh decoder, tvar a first", &read(&mut fresh, a_at.clone()));
    // a decoder that reaches it through the union first, then again
    let mut dec = ImageDecoder::new(enc.counts(), image.clone(), offsets.clone()).unwrap();
    let first = read(&mut dec, t_at.clone());
    show("same decoder, union first, 1st read", &first);
    let second = read(&mut dec, t_at.clone());
    show("same decoder, union first, 2nd read", &second);
    let third = read(&mut dec, a_at.clone());
    show("same decoder, then tvar a", &third);
    if first.is_err() {
        if let Ok(Type::TVar(tv)) = &third {
            assert_eq!(
                tv.cell_constraints().len(),
                1,
                "a read after a failed read decoded tvar a without its constraint"
            );
        }
    }
}
