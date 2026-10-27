fn one_constructor(source: &str, implementation: &str) {
    let count = source
        .split(implementation)
        .skip(1)
        .map(|part| part.split_once("\n}\n").expect("an impl ends").0)
        .map(|body| body.matches("Self {").count())
        .sum::<usize>();
    assert_eq!(
        count, 1,
        "{implementation}: compile and image_decode share one constructor"
    );
}

macro_rules! constructor {
    ($test:ident, $kind:literal, $file:literal) => {
        #[test]
        fn $test() {
            one_constructor(include_str!($file), concat!("impl<R: Rt> ", $kind, "<R> {"));
        }
    };
}

constructor!(map_ref, "MapRef", "../src/node/map.rs");
constructor!(structure, "Struct", "../src/node/data.rs");
constructor!(struct_with, "StructWith", "../src/node/data.rs");
constructor!(struct_ref, "StructRef", "../src/node/data.rs");
constructor!(tuple, "Tuple", "../src/node/data.rs");
constructor!(variant, "Variant", "../src/node/data.rs");
