//! Review gui-core: an i64 Graphix field read through an unsigned Rust type
//! wraps instead of being refused.
//! Run: flock ~/tmp/cargo.lock cargo test -p graphix-package-gui --test review_gui_core_1 -- --nocapture
//! Expected: `Fixed(-1)` columns and `FillPortion(2^32 + 1)` are refused.
//! Observed: Fixed(18446744073709551615) and FillPortion(1).
use graphix_compiler::rep;
use graphix_package_gui::types::{GridColumnsV, LengthV};
use netidx::publisher::{FromValue, Value};

#[test]
fn signed_fields_are_not_wrapped() {
    rep::register();
    let cols = rep::make_variant(arcstr::literal!("Fixed"), [Value::I64(-1)]);
    let cols = GridColumnsV::from_value(cols);
    println!("Fixed(-1) columns read as {cols:?}");
    let portion =
        rep::make_variant(arcstr::literal!("FillPortion"), [Value::I64((1i64 << 32) + 1)]);
    let portion = LengthV::from_value(portion).map(|l| l.0);
    println!("FillPortion(4294967297) reads as {portion:?}");
    assert!(cols.is_err(), "a negative column count was accepted: {cols:?}");
    assert!(portion.is_err(), "an out of range weight was accepted: {portion:?}");
}
