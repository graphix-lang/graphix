//! A fire the walk meets after it passed a reader goes to that reader next
//! cycle only if the reader takes late fires.

use graphix_compiler::{
    BindId, Rt, TagValue,
    branch::RtView,
    expr::ExprId,
    prune::{BoundaryRef, Reader},
};
use graphix_rt::{GXRt, NoExt};
use netidx_value::Value;

#[test]
fn only_a_late_reader_is_passed_a_fire() {
    let mut rt = GXRt::<NoExt>::new();
    let (root, id, cycle) = (ExprId::new(), BindId::new(), rt.cycle());
    let behind = BoundaryRef::new(None);
    let walk = BoundaryRef::new(None);
    behind.visit(cycle);
    walk.visit(cycle);
    let late = Reader { root, boundary: Some(behind.clone()), late: true };
    let quiet = Reader { root, boundary: Some(behind), late: false };
    rt.ref_var(id, late.clone());
    rt.ref_var(id, quiet);
    let mut passed = None;
    let elsewhere = RtView::Root(&mut rt).store_fire(
        id,
        TagValue::fired(Value::Null),
        root,
        Some(&walk),
        &mut passed,
    );
    assert!(!elsewhere);
    assert_eq!(passed.expect("a passed reader").as_slice(), [late]);
}
