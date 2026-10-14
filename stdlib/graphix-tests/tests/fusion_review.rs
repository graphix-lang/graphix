const TEST_REGISTER: &[&dyn graphix_package::Package<graphix_rt::NoExt>] =
    graphix_package::package_refs!();

#[path = "../src/lang/fusion_review.rs"]
mod fusion_review;
