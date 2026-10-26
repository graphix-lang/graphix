//! Run: copy to stdlib/graphix-package-gui/tests/review_x_stack_1.rs, then
//!   flock ~/tmp/cargo.lock timeout -s KILL 2400 cargo test -p graphix-package-gui --test review_x_stack_1 -- --nocapture
//! Expected: every depth compiles (exit 0). Observed at e6ffc6eed (dev): depth 10 and 100
//! pass; 300, 1000 and 3000 abort with "thread has overflowed its stack" (status 134).
//! x-stack-gui: a nested GUI widget tree overflows the stack while
//! `widgets::compile` builds it. The event loop compiles a window's
//! content on the main thread through `Handle::block_on`
//! (`event_loop.rs`, `open`), so each depth here is compiled the same
//! way: on a fresh 8 MiB thread, in a child process (an overflow aborts).

use anyhow::Result;
use graphix_package_core::testing::{Fixture, PackageRef};
use std::{env, process::Command};

const REGISTER: &[PackageRef] = &[
    &graphix_package_core::P,
    &graphix_package_array::P,
    &graphix_package_map::P,
    &graphix_package_str::P,
    &graphix_package_sys::P,
    &graphix_package_gui::P,
];

fn child(depth: usize) -> Result<()> {
    let rt = tokio::runtime::Builder::new_multi_thread().enable_all().build()?;
    let code = format!(
        "use gui::{{container::container, text::text}};\n\
         let result = array::fold(array::init({depth}, |i| i), text(&\"x\"), \
         |acc, i| container(&acc))"
    );
    let (f, v) = rt.block_on(Fixture::new(&code, REGISTER, |_| ()))?;
    let h = rt.handle().clone();
    let gx = f.gx.clone();
    let r = std::thread::Builder::new()
        .stack_size(8 << 20)
        .spawn(move || {
            h.block_on(graphix_package_gui::widgets::compile(gx, v)).map(|_| ())
        })?
        .join()
        .expect("join");
    r
}

#[test]
fn deep_widget_tree_compiles() {
    if let Ok(d) = env::var("X_STACK_DEPTH") {
        child(d.parse().unwrap()).expect("compile");
        return;
    }
    let exe = env::current_exe().unwrap();
    let mut bad = vec![];
    for depth in [10, 100, 300, 1000, 3000] {
        let st = Command::new(&exe)
            .args(["deep_widget_tree_compiles", "--exact", "--nocapture"])
            .env("X_STACK_DEPTH", depth.to_string())
            .status()
            .unwrap();
        println!("depth {depth}: {st:?}");
        if !st.success() {
            bad.push((depth, st));
        }
    }
    assert!(bad.is_empty(), "{bad:?}");
}
