mod dir_ops;
mod integration;
mod metadata;
mod read;
mod readdir;
mod tempdir;
mod watch;
mod write;

use graphix_compiler::expr::parser::GRAPHIX_ESC;
use poolshark::local::LPooled;
use std::{fmt::Write, path::Display};

fn escape_path(path: Display) -> LPooled<String> {
    let mut buf: LPooled<String> = LPooled::take();
    let mut res: LPooled<String> = LPooled::take();
    write!(buf, "{path}").unwrap();
    GRAPHIX_ESC.escape_to(&*buf, &mut res);
    res
}
