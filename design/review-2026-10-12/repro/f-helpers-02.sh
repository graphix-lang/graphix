#!/bin/bash
# f-helpers-02: jit_helpers! picks the pair-return ABI by matching the literal token `TagValue`
#
# Copies the HelperArg/HelperRet traits and the jit_helper_fn!/jit_helpers! macros
# verbatim out of graphix-compiler/src/fusion/emit_helpers.rs into a standalone
# crate (TagValue mocked as a repr(C) pair), declares one helper `-> TagValue` and
# one `-> crate::TagValue`, then calls each the way helper_signature (emit/lower.rs)
# has a kernel call a ret == [AbiTy::Pair] helper: args + trailing out-pointer.
#
# command: bash design/review-2026-10-12/repro/f-helpers-02.sh   (from the repo root)
# expected: both register ret = [Pair] AND both write the out slot (0x7, 0x....)
# observed (HEAD 7324207f), compiled with no warning:
#   bare: ret = [Pair]
#   pathed: ret = [Pair]
#   bare: out slot after call = [0x7, 0xaaaa]
#   pathed: out slot after call = [0xdead, 0xdead]   <- never written; a kernel reads this as a Value
set -e
F=graphix-compiler/src/fusion/emit_helpers.rs
D=$(mktemp -d)
trap 'rm -rf "$D"' EXIT
{
cat <<'RS'
#![allow(improper_ctypes_definitions, dead_code)]
use std::mem::ManuallyDrop;
mod tval { #[repr(C)] #[derive(Debug, Clone, Copy)] pub struct TagValue(pub u64, pub u64); }
pub use crate::tval::TagValue;
RS
sed -n '/^pub(crate) enum AbiTy/,$p' $F | sed -n '1,/^\/\/\/ Every registered helper/p' | sed '$d' \
  | sed 's/^pub(crate) enum AbiTy/#[derive(Debug, Clone, Copy, PartialEq, Eq)] pub(crate) enum AbiTy/'
cat <<'RS'
jit_helpers! {
    registry = demo_helpers;
    safe fn bare(x: u64) -> TagValue { TagValue(x, 0xAAAA) }
    safe fn pathed(x: u64) -> crate::TagValue { crate::TagValue(x, 0xBBBB) }
}
fn main() {
    let mut v = Vec::new();
    demo_helpers(&mut v);
    for s in &v { println!("{}: ret = {:?}", s.name, s.ret); }
    for s in &v {
        let mut slot: [u64; 2] = [0xDEAD, 0xDEAD];
        if s.ret == [AbiTy::Pair] {
            let f: extern "C" fn(u64, *mut [u64; 2]) = unsafe { std::mem::transmute(s.ptr) };
            f(7, &mut slot);
            println!("{}: out slot after call = {:#x?}", s.name, slot);
        }
    }
}
RS
} > "$D/demo.rs"
rustc --edition 2024 -O "$D/demo.rs" -o "$D/demo"
"$D/demo"
