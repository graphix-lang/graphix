# sep28f (848d67f9, base 12100M; katana 3, aieka 5, ryouko 13)

All 21 are one class: `jit/warm: CompileErr("no program")`, in the
generate lane. Every one reproduces locally.

**Cause: a let's local had its initializer's representation, not its
binding's.** `let v: [f64, null] = 1.0` bound a scalar `f64` local
while every consumer classifies on the binding's type (nullable, a
two-word value): a read handed the float register to
`graphix_value_clone`, and a capture passed a `bool` register as a
nullable kernel parameter. Cranelift's verifier refuses both. Before
848d67f9 that only de-fused the region, silently; with the pass-end
link the whole statement was rebuilt without fusion, and that rebuild
left a program image that did not read back, which is the divergence.

**Fixed:** `emit_let_node` (`fusion/emit/flow.rs`) binds by the
binding's type and widens a scalar initializer to its value word;
`emit_ref_node` (`fusion/emit/nodes.rs`) emits the representation the
read's own type declares. A link that fails now panics (Eric 09-28): the
rebuild path, and its image bug, are gone. Pins:
`lang::fusion::scalar_read_at_a_nullable`,
`scalar_let_captured_at_a_nullable`,
`findings/scalar-read-widen-sep2026/`. The fix also fuses
`xkernel-gate-jul2026/01` as one kernel where four regions fused
(`let v0: [f64, null] = null` no longer refuses its block).
