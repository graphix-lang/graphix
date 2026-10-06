## Notes on graphix

- graphix is a dataflow language, when you write code you're actually
  specifying an event graph. This causes semantics to be very
  different than most programming languages in some cases.
  
- The examples directory contains graphix examples

- graphix struct fields are stored as arrays of pairs sorted by the
  field name, keep that in mind when using them in rust bindings.
  
<!-- CR claude for eric: [doc-drift] This hand-written file has not changed since the
initial commit. Agents that honor nested AGENTS.md files still read it, and this rule is
false: a trailing `;` parses in a block and at the top level, because the parser fills
the empty last position with a NoOp (graphix-types/src/expr/parser/mod.rs:806-807,
1066-1067). The real hazard is the opposite one: `let x = { let a = 1; a + 1; };
println("[x]")` checks and never prints, because the block's value is the NoOp.
CLAUDE.md says AGENTS.md is generated, and the struct-field note is in
book/src/udt/structs.md, so delete this file. (x-doc-drift-13) -->
- graphix modules and blocks are ; separated, the last item may not
  end in a semi. Comma separated items work in a similar way.
