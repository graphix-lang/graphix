# x-stack: nested `try` in a seq makes the check cubic in the nesting depth. Each try
# body is lowered in a lexical scope of its own (Lower::lower_scoped), so a step at
# try depth d is compiled d scopes deep, and each of its generated references (the
# catch's `pc` connect, the `#seqe` cells) pays Env::chain_lookup's O(d^2) (the
# mechanism of open CR t-misc-02). Sampling under gdb: Env::lookup_at <-
# chain_lookup <- resolve_visible <- Catch::compile / Connect::compile <- SeqMachine::compile.
# run: python3 x-stack-seq-try-nesting.py D > /tmp/tw.gx; timeout -s KILL 170 graphix --check /tmp/tw.gx
# expected: a check proportional to the file (D = 990 is a 25 KB file the parser admits).
# observed (debug build): --check 0.85 s at D = 250, 6.2 s at 500, 45 s at 990 (--expand
#   and a run the same).
import sys
d = int(sys.argv[1]) if len(sys.argv) > 1 else 990
print("seq { " + "try { 1; " * d + "1; 1" + " } with(e) { 0 }" * d + " }")
