#!/bin/bash
# A link batch larger than a whole fresh arena panics the runtime
# ("the JIT could not link emitted code: the JIT code arena is exhausted",
# graphix-compiler/src/fusion/emit/jit.rs install_or_panic) instead of
# running the program unfused. The program is jit_arena_rotation.rs's.
G=${GRAPHIX:-graphix}
dir=$(mktemp -d)
python3 - > "$dir/arena.gx" <<'PY'
R = 512
s = "let x = sys::time::after_idle(duration:1.ms, 3);\n"
for i in range(R):
    s += f"let r{i} = #[native] (x * {i} + 1);\n"
s += "let total = " + " + ".join(f"r{i}" for i in range(R)) + ";\n"
s += f"sys::exit(select total {{ {sum(3*i+1 for i in range(R))} => 0, _ => 1 }})\n"
print(s, end="")
PY
GRAPHIX_JIT_ARENA=65536 timeout -s KILL 60 $G --no-cache --no-netidx "$dir/arena.gx"
echo "exit $?"
rm -rf "$dir"
