import sys
n = int(sys.argv[1])
lets = "\n".join(f"let x{i} = {i};" for i in range(n))
steps = ";\n  ".join(f"let v{i} = x{i} + 1" for i in range(n))
print(f"""let go = 1;
{lets}
let t0 = sys::time::now(go);
let r = seq go {{
  {steps};
  v{n-1}
}};
let t1 = sys::time::now(r);
sys::exit(sys::time::after_idle(duration:50.ms, t1 ~ 0));
(r, sys::time::diff(t1, t0))""")
