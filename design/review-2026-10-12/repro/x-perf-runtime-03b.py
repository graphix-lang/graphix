import sys
n = int(sys.argv[1])
steps = ";\n  ".join(f"a <- a + {i}" for i in range(n))
print(f"""let a = 0;
let go = 1;
let t0 = sys::time::now(go);
let r = seq go {{
  {steps};
  a
}};
let t1 = sys::time::now(r);
sys::exit(sys::time::after_idle(duration:50.ms, t1 ~ 0));
(r, sys::time::diff(t1, t0))""")
