import sys
n = int(sys.argv[1]); distinct = sys.argv[2] == "d"
lets = "\n".join(f"let x{i} = {i};" for i in range(n))
arms = ",\n  ".join(f"{i} => count(x{i if distinct else 0}) + n" for i in range(n))
print(f"""let k = 2010;
let n = 0;
n <- n + 1;
{lets}
let s = 0;
let r = select s {{
  {arms},
  _ => 0
}};
let t0 = sys::time::now(select n {{ 10 => n, _ => never() }});
let t1 = sys::time::now(select n {{ c if c == k => n, _ => never() }});
sys::exit(sys::time::after_idle(duration:50.ms, t1 ~ 0));
(r, sys::time::diff(t1, t0))""")
