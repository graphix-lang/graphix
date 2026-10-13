import sys
n=int(sys.argv[1]); kind=sys.argv[2]
if kind=="curry":
    s="let f = " + "".join(f"|a{i}| " for i in range(n)) + " + ".join(f"a{i}" for i in range(n)) + ";\n0\n"
elif kind=="ann":
    t="i64"
    for i in range(n):
        t=f"fn(x: {t}) -> i64"
    s=f"let f = |g: {t}| 0;\n0\n"
print(s,end="")
