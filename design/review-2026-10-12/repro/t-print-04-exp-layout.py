import sys
d = int(sys.argv[1])
trig = "a" * 95
s = "1"
for i in range(d):
    s = f"seq {trig} {{ let x{i} = {s}; x{i} }}"
print("let r = " + s + ";\nr")
