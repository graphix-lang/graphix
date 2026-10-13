import socket, time, urllib.request, sys
time.sleep(3)
idle = []
for i in range(2):
    s = socket.create_connection(("127.0.0.1", 18431)); idle.append(s)
time.sleep(0.5)
t = time.time()
try:
    r = urllib.request.urlopen("http://127.0.0.1:18431/x", timeout=8).read()
    print("third request answered", r, round(time.time()-t, 2))
except Exception as e:
    print("third request failed after", round(time.time()-t, 2), "s:", e)
for s in idle: s.close()
time.sleep(0.5)
t = time.time()
try:
    r = urllib.request.urlopen("http://127.0.0.1:18431/y", timeout=8).read()
    print("after closing idle:", r, round(time.time()-t, 2))
except Exception as e:
    print("after closing idle failed:", e)
