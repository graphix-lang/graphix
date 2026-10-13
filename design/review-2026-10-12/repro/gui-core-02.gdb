set pagination off
set confirm off
set breakpoint pending on
handle SIGPIPE nostop noprint pass
handle SIG32 nostop noprint pass
handle SIG33 nostop noprint pass
python
import time, os
out = open(os.environ['GXFRAMES'], 'a')
class BP(gdb.Breakpoint):
    def stop(self):
        out.write('%f\n' % time.time()); out.flush()
        return False
BP('graphix_package_gui::frame::frame')
end
run
