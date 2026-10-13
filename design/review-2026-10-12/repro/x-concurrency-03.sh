#!/bin/sh
# range's queued writes outlive the arm that slept: ~4s of CPU over 3s
# vs ~0.1s for the control, the runtime cycling through ~2M writes to a
# variable nothing references.
d=$(dirname "$0")
time graphix --no-cache "$d/x-concurrency-03.gx" > /dev/null
time graphix --no-cache "$d/x-concurrency-03-control.gx" > /dev/null
