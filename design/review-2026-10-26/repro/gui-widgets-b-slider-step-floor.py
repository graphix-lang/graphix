# Title: a stepped slider drops its last step when (max - min) / step rounds below an integer
# Command: timeout -s KILL 10 python3 -I design/review-2026-10-26/repro/gui-widgets-b-slider-step-floor.py
# Replays slider.rs Scale::new / Scale::value / Scale::position in IEEE f64 (Python floats are f64).
# Expected: slider(#min: &0.0, #max: &0.3, #step: &0.1, ..) reaches 0.3 at its right end,
#           and a program value of 0.3 sits at the right end.
# Observed: count = 2, so the right end sends 0.2; 0.3 and 0.2 both draw at the right end.
import math
def scale(mn, mx, step):
    count = math.floor((mx - mn) / step)
    value = lambda pos: min(mn + round(pos) * step, mx)
    position = lambda v: min(round((min(max(v, mn), mx) - mn) / step), count)
    return count, value, position
for mn, mx, step in [(0.0, 0.3, 0.1), (0.0, 0.7, 0.1), (1.0, 2.2, 0.1), (0.0, 1.0, 0.1)]:
    count, value, position = scale(mn, mx, step)
    print(f"min={mn} max={mx} step={step}: count={count} right-end value={value(count)} "
          f"position(max)={position(mx)} reaches max: {value(count) == mx}")
