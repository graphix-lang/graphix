# sys::time - Timers

<!-- CR claude for claude: [doc-drift] This page is a hand copy of time.gxi and lacks 6 of
its 9 vals (add, sub, add_dur, sub_dur, diff, scale), and nothing else in the book
documents them. Meanwhile book/src/core/fundamental_types.md:139-166 teaches `duration +
duration`, `duration:1.0s * 50` and `datetime + duration`, all refused by the checker,
so the book shows no working time arithmetic. Include the gxi as io.md, tcp.md and
tls.md do. dirs.md, fs.md, net.md and process.md are hand copies too: net.md's subscribe
and call lack their Concrete bounds, and process.md drops most of the gxi's docs.
(sys-io-15) -->
```graphix
/// When v updates wait timeout and then return it. If v updates again
/// before timeout expires, reset the timeout and continue waiting.
val after_idle: fn(timeout: [duration, Number], v: 'a) -> 'a;

/// timer will wait timeout and then update with the current time.
/// If repeat is true, it will do this forever. If repeat is a number n,
/// it will do this n times and then stop. If repeat is false, it will do
/// this once.
val timer: fn(timeout: [duration, Number], repeat: [bool, Number]) -> Result<datetime, `TimerError(string)>;

/// return the current time each time trigger updates
val now: fn(trigger: Any) -> datetime;
```
