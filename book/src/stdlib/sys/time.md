# sys::time - Timers

```graphix
/// When v updates wait timeout and then return it. If v updates again
/// before timeout expires, reset the timeout and continue waiting.
val after_idle: fn(v: [duration, Number], y: 'a) -> 'a;

/// timer will wait timeout and then update with the current time.
/// If repeat is true, it will do this forever. If repeat is a number n,
/// it will do this n times and then stop. If repeat is false, it will do
/// this once.
val timer: fn(v: [duration, Number], v2: [bool, Number]) -> Result<datetime, `TimerError(string)>;

/// return the current time each time trigger updates
val now: fn(v: Any) -> datetime;
```
