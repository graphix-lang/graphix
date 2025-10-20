# Core

```
type Sint = [ i32, z32, i64, z64 ];
type Uint = [ u32, v32, u64, v64 ];
type Int = [ Sint, Uint ];
type Float = [ f32, f64 ];
type Real = [ Float, decimal ];
type Number = [ Int, Real ];
type NotNull = [Number, string, error, array, datetime, duration];
type Primitive = [NotNull, null];
type PrimNoErr = [Number, string, array, datetime, duration, null];
type Log = [`Trace, `Debug, `Info, `Warn, `Error, `Stdout, `Stderr];
type Result<'r, 'e> = ['r, Error<'e>];
type Option<'a> = ['a, null];

type Pos = {
    line: i32,
    column: i32
};

type Source = [
    `File(string),
    `Netidx(string),
    `Internal(string),
    `Unspecified
];

type Ori = {
    parent: [Ori, null],
    source: Source,
    text: string
};

type ErrChain<'a> = {
    cause: [ErrChain<'a>, null],
    error: 'a,
    ori: Ori,
    pos: Pos
};

/// return the first argument when all arguments are equal, otherwise return nothing
val all: fn(@args: Any) -> Any;

/// return true if all arguments are true, otherwise return false
val and: fn(@args: bool) -> bool;

/// return the number of times x has updated
val count: fn(Any) -> i64;

/// return the first argument divided by all subsuquent arguments
val divide: fn('a, @args:'a) -> 'a;

/// return e only if e is an error
val filter_err: fn(Result<'a, 'b>) -> Error<'b>;

/// return v if f(v) is true, otherwise return nothing
val filter: fn('a, fn('a) -> bool throws 'e) -> 'a throws 'e;

/// return true if e is an error
val is_err: fn(Any) -> bool;

/// construct an error from the specified string
val error: fn('a) -> Error<'a>;

/// return the maximum value of any argument
val max: fn('a, @args: 'a) -> 'a;

/// return the mean of the passed in arguments
val mean: fn([Number, Array<Number>], @args: [Number, Array<Number>]) -> Result<f64, `MeanError(string)>;

/// return the minimum value of any argument
val min: fn('a, @args:'a) -> 'a;

/// return v only once, subsuquent updates to v will be ignored
/// and once will return nothing
val once: fn('a) -> 'a;

/// seq will update j - i times, starting at i and ending at j - 1
val seq: fn(i64, i64) -> Result<i64, `SeqError(string)>;

/// return true if any argument is true
val or: fn(@args: bool) -> bool;

/// return the product of all arguments
val product: fn(@args: [Number, Array<[Number, Array<Number>]>]) -> Number;

/// return the sum of all arguments
val sum: fn(@args: [Number, Array<[Number, Array<Number>]>]) -> Number;

/// when v updates return v if the new value is different from the previous value,
/// otherwise return nothing.
val uniq: fn('a) -> 'a;

/// when v updates place it's value in an internal fifo queue. when clock updates
/// return the oldest value from the fifo queue. If clock updates and the queue is
/// empty, record the number of clock updates, and produce that number of
/// values from the queue when they are available.
val queue: fn(#clock:Any, 'a) -> 'a;

/// hold the most recent value of v interally until clock updates. If v updates
/// more than once before clock updates, older values of v will be discarded,
/// only the most recent value will be retained. If clock updates when no v is held
/// internall, record the number of times it updated, and pass that many v updates
/// through immediatly when they happen.
val hold: fn(#clock:Any, 'a) -> 'a;

/// ignore updates to any argument and never return anything
val never: fn(@args: Any) -> _;

/// when v updates, return it, but also print it along
/// with the position of the expression to the specified sink
val dbg: fn(?#dest:[`Stdout, `Stderr, Log], 'a) -> 'a;

/// print a log message to stdout, stderr or the specified log level using the rust log
/// crate. Unlike dbg, log does not also return the value.
val log: fn(?#dest:Log, 'a) -> _;

/// print a raw value to stdout, stderr or the specified log level using the rust log
/// crate. Unlike dbg, log does not also return the value. Does not automatically insert
/// a newline and does not add the source module/location.
val print: fn(?#dest:Log, 'a) -> _;

/// print a raw value to stdout, stderr or the specified log level using the rust log
/// crate followed by a newline. Unlike dbg, log does not also return the value.
val println: fn(?#dest:Log, 'a) -> _;

/// Throttle v so it updates at most every #rate, where rate is a
/// duration (default 0.5 seconds). Intermediate updates that push v
/// over the #rate will be discarded. The most recent update will always
/// be delivered. If the sequence, m0, m1, ..., mN, arrives simultaneously
/// after a period of silence, first m0 will be delivered, then after the rate
/// timer expires mN will be delivered, m1, ..., m(N-1) will be discarded.
val throttle: fn(?#rate:duration, 'a) -> 'a;
```
