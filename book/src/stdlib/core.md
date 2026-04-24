# Core

```graphix
type Sint = [ i8, i16, i32, z32, i64, z64 ];
type Uint = [ u8, u16, u32, v32, u64, v64 ];
type Int = [ Sint, Uint ];
type Float = [ f32, f64 ];
type Real = [ Float, decimal ];
type Number = [ Int, Real ];
type NotNull = [Number, string, error, array, datetime, duration, bytes, bool];
type Primitive = [NotNull, null];
type PrimNoErr = [Number, string, array, datetime, duration, bytes, null];
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

/// return the first argument divided by all subsequent arguments
val divide: fn(@args: [Number, Array<[Number, Array<Number>]>]) -> Number;

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

/// return v only once, subsequent updates to v will be ignored
/// and once will return nothing
val once: fn('a) -> 'a;

/// take n updates from e and drop the rest. The internal count is reset when n updates.
val take: fn(#n:Any, 'a) -> 'a;

/// skip n updates from e and return the rest. The internal count is reset when n updates.
val skip: fn(#n:Any, 'a) -> 'a;

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

/// hold the most recent value of v internally until clock updates. If v updates
/// more than once before clock updates, older values of v will be discarded,
/// only the most recent value will be retained. If clock updates when no v is held
/// internally, record the number of times it updated, and pass that many v updates
/// through immediately when they happen.
val hold: fn(#clock:Any, 'a) -> 'a;

/// ignore updates to any argument and never return anything
val never: fn(@args: Any) -> 'a;

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

/// bitwise AND
val bit_and: fn<'a: Int>('a, 'a) -> 'a;

/// bitwise OR
val bit_or: fn<'a: Int>('a, 'a) -> 'a;

/// bitwise XOR
val bit_xor: fn<'a: Int>('a, 'a) -> 'a;

/// bitwise complement
val bit_not: fn<'a: Int>('a) -> 'a;

/// shift left (wrapping)
val shl: fn<'a: Int>('a, 'a) -> 'a;

/// shift right (wrapping)
val shr: fn<'a: Int>('a, 'a) -> 'a;
```

## core::buffer

The `buffer` submodule provides functions for working with raw bytes:
conversion between bytes and strings/arrays, concatenation, and a
flexible binary encode/decode system with control over endianness and
variable-length encoding.

```graphix
/// Convert bytes to a UTF-8 string.
val to_string: fn(bytes) -> Result<string, `EncodingError(string)>;

/// Convert bytes to a UTF-8 string, replacing invalid sequences.
val to_string_lossy: fn(bytes) -> string;

/// Convert a string to its UTF-8 bytes.
val from_string: fn(string) -> bytes;

/// Concatenate bytes values.
val concat: fn(@args: [bytes, Array<bytes>]) -> bytes;

/// Convert bytes to an Array<u8>.
val to_array: fn(bytes) -> Array<u8>;

/// Convert an Array<u8> to bytes.
val from_array: fn(Array<u8>) -> bytes;

/// Return the length of a bytes value.
val len: fn(bytes) -> u64;

/// Spec for encoding values into bytes. Bare tags are
/// big-endian (network byte order), LE suffix for little-endian.
type Encode = [
  `I8(i8), `U8(u8),
  `I16(i16), `I16LE(i16), `U16(u16), `U16LE(u16),
  `I32(i32), `I32LE(i32), `U32(u32), `U32LE(u32),
  `I64(i64), `I64LE(i64), `U64(u64), `U64LE(u64),
  `F32(f32), `F32LE(f32), `F64(f64), `F64LE(f64),
  `Bytes(bytes),
  `Pad(u64),
  `Varint(u64),
  `Zigzag(i64)
];

/// Spec for decoding bytes into refs. Bare tags are
/// big-endian (network byte order), LE suffix for little-endian.
/// Variable-length fields take a &u64 for the length so that
/// earlier decoded lengths can be resolved within the same call.
type Decode = [
  `I8(&i8), `U8(&u8),
  `I16(&i16), `I16LE(&i16), `U16(&u16), `U16LE(&u16),
  `I32(&i32), `I32LE(&i32), `U32(&u32), `U32LE(&u32),
  `I64(&i64), `I64LE(&i64), `U64(&u64), `U64LE(&u64),
  `F32(&f32), `F32LE(&f32), `F64(&f64), `F64LE(&f64),
  `Bytes(&u64, &bytes),
  `UTF8(&u64, &string),
  `Skip(&u64),
  `Varint(&u64),
  `Zigzag(&i64)
];

/// Encode values into bytes according to the spec.
val encode: fn(Array<Encode>) -> bytes;

/// Decode bytes into refs according to the spec.
/// Returns the remaining bytes after all fields are consumed.
val decode: fn(bytes, Array<Decode>) -> Result<bytes, `DecodeError(string)>;
```

## core::math

The `math` submodule wraps Rust's `f64` math intrinsics (trigonometric,
hyperbolic, exponential, logarithmic, power, rounding, comparison,
predicate, and angle-conversion routines) plus the standard
mathematical constants. Argument and result conventions match
`std::f64`: angles are in radians, NaN propagates through arithmetic,
and `min`/`max` return the non-NaN operand when one input is NaN.

For polymorphic n-ary `min` / `max` / `sum` / `product` over `Number`,
use the top-level functions in `core` instead — the bindings here are
the binary `f64`-only forms.

```graphix
/// Sine. Argument in radians.
val sin: fn(f64) -> f64;

/// Cosine. Argument in radians.
val cos: fn(f64) -> f64;

/// Tangent. Argument in radians.
val tan: fn(f64) -> f64;

/// Inverse sine. Result in radians, [-π/2, π/2].
val asin: fn(f64) -> f64;

/// Inverse cosine. Result in radians, [0, π].
val acos: fn(f64) -> f64;

/// Inverse tangent. Result in radians, (-π/2, π/2).
val atan: fn(f64) -> f64;

/// Four-quadrant inverse tangent: `atan2(y, x)`. Result in radians, (-π, π].
val atan2: fn(f64, f64) -> f64;

/// Hyperbolic sine.
val sinh: fn(f64) -> f64;

/// Hyperbolic cosine.
val cosh: fn(f64) -> f64;

/// Hyperbolic tangent.
val tanh: fn(f64) -> f64;

/// Inverse hyperbolic sine.
val asinh: fn(f64) -> f64;

/// Inverse hyperbolic cosine.
val acosh: fn(f64) -> f64;

/// Inverse hyperbolic tangent.
val atanh: fn(f64) -> f64;

/// `e^x`.
val exp: fn(f64) -> f64;

/// `2^x`.
val exp2: fn(f64) -> f64;

/// `e^x - 1`. More accurate than `exp(x) - 1` near zero.
val exp_m1: fn(f64) -> f64;

/// Natural logarithm (base e).
val ln: fn(f64) -> f64;

/// `ln(1 + x)`. More accurate than `ln(1 + x)` near zero.
val ln_1p: fn(f64) -> f64;

/// Logarithm base 2.
val log2: fn(f64) -> f64;

/// Logarithm base 10.
val log10: fn(f64) -> f64;

/// Logarithm with arbitrary base: `log(x, base) = ln(x) / ln(base)`.
val log: fn(f64, f64) -> f64;

/// `x^y`.
val pow: fn(f64, f64) -> f64;

/// Square root.
val sqrt: fn(f64) -> f64;

/// Cube root.
val cbrt: fn(f64) -> f64;

/// `sqrt(x^2 + y^2)`, computed without overflow for large inputs.
val hypot: fn(f64, f64) -> f64;

/// Largest integer less than or equal to `x`.
val floor: fn(f64) -> f64;

/// Smallest integer greater than or equal to `x`.
val ceil: fn(f64) -> f64;

/// Round to the nearest integer, ties away from zero.
val round: fn(f64) -> f64;

/// Truncate toward zero.
val trunc: fn(f64) -> f64;

/// Fractional part: `x - trunc(x)`.
val fract: fn(f64) -> f64;

/// Absolute value.
val abs: fn(f64) -> f64;

/// Sign: -1.0, 0.0, or 1.0. NaN if `x` is NaN.
val signum: fn(f64) -> f64;

/// `x` with the sign of `y`.
val copysign: fn(f64, f64) -> f64;

/// Smaller of two f64 values, returning the non-NaN operand if one is
/// NaN. For n-ary polymorphic behaviour over `Number`, use the
/// top-level `min` from core.
val min: fn(f64, f64) -> f64;

/// Larger of two f64 values, returning the non-NaN operand if one is
/// NaN. For n-ary polymorphic behaviour over `Number`, use the
/// top-level `max` from core.
val max: fn(f64, f64) -> f64;

/// Clamp `x` to the closed interval `[lo, hi]`.
val clamp: fn(f64, f64, f64) -> f64;

/// True if `x` is NaN.
val is_nan: fn(f64) -> bool;

/// True if `x` is finite (not NaN, not infinite).
val is_finite: fn(f64) -> bool;

/// True if `x` is positive or negative infinity.
val is_infinite: fn(f64) -> bool;

/// Convert radians to degrees.
val to_degrees: fn(f64) -> f64;

/// Convert degrees to radians.
val to_radians: fn(f64) -> f64;

/// π ≈ 3.14159265358979…
val pi: f64;

/// e ≈ 2.71828182845904…
val e: f64;

/// τ = 2π ≈ 6.28318530717958…
val tau: f64;

/// √2 ≈ 1.41421356237309…
val sqrt_2: f64;

/// ln(2) ≈ 0.69314718055994…
val ln_2: f64;

/// ln(10) ≈ 2.30258509299404…
val ln_10: f64;

/// Positive infinity.
val infinity: f64;

/// Not-a-number.
val nan: f64;
```

## core::opt

The `opt` submodule provides combinators for working with optional
values — graphix's `Option<'a>` is just the structural union
`['a, null]`, and these functions mirror the most useful parts of
Rust's `std::option::Option` API in a reactive setting.

The higher-order combinators (`map`, `flat_map`, `filter`, `or_else`,
`ok_or_else`, `is_some_and`, `is_none_or`) are deliberately
fire-and-forget: they never queue inputs. If a callback is slow or
never produces a value, a new input simply supersedes the pending one
(latest wins). Use the explicit `core::queue` / `core::hold` operators
when you need ordered async behavior.

```graphix
/// true if v is not null
val is_some: fn(['a, null]) -> bool;

/// true if v is null
val is_none: fn(['a, null]) -> bool;

/// true if v is not null and equals x. Produces false when v is null
/// regardless of x.
val contains: fn(['a, null], 'a) -> bool;

/// or_never(v) does not return anything if v is null, otherwise it
/// returns v.
val or_never: fn(['a, null]) -> 'a;

/// or_default(v, d): if v is null return d, otherwise return v
val or_default: fn(['a, null], 'a) -> 'a;

/// or(a, b): return a if non-null, otherwise return b (which may
/// itself be null).
val or: fn(['a, null], ['a, null]) -> ['a, null];

/// and(a, b): return b if a is non-null, otherwise return null. The
/// value of b is not inspected when a is null.
val and: fn(['a, null], ['b, null]) -> ['b, null];

/// xor(a, b): return whichever of a or b is non-null, or null if both
/// or neither are non-null.
val xor: fn(['a, null], ['a, null]) -> ['a, null];

/// Collapse a nested option. Present for symmetry with Rust; at the
/// value level this is the identity (graphix structural unions make
/// [[T, null], null] equivalent to [T, null]).
val flatten: fn([['a, null], null]) -> ['a, null];

/// ok_or(v, e): return v unchanged when non-null, otherwise return
/// error(e).
val ok_or: fn(['a, null], 'e) -> Result<'a, 'e>;

/// zip(a, b): if both are non-null return the tuple (a, b), otherwise
/// return null.
val zip: fn(['a, null], ['b, null]) -> [('a, 'b), null];

/// unzip(p): given a tuple-valued option, return the pair of options.
/// null input yields (null, null).
val unzip: fn([('a, 'b), null]) -> (['a, null], ['b, null]);

/// map(v, f): apply f to the inner value if v is non-null, otherwise
/// return null.
val map: fn(['a, null], fn('a) -> 'b) -> ['b, null];

/// flat_map(v, f): apply f to the inner value if v is non-null,
/// otherwise return null. f's own optional result is forwarded
/// unchanged.
val flat_map: fn(['a, null], fn('a) -> ['b, null]) -> ['b, null];

/// filter(v, pred): if v is non-null and pred(v) is true, emit v.
/// Otherwise (v is null, or pred is false) emit null. Same fire-and-
/// forget semantics as map.
///
/// Caveat for reactive predicates: when pred takes more than one cycle
/// to produce its bool, the emitted value is the *current* input, not
/// the input pred was actually answering for. If the input changes
/// between feeding pred and pred firing, you'll see the new value
/// gated on a verdict about the old one. Pure predicates (the
/// expected case) are unaffected. Use `core::queue` if you need
/// strict input-bool pairing.
val filter: fn(['a, null], fn('a) -> bool) -> ['a, null];

/// or_else(v, f): return v if non-null, otherwise return whatever f()
/// produces. f is invoked eagerly and its latest value is cached so
/// later updates to v can be resolved without re-invoking f. If v is
/// null before f has produced its first value, no output is emitted
/// until f fires.
val or_else: fn(['a, null], fn() -> ['a, null]) -> ['a, null];

/// ok_or_else(v, f): return v if non-null, otherwise return
/// error(f()). Same eager-with-caching semantics as or_else; null v
/// before f's first firing is silent until f produces.
val ok_or_else: fn(['a, null], fn() -> 'e) -> Result<'a, 'e>;

/// is_some_and(v, pred): true when v is non-null and pred(v) is true,
/// false when v is null or pred is false. Fire-and-forget like map —
/// for non-null v, no output is produced until pred fires.
val is_some_and: fn(['a, null], fn('a) -> bool) -> bool;

/// is_none_or(v, pred): true when v is null or pred(v) is true, false
/// when v is non-null and pred is false. Fire-and-forget like map —
/// for non-null v, no output is produced until pred fires.
val is_none_or: fn(['a, null], fn('a) -> bool) -> bool;
```
