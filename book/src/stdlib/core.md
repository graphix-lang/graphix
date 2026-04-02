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
