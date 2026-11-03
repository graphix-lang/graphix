//! The one stream type every parse runs over. A parse runs twice only when
//! it fails: first with errors that cost nothing, then, to explain the
//! failure, with `easy`'s. The grammar is generic over its stream, so two
//! stream types would compile it twice; this one switches its errors at
//! run time instead ([`explaining`]).
use super::Lines;
use combine::{
    ParseError, Positioned, RangeStreamOnce, StreamOnce,
    easy::{self, Errors},
    error::{ErrorInfo, ParseResult, StreamError, StringStreamError, Tracked},
    stream::{
        ResetStream,
        position::{self, SourcePosition},
    },
};
use std::{cell::Cell, fmt};

thread_local! {
    /// Whether the errors made now are `easy`'s.
    static EXPLAINING: Cell<bool> = const { Cell::new(false) };
}

/// Run the parse `f` with `easy` errors, which say what was expected.
pub(super) fn explaining<'a, T>(
    f: impl FnOnce() -> Result<T, GxErrors<'a>>,
) -> Result<T, EasyErrors<'a>> {
    let _explaining = crate::Restore::replace(&EXPLAINING, true);
    f().map_err(|e| match e {
        GxErrors::Easy(e) => e,
        GxErrors::Quiet(_) => mixed(),
    })
}

fn is_explaining() -> bool {
    EXPLAINING.get()
}

type EasyError<'a> = easy::Error<char, &'a str>;
type EasyErrors<'a> = Errors<char, &'a str, SourcePosition>;

/// Every error a parse makes is of the kind [`EXPLAINING`] gave as it
/// began: the flag changes only around a whole parse ([`explaining`]) and
/// is restored however that ends, a parse runs on one thread (a stack
/// switch keeps it), and a parse nested in another makes its own errors,
/// which leave it only as text.
fn mixed() -> ! {
    unreachable!("a parse made an error of the other kind")
}

/// One error: `Quiet`'s are the plain `&str` stream's, `Easy`'s
/// [`explaining`]'s.
#[derive(Debug, PartialEq)]
pub(super) enum GxError<'a> {
    Quiet(StringStreamError),
    Easy(EasyError<'a>),
}

macro_rules! either {
    ($f:ident($($arg:expr),*)) => {
        if is_explaining() {
            GxError::Easy(EasyError::$f($($arg),*))
        } else {
            GxError::Quiet(<StringStreamError as StreamError<char, &'a str>>::$f($($arg),*))
        }
    };
}

impl<'a> StreamError<char, &'a str> for GxError<'a> {
    fn unexpected_token(token: char) -> Self {
        either!(unexpected_token(token))
    }

    fn unexpected_range(token: &'a str) -> Self {
        either!(unexpected_range(token))
    }

    fn unexpected_format<T: fmt::Display>(msg: T) -> Self {
        either!(unexpected_format(msg))
    }

    fn unexpected<E: for<'s> ErrorInfo<'s, char, &'a str>>(info: E) -> Self {
        either!(unexpected(info))
    }

    fn unexpected_static_message(msg: &'static str) -> Self {
        either!(unexpected_static_message(msg))
    }

    fn expected_token(token: char) -> Self {
        either!(expected_token(token))
    }

    fn expected_range(token: &'a str) -> Self {
        either!(expected_range(token))
    }

    fn expected_format<T: fmt::Display>(msg: T) -> Self {
        either!(expected_format(msg))
    }

    fn expected<E: for<'s> ErrorInfo<'s, char, &'a str>>(info: E) -> Self {
        either!(expected(info))
    }

    fn expected_static_message(msg: &'static str) -> Self {
        either!(expected_static_message(msg))
    }

    fn message_token(token: char) -> Self {
        either!(message_token(token))
    }

    fn message_range(token: &'a str) -> Self {
        either!(message_range(token))
    }

    fn message_format<T: fmt::Display>(msg: T) -> Self {
        either!(message_format(msg))
    }

    fn message_static_message(msg: &'static str) -> Self {
        either!(message_static_message(msg))
    }

    fn message<E: for<'s> ErrorInfo<'s, char, &'a str>>(info: E) -> Self {
        either!(message(info))
    }

    fn other<E: std::error::Error + Send + Sync + 'static>(err: E) -> Self {
        either!(other(err))
    }

    fn end_of_input() -> Self {
        either!(end_of_input())
    }

    fn is_unexpected_end_of_input(&self) -> bool {
        match self {
            Self::Quiet(e) => StreamError::<char, &str>::is_unexpected_end_of_input(e),
            Self::Easy(e) => StreamError::<char, &str>::is_unexpected_end_of_input(e),
        }
    }

    fn into_other<T: StreamError<char, &'a str>>(self) -> T {
        match self {
            Self::Quiet(e) => StreamError::<char, &str>::into_other(e),
            Self::Easy(e) => StreamError::<char, &str>::into_other(e),
        }
    }
}

/// A parse's errors, of the kind its [`GxError`]s are.
#[derive(Debug, PartialEq)]
pub(super) enum GxErrors<'a> {
    Quiet(StringStreamError),
    Easy(EasyErrors<'a>),
}

impl<'a> GxErrors<'a> {
    fn easy_mut(&mut self) -> &mut EasyErrors<'a> {
        match self {
            Self::Easy(e) => e,
            Self::Quiet(_) => mixed(),
        }
    }
}

type Quiet = StringStreamError;

impl<'a> ParseError<char, &'a str, SourcePosition> for GxErrors<'a> {
    type StreamError = GxError<'a>;

    fn empty(position: SourcePosition) -> Self {
        if is_explaining() {
            Self::Easy(EasyErrors::empty(position))
        } else {
            Self::Quiet(<Quiet as ParseError<char, &str, SourcePosition>>::empty(
                position,
            ))
        }
    }

    fn from_error(position: SourcePosition, err: GxError<'a>) -> Self {
        match err {
            GxError::Quiet(e) => Self::Quiet(<Quiet as ParseError<
                char,
                &str,
                SourcePosition,
            >>::from_error(position, e)),
            GxError::Easy(e) => Self::Easy(EasyErrors::from_error(position, e)),
        }
    }

    fn position(&self) -> SourcePosition {
        match self {
            Self::Quiet(e) => ParseError::<char, &str, SourcePosition>::position(e),
            Self::Easy(e) => e.position,
        }
    }

    fn set_position(&mut self, position: SourcePosition) {
        match self {
            Self::Quiet(e) => {
                ParseError::<char, &str, SourcePosition>::set_position(e, position)
            }
            Self::Easy(e) => e.position = position,
        }
    }

    fn merge(self, other: Self) -> Self {
        match (self, other) {
            (Self::Quiet(a), Self::Quiet(b)) => {
                Self::Quiet(ParseError::<char, &str, SourcePosition>::merge(a, b))
            }
            (Self::Easy(a), Self::Easy(b)) => Self::Easy(a.merge(b)),
            (Self::Quiet(_), Self::Easy(_)) | (Self::Easy(_), Self::Quiet(_)) => mixed(),
        }
    }

    fn add(&mut self, err: GxError<'a>) {
        match (self, err) {
            (Self::Quiet(a), GxError::Quiet(b)) => {
                ParseError::<char, &str, SourcePosition>::add(a, b)
            }
            (Self::Easy(a), GxError::Easy(b)) => ParseError::add(a, b),
            (Self::Quiet(_), GxError::Easy(_)) | (Self::Easy(_), GxError::Quiet(_)) => {
                mixed()
            }
        }
    }

    fn add_expected<E: for<'s> ErrorInfo<'s, char, &'a str>>(&mut self, info: E) {
        match self {
            Self::Quiet(e) => {
                ParseError::<char, &'a str, SourcePosition>::add_expected(e, info)
            }
            Self::Easy(e) => e.add_expected(info),
        }
    }

    fn add_unexpected<E: for<'s> ErrorInfo<'s, char, &'a str>>(&mut self, info: E) {
        match self {
            Self::Quiet(e) => {
                ParseError::<char, &'a str, SourcePosition>::add_unexpected(e, info)
            }
            Self::Easy(e) => e.add_unexpected(info),
        }
    }

    fn add_message<E: for<'s> ErrorInfo<'s, char, &'a str>>(&mut self, info: E) {
        match self {
            Self::Quiet(e) => {
                ParseError::<char, &'a str, SourcePosition>::add_message(e, info)
            }
            Self::Easy(e) => e.add_message(info),
        }
    }

    fn set_expected<F: FnOnce(&mut Tracked<Self>)>(
        self_: &mut Tracked<Self>,
        info: GxError<'a>,
        f: F,
    ) {
        match info {
            GxError::Quiet(info) => {
                f(self_);
                self_.error = Self::Quiet(info)
            }
            GxError::Easy(info) => {
                let start = self_.error.easy_mut().errors.len();
                f(self_);
                let e = self_.error.easy_mut();
                let mut i = 0;
                e.errors.retain(|e| {
                    i += 1;
                    i <= start || !matches!(e, easy::Error::Expected(_))
                });
                e.add_error(info)
            }
        }
    }

    fn clear_expected(&mut self) {
        match self {
            Self::Quiet(_) => (),
            Self::Easy(e) => ParseError::<char, &str, SourcePosition>::clear_expected(e),
        }
    }

    fn is_unexpected_end_of_input(&self) -> bool {
        match self {
            Self::Quiet(e) => {
                ParseError::<char, &str, SourcePosition>::is_unexpected_end_of_input(e)
            }
            Self::Easy(e) => {
                ParseError::<char, &str, SourcePosition>::is_unexpected_end_of_input(e)
            }
        }
    }

    fn into_other<T: ParseError<char, &'a str, SourcePosition>>(self) -> T {
        match self {
            Self::Quiet(e) => ParseError::<char, &str, SourcePosition>::into_other(e),
            Self::Easy(e) => ParseError::<char, &str, SourcePosition>::into_other(e),
        }
    }
}

impl fmt::Display for GxErrors<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Quiet(e) => e.fmt(f),
            Self::Easy(e) => e.fmt(f),
        }
    }
}

/// `text` with [`Lines`] positions and [`GxErrors`].
#[derive(Clone, Debug)]
pub(super) struct GxStream<'a>(position::Stream<&'a str, Lines>);

impl<'a> GxStream<'a> {
    pub(super) fn new(text: &'a str) -> Self {
        Self(position::Stream::with_positioner(text, Lines::default()))
    }
}

fn lift<'a>(e: StringStreamError) -> GxError<'a> {
    if is_explaining() {
        GxError::Easy(StreamError::<char, &str>::into_other(e))
    } else {
        GxError::Quiet(e)
    }
}

impl<'a> StreamOnce for GxStream<'a> {
    type Token = char;
    type Range = &'a str;
    type Position = SourcePosition;
    type Error = GxErrors<'a>;

    #[inline]
    fn uncons(&mut self) -> Result<char, GxError<'a>> {
        self.0.uncons().map_err(lift)
    }

    fn is_partial(&self) -> bool {
        false
    }
}

impl<'a> ResetStream for GxStream<'a> {
    type Checkpoint = <position::Stream<&'a str, Lines> as ResetStream>::Checkpoint;

    #[inline]
    fn checkpoint(&self) -> Self::Checkpoint {
        self.0.checkpoint()
    }

    #[inline]
    fn reset(&mut self, checkpoint: Self::Checkpoint) -> Result<(), GxErrors<'a>> {
        self.0.reset(checkpoint).map_err(|e| {
            if is_explaining() {
                GxErrors::Easy(ParseError::<char, &str, SourcePosition>::into_other(e))
            } else {
                GxErrors::Quiet(e)
            }
        })
    }
}

impl<'a> Positioned for GxStream<'a> {
    #[inline]
    fn position(&self) -> SourcePosition {
        self.0.position()
    }
}

impl<'a> RangeStreamOnce for GxStream<'a> {
    #[inline]
    fn uncons_range(&mut self, size: usize) -> Result<&'a str, GxError<'a>> {
        self.0.uncons_range(size).map_err(lift)
    }

    #[inline]
    fn uncons_while<F: FnMut(char) -> bool>(
        &mut self,
        f: F,
    ) -> Result<&'a str, GxError<'a>> {
        self.0.uncons_while(f).map_err(lift)
    }

    #[inline]
    fn uncons_while1<F: FnMut(char) -> bool>(
        &mut self,
        f: F,
    ) -> ParseResult<&'a str, GxError<'a>> {
        self.0.uncons_while1(f).map_err(lift)
    }

    #[inline]
    fn distance(&self, end: &Self::Checkpoint) -> usize {
        self.0.distance(end)
    }

    fn range(&self) -> &'a str {
        self.0.range()
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use combine::{Parser, parser::char::char};

    /// A parse's errors are free unless it explains, and explaining ends
    /// with its parse, a failure's included.
    #[test]
    fn errors_are_easy_only_while_explaining() {
        let fails = || char('a').parse(GxStream::new("b")).map(|(c, _)| c);
        assert!(matches!(fails(), Err(GxErrors::Quiet(_))));
        let explained = explaining(fails).unwrap_err();
        assert_eq!(explained.to_string().lines().nth(1), Some("Unexpected `b`"));
        assert!(!is_explaining());
        assert!(super::super::parse_one("let x = ").is_err());
        assert!(!is_explaining());
        assert!(matches!(fails(), Err(GxErrors::Quiet(_))));
    }
}
