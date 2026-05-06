# Str

```graphix
type Escape = {
    escape: string,
    escape_char: string,
    tr: Array<(string, string)>
};

/// return true if s starts with #pfx, otherwise return false
val starts_with: fn(#pfx: string, s: string) -> bool;

/// return true if s ends with #sfx otherwise return false
val ends_with: fn(#sfx: string, s: string) -> bool;

/// return true if s contains #part, otherwise return false
val contains: fn(#part: string, s: string) -> bool;

/// if s starts with #pfx then return s with #pfx stripped otherwise return null
val strip_prefix: fn(#pfx: string, s: string) -> Option<string>;

/// if s ends with #sfx then return s with #sfx stripped otherwise return null
val strip_suffix: fn(#sfx: string, s: string) -> Option<string>;

/// return s with leading and trailing whitespace removed
val trim: fn(s: string) -> string;

/// return s with leading whitespace removed
val trim_start: fn(s: string) -> string;

/// return s with trailing whitespace removed
val trim_end: fn(s: string) -> string;

/// replace all instances of #pat in s with #rep and return s
val replace: fn(#pat: string, #rep: string, s: string) -> string;

/// return the parent path of s, or null if s does not have a parent path
val dirname: fn(s: string) -> Option<string>;

/// return the leaf path of s, or null if s is not a path. e.g. /foo/bar -> bar
val basename: fn(s: string) -> Option<string>;

/// given a path ending in .../row/col, return the tuple (row, col).
/// equivalent to (basename(dirname(s)), basename(s)), but in a single
/// builtin call. returns null if s has fewer than two path components.
/// e.g. /foo/bar/baz -> ("bar", "baz"), /foo -> null
val row_col: fn(s: string) -> Option<(string, string)>;

/// return a single string with the arguments concatenated and separated by #sep
val join: fn(#sep:string, @args: [string, Array<string>]) -> string;

/// concatenate the specified strings into a single string
val concat: fn(@args: [string, Array<string>]) -> string;

/// escape all the characters in #to_escape in s with the escape character #escape.
/// The escape character must appear in #to_escape
val escape: fn(?#esc:Escape, s: string) -> Result<string, `StringError(string)>;

/// unescape all the characters in s escaped by the specified #escape character
val unescape: fn(?#esc:Escape, s: string) -> Result<string, `StringError(string)>;

/// split the string by the specified #pat and return an array of each part
val split: fn(#pat: string, s: string) -> Array<string>;

/// reverse split the string by the specified #pat and return an array of each part
val rsplit: fn(#pat: string, s: string) -> Array<string>;

/// split the string at most #n times by the specified #pat and return an array of
/// each part
val splitn: fn(#pat:string, #n:i64, s: string) -> Result<Array<string>, `StringSplitError(string)>;

/// reverse split the string at most #n times by the specified #pat and return an array of
/// each part
val rsplitn: fn(#pat:string, #n:i64, s: string) -> Result<Array<string>, `StringSplitError(string)>;

/// give an escape character #esc, and a #sep character, split the string s into an array
/// of parts delimited by it's non escaped separator characters.
val split_escaped: fn(#esc:string, #sep:string, s: string) -> Result<Array<string>, `SplitEscError(string)>;

/// give an escape character #esc, and a #sep character, split the string s into an array
/// of at most #n parts delimited by it's non escaped separator characters.
val splitn_escaped: fn(#n:i64, #esc:string, #sep:string, s: string) -> Result<Array<string>, `SplitNEscError(string)>;

/// split the string once from the beginning by #pat and return a
/// tuple of strings, or return null if #pat was not found in the string
val split_once: fn(#pat: string, s: string) -> Option<(string, string)>;

/// split the string once from the end by #pat and return a tuple of strings
/// or return null if #pat was not found in the string
val rsplit_once: fn(#pat: string, s: string) -> Option<(string, string)>;

/// change the string to lowercase
val to_lower: fn(s: string) -> string;

/// change the string to uppercase
val to_upper: fn(s: string) -> string;

/// C style sprintf, implements most C standard format args
val sprintf: fn(s: string, @args: Any) -> string;

/// return the length of the string in bytes
val len: fn(s: string) -> i64;

/// extract a substring of s starting at #start with length #len.
/// both #start and #len are Unicode character indexes,
/// not byte indexes. e.g. str::sub(#start:0, #len:2, "💖💖💖")
/// will return "💖💖"
val sub: fn(#start:i64, #len:i64, s: string) -> Result<string, `SubError(string)>;

/// parse the specified string as a value. return the value on success or an
/// error on failure. Note, if you feed the parser a well formed error then
/// parse will also return an error
val parse: fn(s: string) -> Result<PrimNoErr, Any>;
```
