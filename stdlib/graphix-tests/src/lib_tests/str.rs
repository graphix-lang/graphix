use anyhow::Result;
use graphix_package_core::{
    run,
    testing::{FuseExpect, is, prints, refused},
};
use netidx::subscriber::Value;

const STR_STARTS_WITH: &str = r#"
  str::starts_with(#pfx:"foo", "foobarbaz")
"#;

run!(str_starts_with, STR_STARTS_WITH, |v: Result<&Value>| {
    match v {
        Ok(Value::Bool(true)) => true,
        _ => false,
    }
});

const STR_ENDS_WITH: &str = r#"
  str::ends_with(#sfx:"baz", "foobarbaz")
"#;

run!(str_ends_with, STR_ENDS_WITH, |v: Result<&Value>| {
    match v {
        Ok(Value::Bool(true)) => true,
        _ => false,
    }
});

const STR_CONTAINS: &str = r#"
  str::contains(#part:"bar", "foobarbaz")
"#;

run!(str_contains, STR_CONTAINS, |v: Result<&Value>| {
    match v {
        Ok(Value::Bool(true)) => true,
        _ => false,
    }
});

const STR_STRIP_PREFIX: &str = r#"
  str::strip_prefix(#pfx:"foo", "foobarbaz")
"#;

run!(str_strip_prefix, STR_STRIP_PREFIX, |v: Result<&Value>| {
    match v {
        Ok(Value::String(s)) => s == "barbaz",
        _ => false,
    }
});

const STR_STRIP_SUFFIX: &str = r#"
  str::strip_suffix(#sfx:"baz", "foobarbaz")
"#;

run!(str_strip_suffix, STR_STRIP_SUFFIX, |v: Result<&Value>| {
    match v {
        Ok(Value::String(s)) => s == "foobar",
        _ => false,
    }
});

const STR_TRIM: &str = r#"
  str::trim(" foobarbaz ")
"#;

run!(str_trim, STR_TRIM, |v: Result<&Value>| {
    match v {
        Ok(Value::String(s)) => s == "foobarbaz",
        _ => false,
    }
});

const STR_TRIM_START: &str = r#"
  str::trim_start(" foobarbaz ")
"#;

run!(str_trim_start, STR_TRIM_START, |v: Result<&Value>| {
    match v {
        Ok(Value::String(s)) => s == "foobarbaz ",
        _ => false,
    }
});

const STR_TRIM_END: &str = r#"
  str::trim_end(" foobarbaz ")
"#;

run!(str_trim_end, STR_TRIM_END, |v: Result<&Value>| {
    match v {
        Ok(Value::String(s)) => s == " foobarbaz",
        _ => false,
    }
});

const STR_REPLACE: &str = r#"
  str::replace(#pat:"foo", #rep:"baz", "foobarbazfoo")
"#;

run!(str_replace, STR_REPLACE, |v: Result<&Value>| {
    match v {
        Ok(Value::String(s)) => s == "bazbarbazbaz",
        _ => false,
    }
});

const STR_DIRNAME: &str = r#"
  str::dirname("/foo/bar/baz")
"#;

run!(str_dirname, STR_DIRNAME, |v: Result<&Value>| {
    match v {
        Ok(Value::String(s)) => s == "/foo/bar",
        _ => false,
    }
});

// A join whose first part is empty keeps its separator, and a split
// joins back to its source.
const STR_JOIN_EDGES: &str = r#"
  (str::join(#sep: ",", "", "b"), str::join(#sep: "/", str::split(#pat: "/", "/usr/bin")))
"#;

run!(str_join_edges, STR_JOIN_EDGES, prints(r#"[",b", "/usr/bin"]"#));

// A path with no parent has no dirname, and one a level below the root
// has the root; a trailing `/` names an empty leaf (netidx paths, not
// POSIX), so its dirname is the path before it.
const STR_DIRNAME_EDGES: &str = r#"
  (str::dirname("foo"), str::dirname(""), str::dirname("/foo"), str::dirname("/foo/"))
"#;

run!(str_dirname_edges, STR_DIRNAME_EDGES, prints(r#"[null, null, "/", "/foo"]"#));

const STR_BASENAME: &str = r#"
  str::basename("/foo/bar/baz")
"#;

run!(str_basename, STR_BASENAME, |v: Result<&Value>| {
    match v {
        Ok(Value::String(s)) => s == "baz",
        _ => false,
    }
});

const STR_ROW_COL: &str = r#"
  str::row_col("/foo/bar/baz")
"#;

run!(str_row_col, STR_ROW_COL, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::String(s0), Value::String(s1)] => s0 == "bar" && s1 == "baz",
            _ => false,
        },
        _ => false,
    }
});

const STR_ROW_COL_TWO: &str = r#"
  str::row_col("/foo/bar")
"#;

run!(str_row_col_two, STR_ROW_COL_TWO, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::String(s0), Value::String(s1)] => s0 == "foo" && s1 == "bar",
            _ => false,
        },
        _ => false,
    }
});

const STR_ROW_COL_SINGLE: &str = r#"
  str::row_col("/foo")
"#;

run!(str_row_col_single, STR_ROW_COL_SINGLE, |v: Result<&Value>| {
    matches!(v, Ok(Value::Null))
});

const STR_ROW_COL_ROOT: &str = r#"
  str::row_col("/")
"#;

run!(str_row_col_root, STR_ROW_COL_ROOT, |v: Result<&Value>| {
    matches!(v, Ok(Value::Null))
});

const STR_ROW_COL_EMPTY: &str = r#"
  str::row_col("")
"#;

run!(str_row_col_empty, STR_ROW_COL_EMPTY, |v: Result<&Value>| {
    matches!(v, Ok(Value::Null))
});

const STR_JOIN: &str = r#"
  str::join(#sep:"/", "/foo", "bar", ["baz", "zam"])
"#;

run!(str_join, STR_JOIN, |v: Result<&Value>| {
    match v {
        Ok(Value::String(s)) => s == "/foo/bar/baz/zam",
        _ => false,
    }
});

const STR_CONCAT: &str = r#"
  str::concat("foo", "bar", ["baz", "zam"])
"#;

run!(str_concat, STR_CONCAT, |v: Result<&Value>| {
    match v {
        Ok(Value::String(s)) => s == "foobarbazzam",
        _ => false,
    }
});

const STR_ESCAPE: &str = r#"
  str::escape("/foo/bar")
"#;

run!(str_escape, STR_ESCAPE, |v: Result<&Value>| {
    match v {
        Ok(Value::String(s)) => s == "\\/foo\\/bar",
        _ => false,
    }
}; FuseExpect::Jit);

// The defaulted `#esc` marshals the site's compiled default node, so
// the common spelling fuses natively; the escape table is a
// thread-local `FastMemo` entry keyed by the config value.
const STR_ESCAPE_NATIVE: &str = r#"{
  let e = |s: string| { let r = #[native] str::escape(s)$; r };
  e("/foo/bar")
}"#;

run!(str_escape_native, STR_ESCAPE_NATIVE, |v: Result<&Value>| {
    match v {
        Ok(Value::String(s)) => s == "\\/foo\\/bar",
        _ => false,
    }
});

const STR_UNESCAPE: &str = r#"
  str::unescape("\\/foo\\/bar")
"#;

run!(str_unescape, STR_UNESCAPE, |v: Result<&Value>| {
    match v {
        Ok(Value::String(s)) => s == "/foo/bar",
        _ => false,
    }
}; FuseExpect::Jit);

const STR_SPLIT: &str = r#"
{
  let a = str::split(#pat:",", "foo, bar, baz");
  array::map(a, |s| str::trim(s))
}
"#;

run!(str_split, STR_SPLIT, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::String(s0), Value::String(s1), Value::String(s2)] => {
                s0 == "foo" && s1 == "bar" && s2 == "baz"
            }
            _ => false,
        },
        _ => false,
    }
}; FuseExpect::Jit);

const STR_RSPLIT: &str = r#"
{
  let a = str::rsplit(#pat:",", "foo, bar, baz");
  array::map(a, |s| str::trim(s))
}
"#;

run!(str_rsplit, STR_RSPLIT, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::String(s0), Value::String(s1), Value::String(s2)] => {
                s0 == "baz" && s1 == "bar" && s2 == "foo"
            }
            _ => false,
        },
        _ => false,
    }
}; FuseExpect::Jit);

const STR_SPLITN: &str = r#"
{
  let a = str::splitn(#pat:",", #n:2, "foo, bar, baz")?;
  array::map(a, |s| str::trim(s))
}
"#;

run!(str_splitn, STR_SPLITN, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::String(s0), Value::String(s1)] => s0 == "foo" && s1 == "bar, baz",
            _ => false,
        },
        _ => false,
    }
}; FuseExpect::Jit);

const STR_RSPLITN: &str = r#"
{
  let a = str::rsplitn(#pat:",", #n:2, "foo, bar, baz")?;
  array::map(a, |s| str::trim(s))
}
"#;

run!(str_rsplitn, STR_RSPLITN, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::String(s0), Value::String(s1)] => s0 == "baz" && s1 == "foo, bar",
            _ => false,
        },
        _ => false,
    }
}; FuseExpect::Jit);

const STR_SPLIT_ESCAPED: &str = r#"
{
  let a = str::split_escaped(#esc:"\\", #sep:",", "foo\\, bar, baz")?;
  array::map(a, |s| str::trim(s))
}
"#;

run!(str_split_escaped, STR_SPLIT_ESCAPED, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::String(s0), Value::String(s1)] => s0 == "foo\\, bar" && s1 == "baz",
            _ => false,
        },
        _ => false,
    }
}; FuseExpect::Jit);

const STR_SPLITN_ESCAPED: &str = r#"
{
  let a = str::splitn_escaped(#n:2, #esc:"\\", #sep:",", "foo\\, bar, baz, bam")?;
  array::map(a, |s| str::trim(s))
}
"#;

run!(str_splitn_escaped, STR_SPLITN_ESCAPED, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::String(s0), Value::String(s1)] => {
                s0 == "foo\\, bar" && s1 == "baz, bam"
            }
            _ => false,
        },
        _ => false,
    }
}; FuseExpect::Jit);

const STR_SPLIT_ONCE: &str = r#"
  str::split_once(#pat:", ", "foo, bar, baz")
"#;

run!(str_split_once, STR_SPLIT_ONCE, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::String(s0), Value::String(s1)] => s0 == "foo" && s1 == "bar, baz",
            _ => false,
        },
        _ => false,
    }
});

const STR_RSPLIT_ONCE: &str = r#"
  str::rsplit_once(#pat:", ", "foo, bar, baz")
"#;

run!(str_rsplit_once, STR_RSPLIT_ONCE, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::String(s0), Value::String(s1)] => s0 == "foo, bar" && s1 == "baz",
            _ => false,
        },
        _ => false,
    }
});

const STR_TO_LOWER: &str = r#"
  str::to_lower("FOO")
"#;

run!(str_to_lower, STR_TO_LOWER, |v: Result<&Value>| {
    match v {
        Ok(Value::String(s)) => s == "foo",
        _ => false,
    }
});

const STR_TO_UPPER: &str = r#"
  str::to_upper("foo")
"#;

run!(str_to_upper, STR_TO_UPPER, |v: Result<&Value>| {
    match v {
        Ok(Value::String(s)) => s == "FOO",
        _ => false,
    }
});

const STR_LEN: &str = r#"
  str::len("foo")
"#;

run!(str_len, STR_LEN, |v: Result<&Value>| {
    match v {
        Ok(Value::I64(3)) => true,
        _ => false,
    }
});

const STR_CHAR_LEN: &str = r#"
  (str::char_len("💖é"), str::len("💖é"))
"#;

run!(str_char_len, STR_CHAR_LEN, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => a[..] == [Value::I64(2), Value::I64(6)],
        _ => false,
    }
});

const STR_SUB: &str = r#"
  str::sub(#start:1, #len:2, "💗💖🍇")
"#;

run!(str_sub, STR_SUB, |v: Result<&Value>| {
    match v {
        Ok(Value::String(s)) if &*s == "💖🍇" => true,
        _ => false,
    }
});

// A range past the end is cut at it; the whole range is the string.
run!(
    str_sub_bounds,
    r#"{
  let s = "💗💖🍇";
  "[str::sub(#start:2, #len:5, s)]|[str::sub(#start:3, #len:1, s)]|[str::sub(#start:0, #len:3, s)]"
}"#,
    |v: Result<&Value>| {
        matches!(v, Ok(Value::String(s)) if &**s == "🍇||💗💖🍇")
    }; FuseExpect::Jit
);

// A `%s` precision counts bytes and never cuts inside a char; a missing
// or mistyped argument is a FormatError, never a panic.
run!(
    str_sprintf_precision_and_errors,
    r#"{
  let hearts = str::sprintf("%.3s|%.5s|%.*s|", "💖💖💖💖", "💖💖💖💖", 9, "💖💖💖💖");
  let missing = select str::sprintf("%d %d", 1) { error as _ => "error", s => s };
  let mistyped = select str::sprintf("%d", "x") { error as _ => "error", s => s };
  "[hearts$] [missing] [mistyped]"
}"#,
    |v: Result<&Value>| {
        matches!(v, Ok(Value::String(s)) if &**s == "|💖|💖💖| error error")
    }
);

const STR_PARSE: &str = r#"{
  let v: i64 = str::parse("42")?;
  v
}"#;

run!(str_parse, STR_PARSE, |v: Result<&Value>| {
    match v {
        Ok(Value::I64(42)) => true,
        _ => false,
    }
}; FuseExpect::Jit);

// A read can't forge a reference: `Concrete` excludes one.
const STR_PARSE_REF_TARGET: &str = r#"{
  let x = 41;
  let r: &mut i64 = str::parse("u64:1")$;
  *r <- 1;
  x
}"#;

run!(parse_refuses_a_reference_target, STR_PARSE_REF_TARGET, refused("Concrete"); FuseExpect::None);

// The result's own error member covers the annotation's, so the target
// takes only the reference, which `Concrete` refuses.
run!(
    parse_refuses_a_reference_beside_its_error,
    r#"{ let r: [&i64, Error<`ParseError(string)>] = str::parse("u64:7"); r }"#,
    refused("Concrete");
    FuseExpect::None
);

// The target takes the annotation's member that is not the error.
run!(
    parse_target_is_not_its_own_error,
    r#"{ let r: [i64, Error<`ParseError(string)>] = str::parse("7"); r$ + 1 }"#,
    is(Value::I64(8));
    FuseExpect::Jit
);

// Text that is no value names itself and the target, not the value grammar.
run!(
    parse_error_names_the_text_and_target,
    r#"{ let r: [i64, Error<`ParseError(string)>] = str::parse("x1"); select r { error as e => e.0, _ => never() } }"#,
    |v: Result<&Value>| matches!(
        v,
        Ok(Value::Array(a)) if a[0] == Value::from("ParseError") && a[1] == Value::from("cannot parse \"x1\" as i64")
    )
);

// A number is read by the cast, so every u64 parses, and the cast's
// refusal is its message, never the variant it travels in
// (x-engine-collections-03, x-engine-collections-01, collections-str-02,
// x-rerun-5-03, x-builtin-effects-06).
run!(
    parse_reads_numbers_by_the_cast,
    r#"{
  type E = `ParseError(string);
  let big: Result<u64, E> = str::parse(cast<string>(u64:18446744073709551615)$);
  let byte: Result<u8, E> = str::parse("300");
  let pair: Result<(i64, string), E> = str::parse("1.5");
  let msg = |r: Any| select r { error as e => "[e]", v => "[v]" };
  "[big$] [msg(byte)] [msg(pair)]"
}"#,
    |v: Result<&Value>| matches!(
        v,
        Ok(Value::String(s)) if &**s == r#"18446744073709551615 error:`ParseError("cannot parse \"300\" as u8") error:`ParseError("can't cast 1.5 to (i64, string): not a tuple")"#
    )
);

// sprintf prints a composite as interpolation does, never its kind
// marker, and refuses to pad past its cap rather than allocate without
// bound (x-builtin-effects-01, collections-str-01, x-panics-05,
// x-panics-04).
run!(
    sprintf_prints_composites_and_caps_padding,
    r#"{
  let c = str::sprintf("%s|%s|%s|%s", (1, 2), {x: 1}, `Foo, [<1, 2>])$;
  let wide = select str::sprintf("<%*d>", 100000000000, 1) { error as e => "[e]", s => s };
  "[c] [wide]"
}"#,
    |v: Result<&Value>| matches!(
        v,
        Ok(Value::String(s)) if &**s == r#"(1, 2)|{x: 1}|`Foo|[<1, 2>] error:`FormatError("the result exceeds the 67108864 byte limit")"#
    );
    FuseExpect::Jit
);

// A bad argument names itself and its plain value (collections-str-09).
run!(
    sub_names_the_bad_argument,
    r#"{
  let r = str::sub(#start: -1, #len: 2, "héllo");
  let n = str::splitn(#pat: ",", #n: 0, "a,b");
  "[r] [n]"
}"#,
    |v: Result<&Value>| matches!(
        v,
        Ok(Value::String(s)) if &**s == r##"error:`SubError("#start must be non negative, got -1") error:`StringSplitError("#n must be > 0, got 0")"##
    );
    FuseExpect::Jit
);

// `str::parse` is a typed fast fn: the kernel casts the parsed value to
// the site's resolved return type, here a struct under `#[native]`.
const STR_PARSE_STRUCT_NATIVE: &str = r#"{
  let p = |s: string| -> {x: i64, y: string} { let r = #[native] str::parse(s)$; r };
  let {x, y} = p("\[\[\"x\", 41\], \[\"y\", \"z\"\]\]");
  (x + 1, y)
}"#;

run!(str_parse_struct_native, STR_PARSE_STRUCT_NATIVE, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => {
            matches!(&a[..], [Value::I64(42), Value::String(s)] if &**s == "z")
        }
        _ => false,
    }
});

// An escape that is the separator would escape every separator: refused.
run!(
    str_split_escaped_same_char_refused,
    r#"{
  let a = str::split_escaped(#esc: ",", #sep: ",", "a,b");
  let b = str::splitn_escaped(#n: 2, #esc: ",", #sep: ",", "a,b");
  "[a] [b]"
}"#,
    |v: Result<&Value>| {
        matches!(v, Ok(Value::String(s)) if s.matches("escape and separator must differ").count() == 2)
    }
);
