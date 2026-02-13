use crate::test::{REGISTER, ROOT};
use anyhow::Result;
use netidx::subscriber::Value;

const STR_STARTS_WITH: &str = r#"
  str::starts_with(#pfx:"foo", "foobarbaz")
"#;

graphix_package_core::run!(str_starts_with, STR_STARTS_WITH, |v: Result<&Value>| {
    match v {
        Ok(Value::Bool(true)) => true,
        _ => false,
    }
});

const STR_ENDS_WITH: &str = r#"
  str::ends_with(#sfx:"baz", "foobarbaz")
"#;

graphix_package_core::run!(str_ends_with, STR_ENDS_WITH, |v: Result<&Value>| {
    match v {
        Ok(Value::Bool(true)) => true,
        _ => false,
    }
});

const STR_CONTAINS: &str = r#"
  str::contains(#part:"bar", "foobarbaz")
"#;

graphix_package_core::run!(str_contains, STR_CONTAINS, |v: Result<&Value>| {
    match v {
        Ok(Value::Bool(true)) => true,
        _ => false,
    }
});

const STR_STRIP_PREFIX: &str = r#"
  str::strip_prefix(#pfx:"foo", "foobarbaz")
"#;

graphix_package_core::run!(str_strip_prefix, STR_STRIP_PREFIX, |v: Result<&Value>| {
    match v {
        Ok(Value::String(s)) => s == "barbaz",
        _ => false,
    }
});

const STR_STRIP_SUFFIX: &str = r#"
  str::strip_suffix(#sfx:"baz", "foobarbaz")
"#;

graphix_package_core::run!(str_strip_suffix, STR_STRIP_SUFFIX, |v: Result<&Value>| {
    match v {
        Ok(Value::String(s)) => s == "foobar",
        _ => false,
    }
});

const STR_TRIM: &str = r#"
  str::trim(" foobarbaz ")
"#;

graphix_package_core::run!(str_trim, STR_TRIM, |v: Result<&Value>| {
    match v {
        Ok(Value::String(s)) => s == "foobarbaz",
        _ => false,
    }
});

const STR_TRIM_START: &str = r#"
  str::trim_start(" foobarbaz ")
"#;

graphix_package_core::run!(str_trim_start, STR_TRIM_START, |v: Result<&Value>| {
    match v {
        Ok(Value::String(s)) => s == "foobarbaz ",
        _ => false,
    }
});

const STR_TRIM_END: &str = r#"
  str::trim_end(" foobarbaz ")
"#;

graphix_package_core::run!(str_trim_end, STR_TRIM_END, |v: Result<&Value>| {
    match v {
        Ok(Value::String(s)) => s == " foobarbaz",
        _ => false,
    }
});

const STR_REPLACE: &str = r#"
  str::replace(#pat:"foo", #rep:"baz", "foobarbazfoo")
"#;

graphix_package_core::run!(str_replace, STR_REPLACE, |v: Result<&Value>| {
    match v {
        Ok(Value::String(s)) => s == "bazbarbazbaz",
        _ => false,
    }
});

const STR_DIRNAME: &str = r#"
  str::dirname("/foo/bar/baz")
"#;

graphix_package_core::run!(str_dirname, STR_DIRNAME, |v: Result<&Value>| {
    match v {
        Ok(Value::String(s)) => s == "/foo/bar",
        _ => false,
    }
});

const STR_BASENAME: &str = r#"
  str::basename("/foo/bar/baz")
"#;

graphix_package_core::run!(str_basename, STR_BASENAME, |v: Result<&Value>| {
    match v {
        Ok(Value::String(s)) => s == "baz",
        _ => false,
    }
});

const STR_JOIN: &str = r#"
  str::join(#sep:"/", "/foo", "bar", ["baz", "zam"])
"#;

graphix_package_core::run!(str_join, STR_JOIN, |v: Result<&Value>| {
    match v {
        Ok(Value::String(s)) => s == "/foo/bar/baz/zam",
        _ => false,
    }
});

const STR_CONCAT: &str = r#"
  str::concat("foo", "bar", ["baz", "zam"])
"#;

graphix_package_core::run!(str_concat, STR_CONCAT, |v: Result<&Value>| {
    match v {
        Ok(Value::String(s)) => s == "foobarbazzam",
        _ => false,
    }
});

const STR_ESCAPE: &str = r#"
  str::escape("/foo/bar")
"#;

graphix_package_core::run!(str_escape, STR_ESCAPE, |v: Result<&Value>| {
    match v {
        Ok(Value::String(s)) => s == "\\/foo\\/bar",
        _ => false,
    }
});

const STR_UNESCAPE: &str = r#"
  str::unescape("\\/foo\\/bar")
"#;

graphix_package_core::run!(str_unescape, STR_UNESCAPE, |v: Result<&Value>| {
    match v {
        Ok(Value::String(s)) => s == "/foo/bar",
        _ => false,
    }
});

const STR_SPLIT: &str = r#"
{
  let a = str::split(#pat:",", "foo, bar, baz");
  array::map(a, |s| str::trim(s))
}
"#;

graphix_package_core::run!(str_split, STR_SPLIT, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::String(s0), Value::String(s1), Value::String(s2)] => {
                s0 == "foo" && s1 == "bar" && s2 == "baz"
            }
            _ => false,
        },
        _ => false,
    }
});

const STR_RSPLIT: &str = r#"
{
  let a = str::rsplit(#pat:",", "foo, bar, baz");
  array::map(a, |s| str::trim(s))
}
"#;

graphix_package_core::run!(str_rsplit, STR_RSPLIT, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::String(s0), Value::String(s1), Value::String(s2)] => {
                s0 == "baz" && s1 == "bar" && s2 == "foo"
            }
            _ => false,
        },
        _ => false,
    }
});

const STR_SPLITN: &str = r#"
{
  let a = str::splitn(#pat:",", #n:2, "foo, bar, baz")?;
  array::map(a, |s| str::trim(s))
}
"#;

graphix_package_core::run!(str_splitn, STR_SPLITN, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::String(s0), Value::String(s1)] => s0 == "foo" && s1 == "bar, baz",
            _ => false,
        },
        _ => false,
    }
});

const STR_RSPLITN: &str = r#"
{
  let a = str::rsplitn(#pat:",", #n:2, "foo, bar, baz")?;
  array::map(a, |s| str::trim(s))
}
"#;

graphix_package_core::run!(str_rsplitn, STR_RSPLITN, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::String(s0), Value::String(s1)] => s0 == "baz" && s1 == "foo, bar",
            _ => false,
        },
        _ => false,
    }
});

const STR_SPLIT_ESCAPED: &str = r#"
{
  let a = str::split_escaped(#esc:"\\", #sep:",", "foo\\, bar, baz")?;
  array::map(a, |s| str::trim(s))
}
"#;

graphix_package_core::run!(str_split_escaped, STR_SPLIT_ESCAPED, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::String(s0), Value::String(s1)] => s0 == "foo\\, bar" && s1 == "baz",
            _ => false,
        },
        _ => false,
    }
});

const STR_SPLITN_ESCAPED: &str = r#"
{
  let a = str::splitn_escaped(#n:2, #esc:"\\", #sep:",", "foo\\, bar, baz, bam")?;
  array::map(a, |s| str::trim(s))
}
"#;

graphix_package_core::run!(str_splitn_escaped, STR_SPLITN_ESCAPED, |v: Result<
    &Value,
>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::String(s0), Value::String(s1)] => {
                s0 == "foo\\, bar" && s1 == "baz, bam"
            }
            _ => false,
        },
        _ => false,
    }
});

const STR_SPLIT_ONCE: &str = r#"
  str::split_once(#pat:", ", "foo, bar, baz")
"#;

graphix_package_core::run!(str_split_once, STR_SPLIT_ONCE, |v: Result<&Value>| {
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

graphix_package_core::run!(str_rsplit_once, STR_RSPLIT_ONCE, |v: Result<&Value>| {
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

graphix_package_core::run!(str_to_lower, STR_TO_LOWER, |v: Result<&Value>| {
    match v {
        Ok(Value::String(s)) => s == "foo",
        _ => false,
    }
});

const STR_TO_UPPER: &str = r#"
  str::to_upper("foo")
"#;

graphix_package_core::run!(str_to_upper, STR_TO_UPPER, |v: Result<&Value>| {
    match v {
        Ok(Value::String(s)) => s == "FOO",
        _ => false,
    }
});

const STR_LEN: &str = r#"
  str::len("foo")
"#;

graphix_package_core::run!(str_len, STR_LEN, |v: Result<&Value>| {
    match v {
        Ok(Value::I64(3)) => true,
        _ => false,
    }
});

const STR_SUB: &str = r#"
  str::sub(#start:1, #len:2, "💗💖🍇")
"#;

graphix_package_core::run!(str_sub, STR_SUB, |v: Result<&Value>| {
    match v {
        Ok(Value::String(s)) if &*s == "💖🍇" => true,
        _ => false,
    }
});

const STR_PARSE: &str = r#"
  str::parse("42")
"#;

graphix_package_core::run!(str_parse, STR_PARSE, |v: Result<&Value>| {
    match v {
        Ok(Value::I64(42)) => true,
        _ => false,
    }
});
