use anyhow::Result;
use graphix_package_core::run;
use netidx::subscriber::Value;

// The re builtins are fast fns over one thread-local pattern memo
// (`FastMemo`), so every fixture here fuses.

const RE_IS_MATCH: &str = r#"
  re::is_match(#pat:r"[\[\]0-9]+", r"foo[0]")
"#;

run!(re_is_match, RE_IS_MATCH, |v: Result<&Value>| {
    match v {
        Ok(Value::Bool(true)) => true,
        _ => false,
    }
}; graphix_package_core::testing::FuseExpect::Jit);

const RE_FIND: &str = r#"
  re::find(#pat:r"foo", r"foobarfoobazfoo")
"#;

run!(re_find, RE_FIND, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::String(s0), Value::String(s1), Value::String(s2)] => {
                s0 == "foo" && s0 == s1 && s0 == s2
            }
            _ => false,
        },
        _ => false,
    }
}; graphix_package_core::testing::FuseExpect::Jit);

const RE_CAPTURES: &str = r#"
  re::captures(#pat:r"(fo)ob", r"foobarfoobazfoo")
"#;

run!(re_captures, RE_CAPTURES, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::Array(a0), Value::Array(a1)] => match (&a0[..], &a1[..]) {
                (
                    [Value::String(c00), Value::String(c01)],
                    [Value::String(c10), Value::String(c11)],
                ) => c00 == "foob" && c01 == "fo" && c10 == "foob" && c11 == "fo",
                _ => false,
            },
            _ => false,
        },
        _ => false,
    }
}; graphix_package_core::testing::FuseExpect::Jit);

const RE_SPLIT: &str = r#"
  re::split(#pat:r",\s*", r"foo, bar, baz")
"#;

run!(re_split, RE_SPLIT, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::String(s0), Value::String(s1), Value::String(s2)] => {
                s0 == "foo" && s1 == "bar" && s2 == "baz"
            }
            _ => false,
        },
        _ => false,
    }
}; graphix_package_core::testing::FuseExpect::Jit);

const RE_SPLITN: &str = r#"
  re::splitn(#pat:r",\s*", #limit:2, r"foo, bar, baz")
"#;

run!(re_splitn, RE_SPLITN, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::String(s0), Value::String(s1)] => s0 == "foo" && s1 == "bar, baz",
            _ => false,
        },
        _ => false,
    }
}; graphix_package_core::testing::FuseExpect::Jit);

// One memo serves every site: a site whose pattern VARIES per call
// recompiles on the miss and never reads a stale regex.
const RE_PATTERN_VARIES: &str = r#"{
  let m = |p: string| { let r = #[native] re::find(#pat: p, "aabbcc")$; r };
  [m("a+"), m("b+"), m("c+"), m("a+")]
}"#;

run!(re_pattern_varies, RE_PATTERN_VARIES, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => {
            a.len() == 4
                && a.iter().zip(["aa", "bb", "cc", "aa"]).all(|(v, e)| {
                    matches!(v, Value::Array(m)
                        if m.len() == 1 && matches!(&m[0], Value::String(s) if &**s == e))
                })
        }
        _ => false,
    }
});

const RE_INVALID_PATTERN: &str = r#"{
  let m = |p: string| { let r = #[native] re::is_match(#pat: p, "x"); r };
  m("(")
}"#;

run!(re_invalid_pattern, RE_INVALID_PATTERN, |v: Result<&Value>| {
    matches!(v, Ok(Value::Error(_)))
});
