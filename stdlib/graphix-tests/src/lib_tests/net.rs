use anyhow::Result;
use graphix_package_core::run;
use netidx::subscriber::Value;

const NET_PUB_SUB: &str = r#"
{
  sys::net::publish("/local/foo", 42);
  let v: i64 = sys::net::subscribe("/local/foo")?;
  v
}
"#;

run!(net_pub_sub, NET_PUB_SUB, |v: Result<&Value>| {
    match v {
        Ok(Value::I64(42)) => true,
        _ => false,
    }
});

const NET_WRITE0: &str = r#"
{
  let p = "/local/foo";
  let x = 42;
  sys::net::publish(#on_write:|v| x <- cast<i64>(v)?, p, x);
  let s: i64 = sys::net::subscribe(p)?;
  sys::net::write(p, once(s + 1));
  array::group(s, |n, _| n == 2)
}
"#;

run!(net_write0, NET_WRITE0, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::I64(42), Value::I64(43)] => true,
            _ => false,
        },
        _ => false,
    }
});

const NET_WRITE1: &str = r#"
{
  let p = "/local/foo";
  let x = 42;
  sys::net::publish(#on_write:|v: string| x <- cast<i64>(v)?, p, x);
  let s: i64 = sys::net::subscribe(p)?;
  sys::net::write(p, once(s + 1));
  array::group(s, |n, _| n == 2)
}
"#;

run!(net_write1, NET_WRITE1, |v: Result<&Value>| {
    // with type-aware casting, the i64 write gets cast to string
    // and then cast<i64> in the callback converts it back successfully
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::I64(42), Value::I64(43)] => true,
            _ => false,
        },
        _ => false,
    }
});

const NET_LIST: &str = r#"
{
  sys::net::publish("/local/foo", 42);
  sys::net::publish("/local/bar", 42);
  sys::net::list("/local")
}
"#;

run!(net_list, NET_LIST, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => match &a[..] {
            [Value::String(s0), Value::String(s1)] => {
                let mut a = [s0, s1];
                a.sort();
                a[0] == "/local/bar" && a[1] == "/local/foo"
            }
            _ => false,
        },
        _ => false,
    }
});

const NET_LIST_TABLE: &str = r#"
{
  sys::net::publish("/local/t/0/foo", 42);
  sys::net::publish("/local/t/0/bar", 42);
  sys::net::publish("/local/t/1/foo", 42);
  sys::net::publish("/local/t/1/bar", 42);
  let t = dbg(sys::net::list_table("/local/t"))?;
  let cols = array::map(t.columns, |(n, _)| n);
  (array::sort(cols) == ["bar", "foo"])
  && (array::sort(t.rows) == ["/local/t/0", "/local/t/1"])
}
"#;

run!(net_list_table, NET_LIST_TABLE, |v: Result<&Value>| {
    match v {
        Ok(Value::Bool(true)) => true,
        _ => false,
    }
});

const NET_RPC0: &str = r#"
{
  let get_val = "/local/get_val";
  let set_val = "/local/set_val";
  let v: Any = never();
  sys::net::rpc(
    #path:get_val,
    #doc:"get the value",
    #spec:null,
    #f:|a: null| a ~ v);
  sys::net::rpc(
    #path:set_val,
    #doc:"set the value",
    #spec:{val: {default: null, doc: "The value"}},
    #f:|args: {val: Any}| {
      v <- args.val;
      args.val ~ null
    });
  let r: i64 = sys::net::call(set_val, {val: 42})?;
  let r2: i64 = sys::net::call(get_val, null)?;
  r2
}
"#;

run!(net_rpc0, NET_RPC0, |v: Result<&Value>| {
    match v {
        Ok(Value::I64(42)) => true,
        _ => false,
    }
});
