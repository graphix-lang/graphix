use anyhow::Result;
use graphix_package_core::run;
use netidx::subscriber::Value;

const NET_PUB_SUB: &str = r#"
{
  net::publish("/local/foo", 42);
  net::subscribe("/local/foo")?
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
  net::publish(#on_write:|v| x <- cast<i64>(v)?, p, x);
  let s = cast<i64>(net::subscribe(p)?)?;
  net::write(p, once(s + 1));
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
  net::publish(#on_write:|v: string| x <- cast<i64>(v)?, p, x);
  let s = cast<i64>(net::subscribe(p)?)?;
  net::write(p, once(s + 1));
  array::group(s, |n, _| n == 2)
}
"#;

run!(net_write1, NET_WRITE1, |v: Result<&Value>| {
    match v {
        Ok(_) => false,
        Err(_) => true,
    }
});

const NET_LIST: &str = r#"
{
  net::publish("/local/foo", 42);
  net::publish("/local/bar", 42);
  net::list("/local")
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
  net::publish("/local/t/0/foo", 42);
  net::publish("/local/t/0/bar", 42);
  net::publish("/local/t/1/foo", 42);
  net::publish("/local/t/1/bar", 42);
  let t = dbg(net::list_table("/local/t"))?;
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
  net::rpc(
    #path:get_val,
    #doc:"get the value",
    #spec:[],
    #f:|a| a ~ v);
  net::rpc(
    #path:set_val,
    #doc:"set the value",
    #spec:[{name: "val", doc: "The value", default: null}],
    #f:|args| select cast<{val: Any}>(args) {
        error as e => e,
        { val } => {
          v <- val;
          val ~ null
        }
    });
  net::call(set_val, [("val", 42)]);
  net::call(get_val, [])
}
"#;

run!(net_rpc0, NET_RPC0, |v: Result<&Value>| {
    match v {
        Ok(Value::I64(42)) => true,
        _ => false,
    }
});
