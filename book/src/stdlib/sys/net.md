# sys::net - Netidx Operations

The `sys::net` module provides publish/subscribe and RPC operations via
[netidx](https://netidx.github.io/netidx-book).

```graphix
type Table = { rows: Array<string>, columns: Array<string> };
type RpcArg<'a> = { default: 'a, doc: string };

/// write the value to the specified path
val write: fn(path: string, value: Any) -> Result<_, `WriteError(string)>;

/// subscribe to the specified path. The result type is driven by the
/// annotation at the binding site — the runtime converts the published
/// value into the requested type, returning `InvalidCast` if the
/// conversion fails.
val subscribe: fn(path: string) -> Result<'a, [`SubscribeError(string), `InvalidCast(string)]>;

/// call the specified rpc. args must be a struct or null. The result
/// type is driven by the annotation at the binding site.
val call: fn(path: string, args: 'a) -> Result<'b, [`RpcError(string), `InvalidCast(string)]>;

/// Publish an rpc.
/// - spec ('spec) must be a struct where every field is a RpcArg, or null (no arguments)
/// - the argument to f ('args) must be a struct with the same fields as 'spec,
///   or null if 'spec is null
/// - every field in 'args must contain the type of the corresponding default in 'spec
val rpc: fn(
    #path:string,
    #doc:string,
    #spec:'spec,
    #f:fn(args: 'args) -> 'result throws 'e
) -> Result<_, `PublishRpcError(string)> throws 'e;

/// list paths under the specified path. If #update is specified the
/// list refreshes each time the trigger updates; otherwise it refreshes
/// once per second.
val list: fn(?#update:Any, path: string) -> Result<Array<string>, `ListError(string)>;

/// list the table under the specified path. Refresh semantics match `list`.
val list_table: fn(?#update:Any, path: string) -> Result<Table, `ListError(string)>;

/// Publish the specified value at the specified path. Whenever the value
/// updates, the new value is sent to subscribers. If #on_write is specified,
/// writes from subscribers invoke on_write with the written value.
val publish: fn(?#on_write:fn(v: 'a) -> _ throws 'e, path: string, v: Any) -> Result<_, `PublishError(string)> throws 'e;
```
