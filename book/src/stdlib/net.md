# Net

```graphix
mod net: sig {
    type Table = { rows: Array<string>, columns: Array<(string, v64)> };
    type ArgSpec = { name: string, doc: string, default: Any };

    /// write the value to the specified path
    val write: fn(string, Any) -> Result<_, `WriteError(string)>;

    /// subscribe to the specified path
    val subscribe: fn(string) -> Result<Primitive, `SubscribeError(string)>;

    /// call the specified rpc
    val call: fn(string, Array<(string, Any)>) -> Result<Primitive, `RpcError(string)>;

    /// Publish an rpc. When the rpc is called f will be called with the arguments
    /// sent by the caller, and whatever f returns will be sent back to the caller.
    /// If f does not return, the caller will hang waiting for a reply.
    val rpc: fn(
        #path:string,
        #doc:string,
        #spec:Array<ArgSpec>,
        #f:fn(Array<(string, Any)>) -> Any throws 'e
    ) -> Result<_, `PublishRpcError(string)> throws 'e;

    /// list paths under the specified path. If #update is specified, then the list will
    /// be refreshed each time clock is triggered. If update is not specified, the list will
    /// be updated each second
    val list: fn(?#update:Any, string) -> Result<Array<string>, `ListError(string)>;

    /// list the table under the specified path. If #update is specified, then the table
    /// will be refreshed each time clock is triggered. If update is not specified, the table
    /// will be updated each second
    val list_table: fn(?#update:Any, string) -> Result<Table, `ListError(string)>;

    /// Publish the specified value at the specified path. Whenever the value updates,
    /// the new value will be sent to subscribers. If #on_write is specified, then if
    /// subscribers write to the value on_write will be called with the written value.
    /// on_write need not return anything.
    val publish: fn(?#on_write:fn(Any) -> _ throws 'e, string, Any) -> Result<_, `PublishError(string)> throws 'e;
}
```
