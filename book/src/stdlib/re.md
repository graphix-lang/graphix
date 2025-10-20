# Re

```
/// return true if the string is matched by #pat, otherwise return false.
/// return an error if #pat is invalid.
val is_match: fn(#pat:string, string) -> Result<bool, `ReError(string)>;

/// return an array of instances of #pat in s. return an error if #pat is
/// invalid.
val find: fn(#pat:string, string) -> Result<Array<string>, `ReError(string)>;

/// return an array of captures matched by #pat. The array will have an element for each
/// capture, regardless of whether it matched or not. If it did not match the corresponding
/// element will be null. Return an error if #pat is invalid.
val captures: fn(#pat:string, string) -> Result<Array<Array<Option<string>>>, `ReError(string)>;

/// return an array of strings split by #pat. return an error if #pat is invalid.
val split: fn(#pat:string, string) -> Result<Array<string>, `ReError(string)>;

/// split the string by #pat at most #limit times and return an array of the parts.
/// return an error if #pat is invalid
val splitn: fn(#pat:string, #limit:i64, string) -> Result<Array<string>, `ReError(string)>;
```
