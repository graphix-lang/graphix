# http - HTTP Client/Server

The `http` module provides HTTP client and server functionality.

## Interface

```graphix
type Method = [`GET, `POST, `PUT, `DELETE, `PATCH, `HEAD, `OPTIONS];

type Response = {
    body: string,
    headers: Array<(string, string)>,
    status: u16,
    url: string
};

type BinResponse = {
    body: bytes,
    headers: Array<(string, string)>,
    status: u16,
    url: string
};

type Request = {
    body: [string, null],
    headers: Array<(string, string)>,
    method: string,
    path: string,
    query: [string, null]
};

type Client;
type Server;

/// Create a configured HTTP client.
val client: fn(
    ?#timeout: [duration, null],
    ?#default_headers: Array<(string, string)>,
    ?#redirect_limit: u32,
    ?#ca_cert: [bytes, null],
    trigger: Any
) -> Result<Client, `HTTPError(string)>;

/// Get or create a shared default HTTP client.
val default_client: fn(trigger: Any) -> Result<Client, `HTTPError(string)>;

/// Make an HTTP request and return a text response.
val request: fn(
    ?#method: Method,
    ?#headers: Array<(string, string)>,
    ?#body: [string, null],
    ?#timeout: [duration, null],
    client: Client,
    url: string
) -> Result<Response, `HTTPError(string)>;

/// Make an HTTP request and return a binary response.
val request_bin: fn(
    ?#method: Method,
    ?#headers: Array<(string, string)>,
    ?#body: [bytes, null],
    ?#timeout: [duration, null],
    client: Client,
    url: string
) -> Result<BinResponse, `HTTPError(string)>;

/// Convenience: GET request with text response.
val get: fn(client: Client, url: string) -> Result<Response, `HTTPError(string)>;

/// Convenience: GET request with binary response.
val get_bin: fn(client: Client, url: string) -> Result<BinResponse, `HTTPError(string)>;

/// Return the bound address of a running server.
val server_addr: fn(server: Server) -> string;

/// Start an HTTP server.
val serve: fn(
    #addr: string,
    ?#cert: [bytes, null],
    ?#key: [bytes, null],
    ?#max_connections: i64,
    #handler: fn(req: Request) -> Response throws 'e
) -> Result<Server, `HTTPError(string)> throws 'e;
```

## http::rest

Convenience functions for JSON REST APIs with optional bearer token authentication.

```graphix
val get: fn(
    ?#bearer: [string, null],
    ?#headers: Array<(string, string)>,
    client: Client,
    url: string
) -> Result<Response, `HTTPError(string)>;

val post: fn(
    ?#bearer: [string, null],
    ?#headers: Array<(string, string)>,
    #body: string,
    client: Client,
    url: string
) -> Result<Response, `HTTPError(string)>;

val put: fn(
    ?#bearer: [string, null],
    ?#headers: Array<(string, string)>,
    #body: string,
    client: Client,
    url: string
) -> Result<Response, `HTTPError(string)>;

val delete: fn(
    ?#bearer: [string, null],
    ?#headers: Array<(string, string)>,
    client: Client,
    url: string
) -> Result<Response, `HTTPError(string)>;

val patch: fn(
    ?#bearer: [string, null],
    ?#headers: Array<(string, string)>,
    #body: string,
    client: Client,
    url: string
) -> Result<Response, `HTTPError(string)>;
```
