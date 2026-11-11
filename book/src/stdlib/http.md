# http - HTTP Client/Server

The `http` module provides HTTP client and server functionality.

## Interface

```graphix
{{#include ../../../stdlib/graphix-package-http/src/graphix/mod.gxi}}
```

## Handlers

`serve` calls `#handler` once for each request, each call an instance of
its own, and the request is answered by the reply's first event. Calling
the handler is not an event, only its argument is: a reply that does not
derive from the request never fires, and the client waits until it gives
up. Write `|req| req ~ reply`, and raise a constant error as
``error(req ~ `Bad)?``. A reply that reads a field of the request, or
selects on it, fires with it.

## http::rest

Convenience functions for JSON REST APIs with optional bearer token authentication.

```graphix
{{#include ../../../stdlib/graphix-package-http/src/graphix/rest.gxi}}
```
