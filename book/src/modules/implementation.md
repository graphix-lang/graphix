# Implementation Files

Module implementations are stored in `.gx` files. A module may be defined in
either a file or by a value in netidx.

For files, the file name must end in `.gx` and the part before that is the name
of the module. For example a file `m.gx` contains the expressions defining the
module `m`. The name of the module is taken from the filename.

Modules may optionally have an interface file (`.gxi`) that defines their public
API. See [Interface Files](./interfaces.md) for details.

In netidx, the naming convention is the same as for files: implementations end
in `.gx` and interfaces end in `.gxi`. For example, to publish a module named
`strops`, you would publish the implementation at `/libs/graphix/strops.gx` and
optionally the interface at `/libs/graphix/strops.gxi`.

Here is a simple example,

```
$ ls
m.gx  test.gx
```

`test.gx` is the program that we will run, `m.gx` is a module it will load.

`test.gx`
```graphix
mod m;

m::hello
```

`m.gx`
```graphix
let hello = "hello world"
```

running this we get,

```
$ graphix test.gx
"hello world"
```

## Module Load Path

The graphix shell reads the `GRAPHIX_MODPATH` environment variable at startup
and appends it's contents to the built in list of module paths. The syntax is a
comma separated list of paths. Paths that start with `netidx:` are netidx paths,
otherwise file paths are expected. The comma separator can be escaped with `\`.
For example,

```
GRAPHIX_MODPATH=netidx:/foo,/home/user/graphix-modules,/very/str\,ange/path
```

would add
- netidx:/foo
- /home/user/graphix-modules
- /very/str,ange/path

to the Graphix module path

### Default Module Path

By default the module resolve path has several entries,
- the parent directory of the program file passed on the command line. e.g. if
  we are running `/home/user/test.gx` then Graphix will look for modules in
  `/home/user`

- the Graphix init directory. This is a platform specific directory where you
  can put Graphix modules.
  - On Linux `~/.local/share/graphix`
  - On Windows `%APPDATA%\Roaming\graphix`
  - On Mac OS `~/Library/Application Support/graphix`

In REPL mode, which is when it's given no argument, the `graphix` command will
try to load the module `init`. If no such module exists it will silently carry
on. You can use this to load commonly used utilities in the repl automatically.

## Modules in Netidx

We can publish the same code as the files example in netidx and use it in
Graphix directly. First lets publish it,

```
$ printf \
  "/local/graphix/test.gx|string|%s\n/local/graphix/m.gx|string|%s" \
  "$(tr \n ' ' <test.gx)" "$(tr \n ' ' <m.gx)" \
  | netidx publisher
```

Graphix doesn't care about whitespaces like newline, so we can just translate
them to spaces to avoid confusing the command line publisher. Lets see if we
published successfully.

```
$ netidx subscriber /local/graphix/test.gx
/local/graphix/test.gx|string|"mod m;  m::hello"
```

Looks good, now lets run the code. We need to add to the resolve path to tell
the Graphix shell where it should look for modules.

```
$ GRAPHIX_MODPATH=netidx:/local/graphix graphix test
"hello world"
```

## Module Hierarchies

Module hierarchies can be created using directories. To create `m::n` you would
create a directory `m` and in it a file called `mod.gx` and a file called `n.gx`.
Optionally, you can add interface files (`mod.gxi`, `n.gxi`) to define the public
API of each module.

```
$ find .
.
./m
./m/mod.gx
./m/mod.gxi   # optional interface for module m
./m/n.gx
./m/n.gxi     # optional interface for module n
./test.gx
```

`test.gx` is the root of the hierarchy
```graphix
mod m;

m::n::hello
```

`m/mod.gx` is the root of module `m`
```graphix
mod n
```

`m/n.gx` is the `m::n` module
```graphix
let hello = "hello world"
```

if we run the program we get,

```
$ graphix test.gx
"hello world"
```

## Module Hierarchies in Netidx

Module hierarchies in netidx work the same as in the file system. To replicate
the above example we'd publish,

```
/lib/graphix/test.gx     <- the contents of test.gx
/lib/graphix/m/mod.gx    <- the contents of m/mod.gx
/lib/graphix/m/mod.gxi   <- the contents of m/mod.gxi (optional)
/lib/graphix/m/n.gx      <- the contents of m/n.gx
/lib/graphix/m/n.gxi     <- the contents of m/n.gxi (optional)
```
