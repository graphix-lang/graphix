# sys::fs - Filesystem Operations

The `sys::fs` module provides functions for reading, writing, and watching files and directories.

## Interface

```graphix
use sys::io;

type FileType = [
    `Dir,
    `File,
    `Symlink,
    `SymlinkDir,
    `BlockDev,
    `CharDev,
    `Fifo,
    `Socket,
    null
];

/// Filesystem metadata. Not all kind fields are possible on all platforms.
/// permissions will only be set on unix platforms, windows will only
/// expose the ReadOnly flag.
type Metadata = {
    accessed: [datetime, null],
    created: [datetime, null],
    modified: [datetime, null],
    kind: FileType,
    len: u64,
    permissions: [u32, `ReadOnly(bool)]
};

/// a directory entry
type DirEntry = {
    path: string,
    file_name: string,
    depth: i64,
    kind: FileType
};

type Mode = [`Read, `Write, `Append, `ReadWrite, `Create, `CreateNew];
type SeekFrom = [`Start(u64), `End(i64), `Current(i64)];

mod watch;
mod tempdir;

val read_all: fn(string) -> Result<string, `IOError(string)>;
val read_all_bin: fn(string) -> Result<bytes, `IOError(string)>;
val write_all: fn(#path: string, string) -> Result<null, `IOError(string)>;
val write_all_bin: fn(#path: string, bytes) -> Result<null, `IOError(string)>;
val is_file: fn(string) -> Result<string, `IOError(string)>;
val is_dir: fn(string) -> Result<string, `IOError(string)>;
val metadata: fn(?#follow_symlinks: bool, string) -> Result<Metadata, `IOError(string)>;

val readdir: fn(
    ?#max_depth: i64,
    ?#min_depth: i64,
    ?#contents_first: bool,
    ?#follow_symlinks: bool,
    ?#follow_root_symlink: bool,
    ?#same_filesystem: bool,
    string
) -> Result<Array<DirEntry>, `IOError(string)>;

val create_dir: fn(?#all: bool, string) -> Result<null, `IOError(string)>;
val remove_dir: fn(?#all: bool, string) -> Result<null, `IOError(string)>;
val remove_file: fn(string) -> Result<null, `IOError(string)>;

/// Open a file with the specified mode, returning an I/O stream.
///
/// Mode semantics:
/// - `Read: must exist, read only
/// - `Write: create or truncate, write only
/// - `Append: create or append, write only
/// - `ReadWrite: must exist, read and write
/// - `Create: create or truncate, read and write
/// - `CreateNew: must not exist, read and write
val open: fn(Mode, string) -> Result<io::Stream<`File>, `IOError(string)>;

/// Seek to a position in the file. Returns the new position.
val seek: fn(io::Stream<`File>, SeekFrom) -> Result<u64, `IOError(string)>;

/// Get metadata for the open file.
val fstat: fn(io::Stream<`File>) -> Result<Metadata, `IOError(string)>;

/// Truncate or extend the file to the specified length.
val truncate: fn(io::Stream<`File>, u64) -> Result<null, `IOError(string)>;
```

Once a file is opened with `sys::fs::open`, use `sys::io::read`,
`sys::io::write`, and `sys::io::flush` for I/O — these work on any
stream kind.

## sys::fs::watch

```graphix
type Interest = [
    `Established,
    `Any,
    `Access,
    `AccessOpen,
    `AccessClose,
    `AccessRead,
    `AccessOther,
    `Create,
    `CreateFile,
    `CreateFolder,
    `CreateOther,
    `Modify,
    `ModifyData,
    `ModifyDataSize,
    `ModifyDataContent,
    `ModifyDataOther,
    `ModifyMetadata,
    `ModifyMetadataAccessTime,
    `ModifyMetadataWriteTime,
    `ModifyMetadataPermissions,
    `ModifyMetadataOwnership,
    `ModifyMetadataExtended,
    `ModifyMetadataOther,
    `ModifyRename,
    `ModifyRenameTo,
    `ModifyRenameFrom,
    `ModifyRenameBoth,
    `ModifyRenameOther,
    `ModifyOther,
    `Delete,
    `DeleteFile,
    `DeleteFolder,
    `DeleteOther,
    `Other
];

type WatchEvent = {
    paths: Array<string>,
    event: Interest
};

type Watcher;
type Watch;

val create: fn(
    ?#poll_interval:[duration, null],
    ?#poll_batch_size:[i64, null],
    Any
) -> Result<Watcher, `WatchError(string)>;

val watch: fn(?#interest: Array<Interest>, Watcher, string)
    -> Result<Watch, `WatchError(string)>;

val path: fn(@args: [Watch, Array<Watch>, Map<'k, Watch>])
    -> Result<string, `WatchError(string)>;

val events: fn(@args: [Watch, Array<Watch>, Map<'k, Watch>])
    -> Result<WatchEvent, `WatchError(string)>;
```

## sys::fs::tempdir

```graphix
type T;

val path: fn(T) -> string;

val create: fn(
    ?#in:[null, string],
    ?#name:[null, `Prefix(string), `Suffix(string)],
    Any
) -> Result<T, `IOError(string)>;
```
