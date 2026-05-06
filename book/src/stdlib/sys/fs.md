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

/// Read the specified file into memory as a utf8 string and return it.
val read_all: fn(path: string) -> Result<string, `IOError(string)>;

/// Read the specified file into memory as bytes and return it.
val read_all_bin: fn(path: string) -> Result<bytes, `IOError(string)>;

/// Write data to path. If path does not exist it is created. If path exists it
/// is truncated and replaced with data.
val write_all: fn(#path: string, data: string) -> Result<null, `IOError(string)>;

/// Like write_all, but for binary data.
val write_all_bin: fn(#path: string, data: bytes) -> Result<null, `IOError(string)>;

/// If path is a regular file then return path, otherwise return an IOError.
val is_file: fn(path: string) -> Result<string, `IOError(string)>;

/// If path is a directory then return path, otherwise return an IOError.
val is_dir: fn(path: string) -> Result<string, `IOError(string)>;

/// Return metadata for a filesystem object.
val metadata: fn(?#follow_symlinks: bool, path: string) -> Result<Metadata, `IOError(string)>;

/// Read a directory and return an array of directory entries.
val readdir: fn(
    ?#max_depth: i64,
    ?#min_depth: i64,
    ?#contents_first: bool,
    ?#follow_symlinks: bool,
    ?#follow_root_symlink: bool,
    ?#same_filesystem: bool,
    path: string
) -> Result<Array<DirEntry>, `IOError(string)>;

/// Create a directory. If `all` is true (default false) create all intermediate
/// directories as well.
val create_dir: fn(?#all: bool, path: string) -> Result<null, `IOError(string)>;

/// Remove a directory. If `all` is true (default false) recursively remove
/// the contents as well.
val remove_dir: fn(?#all: bool, path: string) -> Result<null, `IOError(string)>;

/// Remove a file.
val remove_file: fn(path: string) -> Result<null, `IOError(string)>;

/// Open a file with the specified mode, returning an I/O stream.
///
/// Mode semantics:
/// - `Read: must exist, read only
/// - `Write: create or truncate, write only
/// - `Append: create or append, write only
/// - `ReadWrite: must exist, read and write
/// - `Create: create or truncate, read and write
/// - `CreateNew: must not exist, read and write
val open: fn(mode: Mode, path: string) -> Result<io::Stream<`File>, `IOError(string)>;

/// Seek to a position in the file. Returns the new position.
val seek: fn(stream: io::Stream<`File>, pos: SeekFrom) -> Result<u64, `IOError(string)>;

/// Get metadata for the open file.
val fstat: fn(stream: io::Stream<`File>) -> Result<Metadata, `IOError(string)>;

/// Truncate or extend the file to the specified length.
val truncate: fn(stream: io::Stream<`File>, len: u64) -> Result<null, `IOError(string)>;
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

/// Create a filesystem watcher.
/// poll_interval defaults to 1s; poll_batch_size defaults to 100.
val create: fn(
    ?#poll_interval:[duration, null],
    ?#poll_batch_size:[i64, null],
    trigger: Any
) -> Result<Watcher, `WatchError(string)>;

/// Watch path for events matching #interest using the given watcher.
val watch: fn(?#interest: Array<Interest>, watcher: Watcher, path: string)
    -> Result<Watch, `WatchError(string)>;

/// The path of the filesystem object involved in the most recent watch event.
/// Accepts a single Watch, an Array of Watches, or a Map with Watch values.
val path: fn(@args: [Watch, Array<Watch>, Map<'k, Watch>])
    -> Result<string, `WatchError(string)>;

/// The full watch event including all paths and the event type.
val events: fn(@args: [Watch, Array<Watch>, Map<'k, Watch>])
    -> Result<WatchEvent, `WatchError(string)>;
```

## sys::fs::tempdir

```graphix
/// An opaque handle to a temporary directory. The directory is
/// automatically deleted when the handle is dropped.
type T;

/// Get the filesystem path of a TempDir.
val path: fn(td: T) -> string;

/// Create a temporary directory.
/// - #in: parent directory (default: system temp dir)
/// - #name: prefix or suffix for the directory name
val create: fn(
    ?#in:[null, string],
    ?#name:[null, `Prefix(string), `Suffix(string)],
    trigger: Any
) -> Result<T, `IOError(string)>;
```
