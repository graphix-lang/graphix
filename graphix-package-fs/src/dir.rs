use super::{convert_path, metadata::convert_filetype};
use anyhow::Result;
use arcstr::{literal, ArcStr};
use graphix_compiler::errf;
use graphix_package_core::{deftype, CachedArgsAsync, CachedVals, EvalCachedAsync};
use netidx_value::{ValArray, Value};
use poolshark::local::LPooled;
use std::result;
use walkdir::{DirEntry, WalkDir};

#[derive(Debug)]
pub(crate) struct ReadDirArgs {
    path: ArcStr,
    max_depth: usize,
    min_depth: usize,
    contents_first: bool,
    follow_symlinks: bool,
    follow_root_symlink: bool,
    same_filesystem: bool,
}

fn blocking_walkdir(args: ReadDirArgs) -> Result<LPooled<Vec<DirEntry>>> {
    let ReadDirArgs {
        path,
        max_depth,
        min_depth,
        contents_first,
        follow_symlinks,
        follow_root_symlink,
        same_filesystem,
    } = args;
    let rd = WalkDir::new(&*path)
        .max_depth(max_depth)
        .min_depth(min_depth)
        .contents_first(contents_first)
        .follow_links(follow_symlinks)
        .follow_root_links(follow_root_symlink)
        .same_file_system(same_filesystem);
    rd.into_iter().map(|r| r.map_err(anyhow::Error::from)).collect()
}

#[derive(Debug, Default)]
pub(crate) struct ReadDirEv;

impl EvalCachedAsync for ReadDirEv {
    const NAME: &str = "fs_readdir";
    deftype!(
        r#"fn(
            ?#max_depth:i64,
            ?#min_depth:i64,
            ?#contents_first:bool,
            ?#follow_symlinks:bool,
            ?#follow_root_symlink:bool,
            ?#same_filesystem:bool,
            string
        ) -> Result<Array<DirEntry>, `IOError(string)>"#
    );
    type Args = result::Result<ReadDirArgs, ArcStr>;

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        let max_depth = cached.get::<i64>(0)?;
        let min_depth = cached.get::<i64>(1)?;
        let contents_first = cached.get::<bool>(2)?;
        let follow_symlinks = cached.get::<bool>(3)?;
        let follow_root_symlink = cached.get::<bool>(4)?;
        let same_filesystem = cached.get::<bool>(5)?;
        let path = cached.get::<ArcStr>(6)?;
        if max_depth < 0 || min_depth < 0 || max_depth < min_depth {
            Some(Err(literal!(
                "max_depth and min_depth must be non negative and max_depth >= min_depth"
            )))
        } else {
            Some(Ok(ReadDirArgs {
                max_depth: max_depth as usize,
                min_depth: min_depth as usize,
                contents_first,
                follow_symlinks,
                follow_root_symlink,
                same_filesystem,
                path,
            }))
        }
    }

    fn eval(args: Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            let args = match args {
                Ok(args) => args,
                Err(s) => return errf!("IOError", "{s}"),
            };
            match tokio::task::spawn_blocking(|| blocking_walkdir(args)).await {
                Err(e) => errf!("IOError", "failed to spawn task {e:?}"),
                Ok(Err(e)) => errf!("IOError", "walkdir failed {e:?}"),
                Ok(Ok(mut ents)) => {
                    let ents = ents.drain(..).map(|ent| {
                        let file_name: Value = Value::String(ArcStr::from(
                            &*ent.file_name().to_string_lossy(),
                        ));
                        let depth: Value = (ent.depth() as i64).into();
                        let kind = convert_filetype(ent.file_type());
                        let path: Value = convert_path(ent.path()).into();
                        Value::from([
                            (literal!("depth"), depth),
                            (literal!("file_name"), file_name),
                            (literal!("kind"), kind),
                            (literal!("path"), path),
                        ])
                    });
                    Value::Array(ValArray::from_iter_exact(ents))
                }
            }
        }
    }
}

pub(crate) type ReadDir = CachedArgsAsync<ReadDirEv>;

#[derive(Debug, Default)]
pub(crate) struct CreateDirOp;

impl EvalCachedAsync for CreateDirOp {
    const NAME: &str = "fs_create_dir";
    deftype!("fn(?#all:bool, string) -> Result<null, `IOError(string)>");
    type Args = (bool, ArcStr);

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        Some((cached.get::<bool>(0)?, cached.get::<ArcStr>(1)?))
    }

    fn eval((all, path): Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            let result = if all {
                tokio::fs::create_dir_all(&*path).await
            } else {
                tokio::fs::create_dir(&*path).await
            };
            match result {
                Ok(()) => Value::Null,
                Err(e) => errf!("IOError", "could not create directory {path}, {e:?}"),
            }
        }
    }
}

pub(crate) type CreateDir = CachedArgsAsync<CreateDirOp>;

#[derive(Debug, Default)]
pub(crate) struct RemoveDirOp;

impl EvalCachedAsync for RemoveDirOp {
    const NAME: &str = "fs_remove_dir";
    deftype!("fn(?#all:bool, string) -> Result<null, `IOError(string)>");
    type Args = (bool, ArcStr);

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        Some((cached.get::<bool>(0)?, cached.get::<ArcStr>(1)?))
    }

    fn eval((all, path): Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            let result = if all {
                tokio::fs::remove_dir_all(&*path).await
            } else {
                tokio::fs::remove_dir(&*path).await
            };
            match result {
                Ok(()) => Value::Null,
                Err(e) => errf!("IOError", "could not remove directory {path}, {e:?}"),
            }
        }
    }
}

pub(crate) type RemoveDir = CachedArgsAsync<RemoveDirOp>;
