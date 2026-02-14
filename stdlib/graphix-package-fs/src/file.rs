use arcstr::ArcStr;
use bytes::Bytes;
use graphix_compiler::errf;
use graphix_package_core::{deftype, CachedArgsAsync, CachedVals, EvalCachedAsync};
use netidx_value::Value;
use std::fmt::Debug;

#[derive(Debug, Default)]
pub(crate) struct ReadAllOp;

impl EvalCachedAsync for ReadAllOp {
    const NAME: &str = "fs_read_all";
    deftype!("fn(string) -> Result<string, `IOError(string)>");
    type Args = ArcStr;

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        cached.get::<ArcStr>(0)
    }

    fn eval(path: Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            match tokio::fs::read_to_string(&*path).await {
                Ok(s) => Value::from(s),
                Err(e) => errf!("IOError", "could not read {path}, {e:?}"),
            }
        }
    }
}

pub(crate) type ReadAll = CachedArgsAsync<ReadAllOp>;

#[derive(Debug, Default)]
pub(crate) struct ReadAllBinOp;

impl EvalCachedAsync for ReadAllBinOp {
    const NAME: &str = "fs_read_all_bin";
    deftype!("fn(string) -> Result<bytes, `IOError(string)>");
    type Args = ArcStr;

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        cached.get::<ArcStr>(0)
    }

    fn eval(path: Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            match tokio::fs::read(&*path).await {
                Ok(s) => Value::from(Bytes::from(s)),
                Err(e) => errf!("IOError", "could not read {path}, {e:?}"),
            }
        }
    }
}

pub(crate) type ReadAllBin = CachedArgsAsync<ReadAllBinOp>;

#[derive(Debug, Default)]
pub(crate) struct WriteAllOp;

impl EvalCachedAsync for WriteAllOp {
    const NAME: &str = "fs_write_all";
    deftype!("fn(#path:string, string) -> Result<null, `IOError(string)>");
    type Args = (ArcStr, ArcStr);

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        Some((cached.get::<ArcStr>(0)?, cached.get::<ArcStr>(1)?))
    }

    fn eval((path, value): Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            match tokio::fs::write(&*path, &*value).await {
                Ok(()) => Value::Null,
                Err(e) => errf!("IOError", "could not write {path}, {e:?}"),
            }
        }
    }
}

pub(crate) type WriteAll = CachedArgsAsync<WriteAllOp>;

#[derive(Debug, Default)]
pub(crate) struct WriteAllBinOp;

impl EvalCachedAsync for WriteAllBinOp {
    const NAME: &str = "fs_write_all_bin";
    deftype!("fn(#path:string, bytes) -> Result<null, `IOError(string)>");
    type Args = (ArcStr, Bytes);

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        Some((cached.get::<ArcStr>(0)?, cached.get::<Bytes>(1)?))
    }

    fn eval((path, value): Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            match tokio::fs::write(&*path, &*value).await {
                Ok(()) => Value::Null,
                Err(e) => errf!("IOError", "could not write {path}, {e:?}"),
            }
        }
    }
}

pub(crate) type WriteAllBin = CachedArgsAsync<WriteAllBinOp>;

#[derive(Debug, Default)]
pub(crate) struct RemoveFileOp;

impl EvalCachedAsync for RemoveFileOp {
    const NAME: &str = "fs_remove_file";
    deftype!("fn(string) -> Result<null, `IOError(string)>");
    type Args = ArcStr;

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        cached.get::<ArcStr>(0)
    }

    fn eval(path: Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            match tokio::fs::remove_file(&*path).await {
                Ok(()) => Value::Null,
                Err(e) => errf!("IOError", "could not remove file {path}, {e:?}"),
            }
        }
    }
}

pub(crate) type RemoveFile = CachedArgsAsync<RemoveFileOp>;
