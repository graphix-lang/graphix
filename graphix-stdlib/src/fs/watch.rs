use crate::deftype;
use anyhow::{anyhow, bail};
use arcstr::{literal, ArcStr};
use compact_str::format_compact;
use enumflags2::{bitflags, BitFlags};
use futures::{channel::mpsc, future::join_all, SinkExt, StreamExt};
use fxhash::{FxHashMap, FxHashSet};
use graphix_compiler::{
    errf, expr::ExprId, Apply, BindId, BuiltIn, BuiltInInitFn, CustomBuiltinType, Event,
    ExecCtx, LibState, Node, Rt, UserEvent, CBATCH_POOL,
};
use netidx::utils::Either;
use netidx_value::{FromValue, ValArray, Value};
use notify::{
    event::{
        AccessKind, CreateKind, DataChange, MetadataKind, ModifyKind, RemoveKind,
        RenameMode,
    },
    EventKind, RecommendedWatcher, RecursiveMode, Watcher,
};
use poolshark::{
    global::{GPooled, Pool},
    local::LPooled,
};
use std::{
    any::Any,
    collections::{hash_set, VecDeque},
    ffi::OsString,
    path::{Path, PathBuf},
    result::Result,
    sync::{Arc, LazyLock},
    time::Duration,
};
use tokio::{fs, select, sync::mpsc as tmpsc, task};

static PATHS: LazyLock<Pool<FxHashSet<ArcStr>>> = LazyLock::new(|| Pool::new(1000, 1000));
const MAX_NOTIFY_BATCH: usize = 10_000;
const POLL_BATCH: usize = 100;
const POLL_TIMEOUT: Duration = Duration::from_millis(250);
const DEFAULT_POLL_INTERVAL: Duration = Duration::from_secs(1);

#[derive(Debug, Clone, Copy)]
#[bitflags]
#[repr(u64)]
enum Interest {
    Established,
    Any,
    Access,
    AccessOpen,
    AccessClose,
    AccessRead,
    AccessOther,
    Create,
    CreateFile,
    CreateFolder,
    CreateOther,
    Modify,
    ModifyData,
    ModifyDataSize,
    ModifyDataContent,
    ModifyDataOther,
    ModifyMetadata,
    ModifyMetadataAccessTime,
    ModifyMetadataWriteTime,
    ModifyMetadataPermissions,
    ModifyMetadataOwnership,
    ModifyMetadataExtended,
    ModifyMetadataOther,
    ModifyRename,
    ModifyRenameTo,
    ModifyRenameFrom,
    ModifyRenameBoth,
    ModifyRenameOther,
    ModifyOther,
    Delete,
    DeleteFile,
    DeleteFolder,
    DeleteOther,
    Other,
}

macro_rules! impl_value_conv {
    ($enum:ident { $($variant:ident),* $(,)? }) => {
        impl FromValue for $enum {
            fn from_value(v: Value) -> anyhow::Result<Self> {
                match v {
                    Value::String(s) => match &*s {
                        $(stringify!($variant) => Ok(Self::$variant),)*
                        _ => Err(anyhow::anyhow!("Invalid {} variant: {}", stringify!($enum), s)),
                    },
                    _ => Err(anyhow::anyhow!("Expected string value for {}, got: {:?}", stringify!($enum), v)),
                }
            }
        }

        impl Into<Value> for $enum {
            fn into(self) -> Value {
                match self {
                    $(Self::$variant => Value::String(literal!(stringify!($variant))),)*
                }
            }
        }
    };
}

impl_value_conv!(Interest {
    Established,
    Any,
    Access,
    AccessOpen,
    AccessClose,
    AccessRead,
    AccessOther,
    Create,
    CreateFile,
    CreateFolder,
    CreateOther,
    Modify,
    ModifyData,
    ModifyDataSize,
    ModifyDataContent,
    ModifyDataOther,
    ModifyMetadata,
    ModifyMetadataAccessTime,
    ModifyMetadataWriteTime,
    ModifyMetadataPermissions,
    ModifyMetadataOwnership,
    ModifyMetadataExtended,
    ModifyMetadataOther,
    ModifyRename,
    ModifyRenameTo,
    ModifyRenameFrom,
    ModifyRenameBoth,
    ModifyRenameOther,
    ModifyOther,
    Delete,
    DeleteFile,
    DeleteFolder,
    DeleteOther,
    Other,
});

impl From<&EventKind> for Interest {
    fn from(kind: &EventKind) -> Self {
        match kind {
            EventKind::Any => Self::Any,
            EventKind::Access(AccessKind::Any) => Self::Access,
            EventKind::Access(AccessKind::Close(_)) => Self::AccessClose,
            EventKind::Access(AccessKind::Open(_)) => Self::AccessOpen,
            EventKind::Access(AccessKind::Read) => Self::AccessRead,
            EventKind::Access(AccessKind::Other) => Self::AccessOther,
            EventKind::Create(CreateKind::Any) => Self::Create,
            EventKind::Create(CreateKind::File) => Self::CreateFile,
            EventKind::Create(CreateKind::Folder) => Self::CreateFolder,
            EventKind::Create(CreateKind::Other) => Self::CreateOther,
            EventKind::Modify(ModifyKind::Any) => Self::Modify,
            EventKind::Modify(ModifyKind::Data(DataChange::Any)) => Self::ModifyData,
            EventKind::Modify(ModifyKind::Data(DataChange::Content)) => {
                Self::ModifyDataContent
            }
            EventKind::Modify(ModifyKind::Data(DataChange::Size)) => Self::ModifyDataSize,
            EventKind::Modify(ModifyKind::Data(DataChange::Other)) => {
                Self::ModifyDataOther
            }
            EventKind::Modify(ModifyKind::Metadata(MetadataKind::Any)) => {
                Self::ModifyMetadata
            }
            EventKind::Modify(ModifyKind::Metadata(MetadataKind::AccessTime)) => {
                Self::ModifyMetadataAccessTime
            }
            EventKind::Modify(ModifyKind::Metadata(MetadataKind::Extended)) => {
                Self::ModifyMetadataExtended
            }
            EventKind::Modify(ModifyKind::Metadata(MetadataKind::Other)) => {
                Self::ModifyMetadataOther
            }
            EventKind::Modify(ModifyKind::Metadata(MetadataKind::Ownership)) => {
                Self::ModifyMetadataOwnership
            }
            EventKind::Modify(ModifyKind::Metadata(MetadataKind::Permissions)) => {
                Self::ModifyMetadataPermissions
            }
            EventKind::Modify(ModifyKind::Metadata(MetadataKind::WriteTime)) => {
                Self::ModifyMetadataWriteTime
            }
            EventKind::Modify(ModifyKind::Name(RenameMode::Any)) => Self::ModifyRename,
            EventKind::Modify(ModifyKind::Name(RenameMode::Both)) => {
                Self::ModifyRenameBoth
            }
            EventKind::Modify(ModifyKind::Name(RenameMode::From)) => {
                Self::ModifyRenameFrom
            }
            EventKind::Modify(ModifyKind::Name(RenameMode::To)) => Self::ModifyRenameTo,
            EventKind::Modify(ModifyKind::Name(RenameMode::Other)) => {
                Self::ModifyRenameOther
            }
            EventKind::Modify(ModifyKind::Other) => Self::ModifyOther,
            EventKind::Remove(RemoveKind::Any) => Self::Delete,
            EventKind::Remove(RemoveKind::File) => Self::DeleteFile,
            EventKind::Remove(RemoveKind::Folder) => Self::DeleteFolder,
            EventKind::Remove(RemoveKind::Other) => Self::DeleteOther,
            EventKind::Other => Self::Other,
        }
    }
}

#[derive(Debug, Clone)]
struct Watch {
    path: ArcStr,
    id: BindId,
    interest: BitFlags<Interest>,
}

impl Watch {
    fn interested(&self, kind: &EventKind) -> bool {
        use Interest::*;
        match kind {
            EventKind::Any => !self.interest.is_empty(),
            EventKind::Access(AccessKind::Any) => self
                .interest
                .intersects(Access | AccessClose | AccessOpen | AccessRead | AccessOther),
            EventKind::Access(AccessKind::Close(_)) => {
                self.interest.intersects(Access | AccessClose)
            }
            EventKind::Access(AccessKind::Open(_)) => {
                self.interest.intersects(Access | AccessOpen)
            }
            EventKind::Access(AccessKind::Read) => {
                self.interest.intersects(Access | AccessRead)
            }
            EventKind::Access(AccessKind::Other) => {
                self.interest.intersects(Access | AccessOther)
            }
            EventKind::Create(CreateKind::Any) => {
                self.interest.intersects(Create | CreateFile | CreateFolder | CreateOther)
            }
            EventKind::Create(CreateKind::File) => {
                self.interest.intersects(Create | CreateFile)
            }
            EventKind::Create(CreateKind::Folder) => {
                self.interest.intersects(Create | CreateFolder)
            }
            EventKind::Create(CreateKind::Other) => {
                self.interest.intersects(Create | CreateOther)
            }
            EventKind::Modify(ModifyKind::Any) => self.interest.intersects(
                Modify
                    | ModifyData
                    | ModifyDataSize
                    | ModifyDataContent
                    | ModifyDataOther
                    | ModifyMetadata
                    | ModifyMetadataAccessTime
                    | ModifyMetadataWriteTime
                    | ModifyMetadataPermissions
                    | ModifyMetadataOwnership
                    | ModifyMetadataExtended
                    | ModifyMetadataOther
                    | ModifyRename
                    | ModifyRenameTo
                    | ModifyRenameFrom
                    | ModifyRenameBoth
                    | ModifyRenameOther
                    | ModifyOther,
            ),
            EventKind::Modify(ModifyKind::Data(DataChange::Any)) => {
                self.interest.intersects(
                    Modify
                        | ModifyData
                        | ModifyDataSize
                        | ModifyDataContent
                        | ModifyDataOther,
                )
            }
            EventKind::Modify(ModifyKind::Data(DataChange::Content)) => {
                self.interest.intersects(Modify | ModifyData | ModifyDataContent)
            }
            EventKind::Modify(ModifyKind::Data(DataChange::Size)) => {
                self.interest.intersects(Modify | ModifyData | ModifyDataSize)
            }
            EventKind::Modify(ModifyKind::Data(DataChange::Other)) => {
                self.interest.intersects(Modify | ModifyData | ModifyDataOther)
            }
            EventKind::Modify(ModifyKind::Metadata(MetadataKind::Any)) => {
                self.interest.intersects(
                    Modify
                        | ModifyMetadata
                        | ModifyMetadataAccessTime
                        | ModifyMetadataWriteTime
                        | ModifyMetadataPermissions
                        | ModifyMetadataOwnership
                        | ModifyMetadataExtended
                        | ModifyMetadataOther,
                )
            }
            EventKind::Modify(ModifyKind::Metadata(MetadataKind::AccessTime)) => self
                .interest
                .intersects(Modify | ModifyMetadata | ModifyMetadataAccessTime),
            EventKind::Modify(ModifyKind::Metadata(MetadataKind::Extended)) => {
                self.interest.intersects(Modify | ModifyMetadata | ModifyMetadataExtended)
            }
            EventKind::Modify(ModifyKind::Metadata(MetadataKind::Other)) => {
                self.interest.intersects(Modify | ModifyMetadata | ModifyMetadataOther)
            }
            EventKind::Modify(ModifyKind::Metadata(MetadataKind::Ownership)) => self
                .interest
                .intersects(Modify | ModifyMetadata | ModifyMetadataOwnership),
            EventKind::Modify(ModifyKind::Metadata(MetadataKind::Permissions)) => self
                .interest
                .intersects(Modify | ModifyMetadata | ModifyMetadataPermissions),
            EventKind::Modify(ModifyKind::Metadata(MetadataKind::WriteTime)) => self
                .interest
                .intersects(Modify | ModifyMetadata | ModifyMetadataWriteTime),
            EventKind::Modify(ModifyKind::Name(RenameMode::Any)) => {
                self.interest.intersects(
                    Modify
                        | ModifyRename
                        | ModifyRenameTo
                        | ModifyRenameFrom
                        | ModifyRenameBoth
                        | ModifyRenameOther,
                )
            }
            EventKind::Modify(ModifyKind::Name(RenameMode::Both)) => {
                self.interest.intersects(Modify | ModifyRename | ModifyRenameBoth)
            }
            EventKind::Modify(ModifyKind::Name(RenameMode::From)) => {
                self.interest.intersects(Modify | ModifyRename | ModifyRenameFrom)
            }
            EventKind::Modify(ModifyKind::Name(RenameMode::To)) => {
                self.interest.intersects(Modify | ModifyRename | ModifyRenameTo)
            }
            EventKind::Modify(ModifyKind::Name(RenameMode::Other)) => {
                self.interest.intersects(Modify | ModifyRename | ModifyRenameOther)
            }
            EventKind::Modify(ModifyKind::Other) => {
                self.interest.intersects(Modify | ModifyOther)
            }
            EventKind::Remove(RemoveKind::Any) => {
                self.interest.intersects(Delete | DeleteFile | DeleteFolder | DeleteOther)
            }
            EventKind::Remove(RemoveKind::File) => {
                self.interest.intersects(Delete | DeleteFile)
            }
            EventKind::Remove(RemoveKind::Folder) => {
                self.interest.intersects(Delete | DeleteFolder)
            }
            EventKind::Remove(RemoveKind::Other) => {
                self.interest.intersects(Delete | DeleteOther)
            }
            EventKind::Other => self.interest.contains(Other),
        }
    }
}

#[derive(Debug)]
enum WatchCmd {
    Watch(Watch),
    Stop(BindId),
    SetPollInterval(Duration),
    SetPollBatch(usize),
}

#[derive(Debug, Clone)]
enum WatchEventKind {
    Error(ArcStr),
    Event(Interest),
}

#[derive(Debug)]
struct WatchEvent {
    paths: GPooled<FxHashSet<ArcStr>>,
    event: WatchEventKind,
}

impl CustomBuiltinType for WatchEvent {}

#[derive(Debug, Clone, PartialEq, Eq)]
struct PathStatus {
    /// the canonical part of the path that exists
    exists: PathBuf,
    /// the path parts that are missing, in reverse order
    missing: LPooled<Vec<OsString>>,
    /// the full path including the missing parts, which aren't canonical
    full_path: PathBuf,
}

impl PathStatus {
    fn exists(&self) -> bool {
        self.missing.is_empty()
    }

    async fn new(path: &Path) -> Self {
        let mut missing: LPooled<Vec<OsString>> = LPooled::take();
        let mut root: &Path = &path;
        let mut t = loop {
            match fs::canonicalize(root).await {
                Ok(exists) => {
                    break Self { exists: exists.clone(), missing, full_path: exists }
                }
                Err(_) => match (root.parent(), root.file_name()) {
                    (None, None) => {
                        break Self {
                            exists: PathBuf::from(path),
                            missing,
                            full_path: PathBuf::from(path),
                        }
                    }
                    (None, Some(_)) => {
                        break Self {
                            exists: PathBuf::from(root),
                            missing,
                            full_path: PathBuf::from(root),
                        }
                    }
                    (Some(parent), None) => root = parent,
                    (Some(parent), Some(file)) => {
                        missing.push(OsString::from(file));
                        root = parent;
                    }
                },
            }
        };
        for part in t.missing.iter().rev() {
            t.full_path.push(part);
        }
        t
    }
}

fn utf8_path(path: &PathBuf) -> ArcStr {
    ArcStr::from(&*path.as_os_str().to_string_lossy())
}

#[derive(Debug, Clone)]
struct WatchInt {
    watch: Watch,
    path_status: PathStatus,
}

enum AddAction {
    AddWatch { path: PathBuf, notify: bool },
    AddPending { watch_path: PathBuf, full_path: PathBuf, notify: bool },
}

struct ChangeOfStatus {
    remove: Option<PathBuf>,
    add: Option<AddAction>,
    syn: bool,
}

#[derive(Default)]
struct Watched {
    by_id: FxHashMap<BindId, WatchInt>,
    by_root: FxHashMap<PathBuf, FxHashSet<BindId>>,
    to_poll: Vec<BindId>,
    poll_batch: Option<usize>,
}

impl Watched {
    /// add a watch and return the necessary action to the notify::Watcher
    async fn add_watch(&mut self, w: Watch) -> AddAction {
        let id = w.id;
        let path_status = PathStatus::new(&Path::new(&*w.path)).await;
        let w = WatchInt { watch: w, path_status };
        let watch_path = w.path_status.exists.clone();
        let full_path = w.path_status.full_path.clone();
        let exists = w.path_status.exists();
        let notify = w.watch.interest.contains(Interest::Established);
        self.by_root.entry(watch_path.clone()).or_default().insert(id);
        self.by_id.insert(id, w);
        if exists {
            AddAction::AddWatch { path: watch_path, notify }
        } else {
            AddAction::AddPending { watch_path, full_path, notify }
        }
    }

    /// remove a watch, and return an optional action to be performed on the
    /// watcher
    fn remove_watch(&mut self, id: &BindId) -> (Option<WatchInt>, Option<PathBuf>) {
        let w = self.by_id.remove(id);
        let to_stop =
            w.as_ref().and_then(|w| match self.by_root.get_mut(&w.path_status.exists) {
                None => Some(w.path_status.exists.clone()),
                Some(ids) => {
                    ids.remove(id);
                    ids.is_empty().then(|| {
                        self.by_root.remove(&w.path_status.exists);
                        w.path_status.exists.clone()
                    })
                }
            });
        (w, to_stop)
    }

    /// inform of a change of status for this watch id, return a set of
    /// necessary actions to the notify::Watcher
    async fn change_status(&mut self, id: BindId) -> ChangeOfStatus {
        let (w, remove) = self.remove_watch(&id);
        // syn true means we need to generate a synthetic delete
        // for a delete that otherwise would not be reported
        let mut syn = false;
        let add = match w {
            Some(w) => {
                syn = &Some(w.path_status.exists) == &remove;
                Some(self.add_watch(w.watch).await)
            }
            None => None,
        };
        let (remove, add) = match (&remove, &add) {
            (Some(remove), Some(AddAction::AddPending { watch_path, .. }))
            | (Some(remove), Some(AddAction::AddWatch { path: watch_path, .. }))
                if remove == watch_path =>
            {
                (None, None)
            }
            (Some(_), Some(AddAction::AddPending { .. }))
            | (Some(_), Some(AddAction::AddWatch { .. }))
            | (None, Some(_))
            | (Some(_), None)
            | (None, None) => (remove, add),
        };
        ChangeOfStatus { remove, add, syn }
    }

    fn relevant_to<'a>(&'a self, path: &'a Path) -> impl Iterator<Item = &'a WatchInt> {
        struct I<'a> {
            level: usize,
            ids: LPooled<VecDeque<hash_set::Iter<'a, BindId>>>,
            t: &'a Watched,
        }
        impl<'a> Iterator for I<'a> {
            type Item = &'a WatchInt;

            fn next(&mut self) -> Option<Self::Item> {
                loop {
                    match self.ids.front_mut() {
                        None => break None,
                        Some(set) => match set.next() {
                            Some(id) => match self.t.by_id.get(id) {
                                Some(w)
                                // limit established paths to the path itself and it's immediate parent
                                if self.level < 2 || !w.path_status.exists() =>
                                {
                                    break Some(w)
                                }
                                Some(_) | None => continue,
                            },
                            None => {
                                self.level += 1;
                                self.ids.pop_front();
                            }
                        },
                    }
                }
            }
        }
        let mut ids: LPooled<VecDeque<hash_set::Iter<'a, BindId>>> = LPooled::take();
        let mut root = Some(path);
        while let Some(path) = root {
            if let Some(h) = self.by_root.get(path) {
                ids.push_back(h.iter())
            }
            root = path.parent();
        }
        I { level: 0, ids, t: self }
    }

    fn poll_batch(&self) -> usize {
        self.poll_batch.unwrap_or(POLL_BATCH)
    }

    /// poll all watches and return a list of ids who's status might have changed
    async fn poll_cycle(&mut self) -> LPooled<Vec<BindId>> {
        if self.to_poll.is_empty() {
            self.to_poll.extend(self.by_id.keys().map(|id| *id));
        }
        let poll_batch = self.poll_batch();
        let mut to_check: LPooled<Vec<&WatchInt>> = LPooled::take();
        let mut i = 0;
        while i < poll_batch
            && let Some(id) = self.to_poll.pop()
        {
            i += 1;
            if let Some(w) = self.by_id.get(&id) {
                to_check.push(w)
            }
        }
        join_all(to_check.drain(..).map(|w| async {
            let exists =
                tokio::time::timeout(POLL_TIMEOUT, tokio::fs::try_exists(&*w.watch.path))
                    .await
                    .unwrap_or(Ok(false))
                    .unwrap_or(false);
            let established = w.path_status.exists();
            if (established && !exists) || (!established && exists) {
                Some(w.watch.id)
            } else {
                None
            }
        }))
        .await
        .into_iter()
        .filter_map(|x| x)
        .collect()
    }

    async fn process_event(
        &mut self,
        batch: &mut GPooled<Vec<(BindId, Box<dyn CustomBuiltinType>)>>,
        ev: Result<notify::Event, notify::Error>,
    ) -> LPooled<Vec<BindId>> {
        eprintln!("watch event {ev:?}");
        let mut by_id: LPooled<FxHashMap<BindId, WatchEvent>> = LPooled::take();
        let mut status_changed: LPooled<Vec<BindId>> = LPooled::take();
        match ev {
            Ok(ev) => {
                let event = WatchEventKind::Event((&ev.kind).into());
                for path in ev.paths.iter() {
                    let utf8_path = utf8_path(path);
                    for w in self.relevant_to(path) {
                        macro_rules! report {
                            () => {{
                                let wev = by_id.entry(w.watch.id).or_insert_with(|| {
                                    WatchEvent {
                                        event: event.clone(),
                                        paths: PATHS.take(),
                                    }
                                });
                                wev.paths.insert(utf8_path.clone());
                            }};
                        }
                        if w.path_status.exists() {
                            if w.watch.interested(&ev.kind) {
                                report!();
                            }
                            match &ev.kind {
                                EventKind::Remove(_)
                                | EventKind::Modify(ModifyKind::Name(RenameMode::From))
                                    if path == &w.path_status.exists =>
                                {
                                    status_changed.push(w.watch.id)
                                }
                                EventKind::Any
                                | EventKind::Access(_)
                                | EventKind::Create(_)
                                | EventKind::Modify(_)
                                | EventKind::Remove(_)
                                | EventKind::Other => (),
                            }
                        } else {
                            match &ev.kind {
                                EventKind::Create(_)
                                | EventKind::Modify(ModifyKind::Name(RenameMode::To)) => {
                                    if w.watch.interested(&ev.kind)
                                        && tokio::fs::try_exists(&*w.watch.path)
                                            .await
                                            .unwrap_or(false)
                                    {
                                        report!()
                                    }
                                    status_changed.push(w.watch.id);
                                }
                                EventKind::Any
                                | EventKind::Access(_)
                                | EventKind::Modify(_)
                                | EventKind::Remove(_)
                                | EventKind::Other => (),
                            }
                        }
                    }
                }
            }
            Err(e) => {
                let err = ArcStr::from(format_compact!("{:?}", e.kind).as_str());
                for path in e.paths.iter() {
                    let utf8_path = utf8_path(path);
                    for w in self.relevant_to(&path) {
                        match &e.kind {
                            notify::ErrorKind::PathNotFound => {
                                status_changed.push(w.watch.id)
                            }
                            _ => {
                                let wev = by_id.entry(w.watch.id).or_insert_with(|| {
                                    WatchEvent {
                                        paths: PATHS.take(),
                                        event: WatchEventKind::Error(err.clone()),
                                    }
                                });
                                wev.paths.insert(utf8_path.clone());
                            }
                        }
                    }
                }
            }
        }
        let evs = by_id
            .drain()
            .map(|(id, wev)| (id, Box::new(wev) as Box<dyn CustomBuiltinType>));
        batch.extend(evs);
        status_changed
    }
}

fn push_error(
    batch: &mut GPooled<Vec<(BindId, Box<dyn CustomBuiltinType>)>>,
    id: BindId,
    path: Option<ArcStr>,
    e: Either<notify::Error, ArcStr>,
) {
    let mut wev = WatchEvent {
        paths: PATHS.take(),
        event: WatchEventKind::Error(match e {
            Either::Left(e) => ArcStr::from(format_compact!("{e:?}").as_str()),
            Either::Right(e) => e,
        }),
    };
    if let Some(path) = path {
        wev.paths.insert(path);
    }
    batch.push((id, Box::new(wev)))
}

fn push_event(
    batch: &mut GPooled<Vec<(BindId, Box<dyn CustomBuiltinType>)>>,
    id: BindId,
    path: &PathBuf,
    event: WatchEventKind,
) {
    let path = utf8_path(path);
    let mut wev = WatchEvent { paths: PATHS.take(), event };
    wev.paths.insert(path);
    batch.push((id, Box::new(wev)))
}

async fn file_watcher_loop(
    mut watcher: RecommendedWatcher,
    mut rx_notify: tmpsc::Receiver<notify::Result<notify::Event>>,
    mut rx: mpsc::UnboundedReceiver<WatchCmd>,
    mut tx: mpsc::Sender<GPooled<Vec<(BindId, Box<dyn CustomBuiltinType>)>>>,
) {
    let mut watched = Watched::default();
    let mut recv_buf = vec![];
    let mut batch = CBATCH_POOL.take();
    let mut poll_interval = tokio::time::interval(DEFAULT_POLL_INTERVAL);
    macro_rules! or_push {
        ($path:expr, $id:expr, $r:expr) => {
            if let Err(e) = $r {
                let path = utf8_path(&$path);
                push_error(&mut batch, $id, Some(path), Either::Left(e))
            }
        };
    }
    macro_rules! add_watch {
        ($path:expr, $id:expr, on_success: $success:block) => {
            match watcher.watch($path, RecursiveMode::NonRecursive) {
                Ok(()) => $success,
                Err(e) => {
                    push_error(&mut batch, $id, Some(utf8_path($path)), Either::Left(e))
                }
            }
        };
    }
    macro_rules! status_change {
        ($id:expr, $synthetic:expr) => {{
            let stc = watched.change_status($id).await;
            if let Some(path) = stc.remove {
                if $synthetic
                    && stc.syn
                    && let Some(w) = watched.by_id.get(&$id)
                    && w.watch.interest.intersects(
                        Interest::Delete
                        | Interest::DeleteFile
                        | Interest::DeleteFolder
                        | Interest::Other)
                {
                    push_event(&mut batch, $id, &path, WatchEventKind::Event(Interest::Delete))
                }
                or_push!(&path, $id, watcher.unwatch(&path));
            }
            match stc.add {
                None => (),
                Some(AddAction::AddWatch { path, .. }) => {
                    add_watch!(&path, $id, on_success: {
                        if $synthetic
                            && let Some(w) = watched.by_id.get(&$id)
                            && w.watch.interest.intersects(
                                Interest::Create
                                | Interest::CreateFile
                                | Interest::CreateFolder
                                | Interest::CreateOther)
                        {
                            push_event(&mut batch, $id, &path, WatchEventKind::Event(Interest::Create))
                        }
                    });
                }
                Some(AddAction::AddPending { watch_path, .. }) => {
                    add_watch!(&watch_path, $id, on_success: { () })
                }
            }
        }}
    }
    loop {
        select! {
            _ = poll_interval.tick() => {
                if watched.poll_batch() > 0 {
                    for id in watched.poll_cycle().await.drain(..) {
                        status_change!(id, true)
                    }
                }
            },
            n = rx_notify.recv_many(&mut recv_buf, MAX_NOTIFY_BATCH) => {
                if n == 0 {
                    break
                }
                for ev in recv_buf.drain(..) {
                    let mut status = watched.process_event(&mut batch, ev).await;
                    for id in status.drain(..) {
                        status_change!(id, false)
                    }
                }
            },
            cmd = rx.next() => match cmd {
                None => break,
                Some(WatchCmd::Watch(w)) => {
                    let id = w.id;
                    let nev = WatchEventKind::Event(Interest::Established);
                    let mut same_path = false;
                    if let (Some(old_w), Some(remove)) = watched.remove_watch(&id) {
                        if old_w.watch.path == w.path {
                            same_path = true
                        } else {
                            if let Err(e) = watcher.unwatch(&remove) {
                                log::warn!("could not unwatch {} when switching {e:?}", remove.display())
                            }
                        }
                    }
                    match watched.add_watch(w).await {
                        AddAction::AddWatch { path, notify } if !same_path => {
                            add_watch!(&path, id, on_success: {
                                if notify {
                                    push_event(&mut batch, id, &path, nev)
                                }
                            })
                        }
                        AddAction::AddWatch { path, notify } => {
                            if notify {
                                push_event(&mut batch, id, &path, nev)
                            }
                        }
                        AddAction::AddPending { watch_path, full_path, notify } => {
                            add_watch!(&watch_path, id, on_success: {
                                if notify {
                                    push_event(&mut batch, id, &full_path, nev);
                                }
                            })
                        }
                    }
                },
                Some(WatchCmd::Stop(id)) => match watched.remove_watch(&id).1 {
                    None => (),
                    Some(path) => {
                        or_push!(path, id, watcher.unwatch(&path))
                    }
                },
                Some(WatchCmd::SetPollInterval(d)) => {
                    poll_interval = tokio::time::interval(d);
                }
                Some(WatchCmd::SetPollBatch(n)) => {
                    watched.poll_batch = Some(n)
                }
            },
        }
        if !batch.is_empty() {
            if let Err(_) = tx.send(batch).await {
                break;
            }
            batch = CBATCH_POOL.take()
        }
    }
    let mut batch = CBATCH_POOL.take();
    while let Ok(Some(cmd)) = rx.try_next() {
        match cmd {
            WatchCmd::Stop(_)
            | WatchCmd::SetPollBatch(_)
            | WatchCmd::SetPollInterval(_) => (),
            WatchCmd::Watch(w) => {
                let e = literal!("the watcher thread has stopped");
                push_error(&mut batch, w.id, None, Either::Right(e));
            }
        }
    }
    if !batch.is_empty() {
        let _ = tx.send(batch).await;
    }
}

struct WatchCtx(anyhow::Result<mpsc::UnboundedSender<WatchCmd>>);

impl WatchCtx {
    fn send(&self, cmd: WatchCmd) -> anyhow::Result<()> {
        match &self.0 {
            Ok(tx) => tx.unbounded_send(cmd).map_err(|_| anyhow!("watcher died")),
            Err(e) => bail!("could not start watcher {e:?}"),
        }
    }
}

struct NotifyChan(tmpsc::Sender<notify::Result<notify::Event>>);

impl notify::EventHandler for NotifyChan {
    fn handle_event(&mut self, event: notify::Result<notify::Event>) {
        let _ = self.0.blocking_send(event);
    }
}

fn get_watcher<'a, R: Rt>(rt: &mut R, st: &'a mut LibState) -> &'a mut WatchCtx {
    st.get_or_else::<WatchCtx, _>(|| {
        let (notify_tx, notify_rx) = tmpsc::channel(10);
        let notify_tx = NotifyChan(notify_tx);
        match notify::recommended_watcher(notify_tx) {
            Err(e) => WatchCtx(Err(e.into())),
            Ok(watcher) => {
                let (cmd_tx, cmd_rx) = mpsc::unbounded();
                let (ev_tx, ev_rx) = mpsc::channel(10);
                task::spawn(async move {
                    file_watcher_loop(watcher, notify_rx, cmd_rx, ev_tx).await
                });
                rt.watch(ev_rx);
                WatchCtx(Ok(cmd_tx))
            }
        }
    })
}

#[derive(Debug)]
pub(super) struct SetGlobals;

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for SetGlobals {
    const NAME: &str = "fs_set_global_watch_parameters";
    deftype!(
        "fs",
        r#"fn(
            ?#poll_interval:[duration, null],
            ?#poll_batch_size:[i64, null]
        ) -> Result<null, `WatchError(string)>"#
    );

    fn init(_: &mut ExecCtx<R, E>) -> BuiltInInitFn<R, E> {
        Arc::new(|_, _, _, _, _| Ok(Box::new(SetGlobals)))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for SetGlobals {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        let poll_interval = from[0]
            .update(ctx, event)
            .and_then(|v| v.cast_to::<Option<Duration>>().ok().flatten());
        let batch_size = from[1]
            .update(ctx, event)
            .and_then(|v| v.cast_to::<Option<i64>>().ok().flatten());
        match poll_interval {
            Some(poll_interval) if poll_interval < Duration::from_millis(100) => {
                return Some(errf!("WatchError", "poll_interval must be >= 100ms"))
            }
            None | Some(_) => (),
        }
        match batch_size {
            Some(batch_size) if batch_size < 0 => {
                return Some(errf!("WatchError", "batch_size must be >= 0"))
            }
            None | Some(_) => (),
        }
        let mut err = String::new();
        use std::fmt::Write;
        if let Some(poll_interval) = poll_interval {
            let wctx = get_watcher(&mut ctx.rt, &mut ctx.libstate);
            if let Err(e) = wctx.send(WatchCmd::SetPollInterval(poll_interval)) {
                write!(err, "could not set poll interval: {e:?}").unwrap();
            }
        }
        if let Some(batch_size) = batch_size {
            let wctx = get_watcher(&mut ctx.rt, &mut ctx.libstate);
            if let Err(e) = wctx.send(WatchCmd::SetPollBatch(batch_size as usize)) {
                if !err.is_empty() {
                    write!(err, ", ").unwrap()
                }
                write!(err, "could not set poll interval: {e:?}").unwrap()
            }
        }
        if poll_interval.is_some() || batch_size.is_some() {
            if err.is_empty() {
                Some(Value::Null)
            } else {
                Some(errf!("WatchError", "{err}"))
            }
        } else {
            None
        }
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {}
}

macro_rules! watch {
    (
        type_name: $type_name:ident,
        builtin_name: $builtin_name:literal,
        graphix_type: $graphix_type:literal,
        handle_event: |$id:ident, $ctx:ident, $ev:ident| $handle_event:block
    ) => {
        #[derive(Debug)]
        pub(super) struct $type_name {
            id: BindId,
            top_id: ExprId,
            interest: Option<BitFlags<Interest>>,
            path: Option<ArcStr>,
        }

        impl<R: Rt, E: UserEvent> BuiltIn<R, E> for $type_name {
            const NAME: &str = $builtin_name;
            deftype!("fs", $graphix_type);

            fn init(_: &mut ExecCtx<R, E>) -> BuiltInInitFn<R, E> {
                Arc::new(|ctx, _, _, _, top_id| {
                    let id = BindId::new();
                    ctx.rt.ref_var(id, top_id);
                    Ok(Box::new($type_name { id, top_id, interest: None, path: None }))
                })
            }
        }

        impl<R: Rt, E: UserEvent> Apply<R, E> for $type_name {
            fn update(
                &mut self,
                ctx: &mut ExecCtx<R, E>,
                from: &mut [Node<R, E>],
                event: &mut Event<E>,
            ) -> Option<Value> {
                let mut up = false;
                if let Some(Ok(mut int)) = from[0]
                    .update(ctx, event)
                    .map(|v| v.cast_to::<LPooled<Vec<Interest>>>())
                {
                    let int = int.drain(..).fold(BitFlags::empty(), |mut acc, fl| {
                        acc.insert(fl);
                        acc
                    });
                    up |= self.interest != Some(int);
                    self.interest = Some(int);
                }
                if let Some(Ok(path)) =
                    from[1].update(ctx, event).map(|v| v.cast_to::<ArcStr>())
                {
                    let path = Some(path);
                    up = path != self.path;
                    self.path = path;
                }
                if up
                    && let Some(path) = &self.path
                    && let Some(interest) = self.interest
                {
                    let wctx = get_watcher(&mut ctx.rt, &mut ctx.libstate);
                    if let Err(e) = wctx.send(WatchCmd::Watch(Watch {
                        path: path.clone(),
                        id: self.id,
                        interest,
                    })) {
                        ctx.rt.set_var(self.id, errf!("WatchError", "{e:?}"));
                    }
                }
                if let Some(mut w) = event.custom.remove(&self.id)
                    && let Some(w) =
                        (&mut *w as &mut dyn Any).downcast_mut::<WatchEvent>()
                {
                    let $id = self.id;
                    let $ctx = ctx;
                    let $ev = w;
                    $handle_event;
                }
                event.variables.get(&self.id).cloned()
            }

            fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
                let wctx = get_watcher(&mut ctx.rt, &mut ctx.libstate);
                let _ = wctx.send(WatchCmd::Stop(self.id));
                ctx.rt.unref_var(self.id, self.top_id);
                self.id = BindId::new();
                ctx.rt.ref_var(self.id, self.top_id);
            }

            fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
                let wctx = get_watcher(&mut ctx.rt, &mut ctx.libstate);
                let _ = wctx.send(WatchCmd::Stop(self.id));
                ctx.rt.unref_var(self.id, self.top_id);
            }
        }
    };
}

watch!(
    type_name: WatchBuiltIn,
    builtin_name: "fs_watch",
    graphix_type: "fn(?#interest:Array<Interest>, string) -> Result<string, `WatchError(string)>",
    handle_event: |id, ctx, w| {
        match &w.event {
            WatchEventKind::Event(_) => {
                for p in w.paths.drain() {
                    ctx.rt.set_var(id, Value::String(p))
                }
            }
            WatchEventKind::Error(e) => ctx.rt.set_var(id, errf!("WatchError", "{e:?}")),
        }
    }
);

watch!(
    type_name: WatchFullBuiltIn,
    builtin_name: "fs_watch_full",
    graphix_type: "fn(?#interest:Array<Interest>, string) -> Result<WatchEvent, `WatchError(string)>",
    handle_event: |id, ctx, w| {
        let paths =
            Value::Array(ValArray::from_iter_exact(w.paths.drain().map(Value::String)));
        match &w.event {
            WatchEventKind::Error(e) => ctx.rt.set_var(id, errf!("WatchError", "{e:?}")),
            WatchEventKind::Event(int) => {
                let e =
                    ((literal!("event"), Value::from(int)), (literal!("paths"), paths));
                ctx.rt.set_var(id, e.into())
            }
        }
    }
);
