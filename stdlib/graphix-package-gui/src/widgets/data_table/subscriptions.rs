//! Subscription dispatch and the DataTableW methods that drive live
//! data: row/sort subs, column-type diff application, and re-sorting.
//!
//! `SharedCells` owns every netidx subscription this widget creates.
//! The dispatch task writes raw `Value`s into `inner.values` keyed by
//! `SubId`; the render path looks up by cell address through
//! `inner.cells: (Path, col) → SubId` and formats only the values that
//! are actually drawn. Subscription updates can run far faster than
//! draws (a 100 Hz cell on a 60 Hz display) and we don't want to
//! allocate one ArcStr per dropped update.

use super::types::{
    decimate_sparkline, format_value, numeric_key, parse_selection, parse_table_columns,
    row_basename, value_to_f64, ColumnState, ColumnType, SortDirection, SourceEntry,
};
use super::{
    compile_callable_opt, DataTableW, DisplayMode, MAX_SPARKLINE_POINTS, VALUE_COL_KEY,
};
use ahash::{AHashMap, AHashSet};
use anyhow::Result;
use arcstr::ArcStr;
use compact_str::format_compact;
use futures::channel::mpsc;
use graphix_rt::{CallableId, GXExt, GXHandle};
use log::warn;
use netidx::{
    path::Path,
    protocol::valarray::ValArray,
    publisher::Value,
    subscriber::{Dval, Event, SubId, UpdatesFlags},
};
use nohash::IntMap;
use parking_lot::Mutex;
use poolshark::{global::GPooled, local::LPooled};
use smallvec::SmallVec;
use std::{
    collections::VecDeque,
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc, Weak,
    },
    time::{Duration, Instant},
};

/// One role a subscription plays. A single `SubId` can play several
/// roles when the same cell path is both displayed in the grid and
/// listed in `sort_by` — netidx dedupes subscriptions by path, so
/// the subscriber returns the same `Dval` (same `SubId`) for a
/// repeated subscribe call and our routing has to fan one update out
/// to every role registered for it.
#[derive(Clone)]
pub(super) enum SubRole {
    /// A cell subscription. `col_name` is `VALUE_COL_KEY` for
    /// `DisplayMode::Value`. `sparkline_history_secs = Some(secs)`
    /// when the column is a sparkline type, `None` otherwise.
    Grid { row_path: Path, col_name: ArcStr, sparkline_history_secs: Option<f64> },
    /// A sort-column subscription. The actual sort key for a row is
    /// recovered at sort time via the `cells: (Path, col) → SubId`
    /// index, so this is just a marker — its presence on a SubId tells
    /// the dispatch task to set `sort_col_dirty` on every update so
    /// `before_view` triggers a re-sort.
    Sort,
}

/// All roles for a single `SubId`. Inline capacity 2 covers the
/// common case: cell subscription + optional sort role.
pub(super) type SubRoles = SmallVec<[SubRole; 2]>;

/// Plain-data inner state of `SharedCells`, guarded by a single
/// `Mutex` in the outer struct. Locking once per dispatch batch
/// (vs. four interleaved locks before) keeps the hot path as cheap
/// as possible.
pub(super) struct SharedCellsInner {
    /// `SubId` → owned `Dval`. Dropping a Dval cancels the netidx
    /// subscription, so this map is the single source of subscription
    /// lifetime; the widget no longer holds row_subs or sort_col_subs.
    pub(super) dvals: IntMap<SubId, Dval>,
    /// Most recent `Value` from each subscription. Stored raw — the
    /// dispatch task writes the netidx `Value` as-is and we format to
    /// `ArcStr` only at draw / sort time. A 100 Hz cell scrolled out
    /// of view costs one `Value::clone` per update (refcount bump),
    /// not one ArcStr allocation.
    pub(super) values: IntMap<SubId, Value>,
    /// Lazy `ArcStr` cache for display. Populated at render time when
    /// a cell is first formatted; the dispatch task evicts an entry
    /// whenever its `Value` changes. So a stable cell allocates one
    /// ArcStr the first time it is drawn and reuses it forever; a
    /// fast-updating off-screen cell never allocates because its
    /// invalidations are never followed by a render.
    pub(super) formatted: IntMap<SubId, ArcStr>,
    /// `(row_path, col_name)` → `SubId`. Cell index used by the render
    /// path to find the value for a given cell. Sort columns share
    /// `SubId`s with grid cells whenever the paths coincide (netidx
    /// dedupes `subscribe` by path), so an entry exists for every
    /// `(Path, col)` we have a live subscription on.
    pub(super) cells: AHashMap<(Path, ArcStr), SubId>,
    /// `SubId` → roles played by that subscription. Multi-role because
    /// a cell that is also sorted on registers two roles against the
    /// same id; the dispatch task fans every update out to each role.
    pub(super) routing: IntMap<SubId, SubRoles>,
    /// Sparkline history: `(row_path, col_name)` → timestamped values.
    /// Identity-keyed so history survives row reordering. `LPooled`
    /// returns `VecDeque`s to the thread-local pool when sparkline
    /// cells churn (sort/scroll evicts and re-creates entries).
    pub(super) sparklines: AHashMap<(Path, ArcStr), LPooled<VecDeque<(Instant, f64)>>>,
    /// Latest `on_update` callable id (if any). Written by the widget
    /// when the `on_update` ref resolves; read by the dispatch task at
    /// the start of each batch.
    pub(super) on_update: Option<CallableId>,
}

impl SharedCellsInner {
    fn new() -> Self {
        Self {
            dvals: IntMap::default(),
            values: IntMap::default(),
            formatted: IntMap::default(),
            cells: AHashMap::default(),
            routing: IntMap::default(),
            sparklines: AHashMap::default(),
            on_update: None,
        }
    }

    /// Format the cached display string for `id`, populating the
    /// cache on a miss. Returns the formatted `ArcStr` (a refcount
    /// clone of the cached value) or `None` if no value is known.
    pub(super) fn formatted_for(&mut self, id: SubId) -> Option<ArcStr> {
        if let Some(s) = self.formatted.get(&id) {
            return Some(s.clone());
        }
        let v = self.values.get(&id)?;
        let s = format_value(v);
        self.formatted.insert(id, s.clone());
        Some(s)
    }

    /// Drop every active subscription and the routing/value/cell
    /// indexes that point at them. Sparklines are kept — the caller
    /// (`apply_table`) is replacing the table wholesale and would
    /// otherwise lose accumulated history that still applies if the
    /// new table includes any of the same `(row, col)` cells.
    fn clear_subs(&mut self) {
        self.dvals.clear();
        self.values.clear();
        self.formatted.clear();
        self.cells.clear();
        self.routing.clear();
    }
}

pub(super) struct SharedCells<X: GXExt> {
    pub(super) inner: Mutex<SharedCellsInner>,
    /// Set by the dispatch task whenever a grid cell value changes.
    /// `before_view` polls it to know whether to redraw.
    pub(super) dirty: AtomicBool,
    /// Set by the dispatch task whenever a sort-column value changes.
    /// `before_view` polls it to drive `resort_by_column`.
    pub(super) sort_col_dirty: AtomicBool,
    /// Runtime handle for firing `on_update` callbacks from the
    /// dispatch task. Cloned once from the widget at construction;
    /// immutable thereafter so no lock needed.
    pub(super) gx: GXHandle<X>,
}

impl<X: GXExt> SharedCells<X> {
    pub(super) fn new(gx: GXHandle<X>) -> Self {
        Self {
            inner: Mutex::new(SharedCellsInner::new()),
            dirty: AtomicBool::new(false),
            sort_col_dirty: AtomicBool::new(false),
            gx,
        }
    }
}

/// Single background task that processes every subscription update
/// for this widget. Eliminates the previous per-cell / per-sort-row
/// task explosion, amortizes lock costs across whole batches, and
/// lets netidx deliver larger batches because there is only one
/// consumer to backpressure. `Weak` so the task exits when the widget
/// drops — the `upgrade` at the top of each iteration is the shutdown
/// signal.
pub(super) fn spawn_dispatch_task<X: GXExt>(
    rt: &tokio::runtime::Handle,
    cells: &Arc<SharedCells<X>>,
    mut rx: mpsc::Receiver<GPooled<Vec<(SubId, Event)>>>,
) {
    let cells: Weak<SharedCells<X>> = Arc::downgrade(cells);
    rt.spawn(async move {
        use futures::StreamExt;
        // Reused scratch for callbacks-to-fire. Clearing (not
        // dropping) keeps capacity between batches.
        let mut callback_fires: LPooled<Vec<(ArcStr, Value)>> = LPooled::take();
        while let Some(mut batch) = rx.next().await {
            let Some(cells) = cells.upgrade() else { return };
            let now = Instant::now();
            let mut grid_dirty = false;
            let mut sort_dirty = false;
            {
                let mut inner = cells.inner.lock();
                let cb_id = inner.on_update;
                for (sub_id, event) in batch.drain(..) {
                    // Gate on `dvals` — it is the authoritative record of
                    // "this widget still owns a subscription with this id".
                    if !inner.dvals.contains_key(&sub_id) {
                        continue;
                    }
                    let v = match event {
                        Event::Update(v) => v,
                        Event::Unsubscribed => {
                            // Drop cached value + formatted text;
                            // render falls back to the column's
                            // Netidx placeholder. Roles fan-out
                            // marks grid_dirty / sort_dirty so the
                            // next view picks up the change.
                            inner.values.remove(&sub_id);
                            inner.formatted.remove(&sub_id);
                            if let Some(roles) = inner.routing.get(&sub_id).cloned() {
                                for role in roles.iter() {
                                    match role {
                                        SubRole::Grid { .. } => grid_dirty = true,
                                        SubRole::Sort => sort_dirty = true,
                                    }
                                }
                            }
                            continue;
                        }
                    };
                    inner.values.insert(sub_id, v.clone());
                    // Evict the formatted-display cache for this id —
                    // the next render will repopulate it. We don't
                    // format here because the cell may not be drawn
                    // before the next update overwrites it.
                    inner.formatted.remove(&sub_id);
                    let roles = match inner.routing.get(&sub_id) {
                        Some(roles) => roles.clone(),
                        None => continue,
                    };
                    for role in roles.iter() {
                        match role {
                            SubRole::Grid {
                                row_path,
                                col_name,
                                sparkline_history_secs,
                            } => {
                                grid_dirty = true;
                                if let Some(hs) = sparkline_history_secs {
                                    if let Some(f) = value_to_f64(&v) {
                                        let key = (row_path.clone(), col_name.clone());
                                        let history = inner
                                            .sparklines
                                            .entry(key)
                                            .or_insert_with(LPooled::take);
                                        history.push_back((now, f));
                                        let cutoff = now - Duration::from_secs_f64(*hs);
                                        while history
                                            .front()
                                            .map(|(t, _)| *t < cutoff)
                                            .unwrap_or(false)
                                        {
                                            history.pop_front();
                                        }
                                        if history.len() > MAX_SPARKLINE_POINTS {
                                            decimate_sparkline(history);
                                        }
                                    }
                                }
                                if cb_id.is_some() {
                                    let cell_path: ArcStr = if col_name == &VALUE_COL_KEY
                                    {
                                        row_path.clone().into()
                                    } else {
                                        format_compact!(
                                            "{}/{}",
                                            row_path.as_ref() as &str,
                                            col_name.as_str()
                                        )
                                        .as_str()
                                        .into()
                                    };
                                    callback_fires.push((cell_path, v.clone()));
                                }
                            }
                            SubRole::Sort => {
                                sort_dirty = true;
                            }
                        }
                    }
                }
            } // inner unlocked before firing callbacks and wake
            if grid_dirty {
                cells.dirty.store(true, Ordering::Relaxed);
            }
            if sort_dirty {
                cells.sort_col_dirty.store(true, Ordering::Relaxed);
            }
            // Snapshot callback id outside the lock — if it changed
            // between the two reads we'll fire the older id, which is
            // the same race as before the refactor (acceptable).
            let cb_id = cells.inner.lock().on_update;
            if let Some(cb_id) = cb_id {
                for (cell_path, v) in callback_fires.drain(..) {
                    let _ = cells
                        .gx
                        .call(cb_id, ValArray::from_iter([Value::String(cell_path), v]));
                }
            } else {
                callback_fires.clear();
            }
            if grid_dirty || sort_dirty {
                if let Some(w) = crate::REDRAW_WAKER.get() {
                    w.wake();
                }
            }
        }
    });
}

impl<X: GXExt> DataTableW<X> {
    /// Parse the table ref value and rebuild every column entry in
    /// place. `columns` is the single source of truth for column
    /// identity, order, type, and source — there is no separate
    /// `column_types` reconcile, no displayed-prefix invariant.
    ///
    /// Returns `Vec<ArcStr>` of column names whose refs/callables
    /// need to be compiled (always non-empty for newly-arrived
    /// columns, empty when the column set is unchanged). Callers
    /// invoke `compile_pending_columns` on the returned list only
    /// when it's non-empty — `Handle::block_on` from inside a tokio
    /// task panics on an immediately-ready future, so the empty case
    /// must stay on the sync path.
    pub(super) fn apply_table_sync(&mut self) -> LPooled<Vec<ArcStr>> {
        let mut pending: LPooled<Vec<ArcStr>> = LPooled::take();
        // Drop every existing subscription, route, value, and cell
        // index. Sparklines are kept so re-adding the same (row, col)
        // doesn't lose history.
        self.cells.inner.lock().clear_subs();
        self.row_paths.clear();
        // Reset column-width cache: the prior table's columns may not
        // appear in the new one, and any widths that *do* match a new
        // column name are stale until re-measured.
        self.cached_col_widths.lock().clear();
        // Re-read selection from graphix ref (don't clear user selection)
        self.selection =
            self.selection_ref.last.as_ref().map(parse_selection).unwrap_or_default();
        self.editing = None;
        self.first_row = 0;
        self.first_col = 0;
        let Some(table_val) = self.table_ref.last.as_ref().filter(|v| **v != Value::Null)
        else {
            // No table value — keep `columns` untouched; rendering
            // hits the empty-row early-return in `view()`.
            return pending;
        };
        // Table is { columns: Array<[string, ColumnSpec]>, rows: Array<string> }
        // Fields alphabetical: columns, rows.
        let (cols_val, rows_val) =
            match table_val.clone().cast_to::<[(ArcStr, Value); 2]>() {
                Ok([(_, cv), (_, rv)]) => (cv, rv),
                Err(_) => {
                    warn!("failed to parse table value");
                    return pending;
                }
            };
        let mut new_specs = parse_table_columns(&cols_val);
        let mut rows_raw: LPooled<Vec<Value>> =
            rows_val.cast_to::<LPooled<Vec<Value>>>().unwrap_or_default();
        // Convert row strings to Paths (for absolute netidx paths) or
        // keep as-is for virtual rows. `Path` is a newtype over
        // `ArcStr`, so map keys built from it clone via refcount bump.
        self.row_paths.extend(rows_raw.drain(..).filter_map(|v| match v {
            Value::String(s) => Some(Path::from(s)),
            _ => None,
        }));
        // Rebuild `columns` in user-supplied order, preserving
        // existing entries' compiled refs/callables when possible
        // (matched by name). New (never-seen) columns get fresh
        // `ColumnState`s; the loop below compiles their refs/callables
        // before returning.
        let mut existing: LPooled<AHashMap<ArcStr, ColumnState<X>>> =
            self.columns.drain(..).collect();
        for spec in new_specs.drain(..) {
            let entry = match existing.remove(&spec.name) {
                Some(mut prev) => {
                    // Reuse compiled state when the bid matches —
                    // otherwise drop it so the recompile path below
                    // installs fresh refs/callables.
                    let prev_cb = prev.spec.callback_value.as_ref().cloned();
                    if prev_cb != spec.callback_value {
                        prev.callback = None;
                    }
                    if prev.spec.source_bid != spec.source_bid {
                        prev.source = None;
                    }
                    if prev.spec.width_bid != spec.width_bid {
                        prev.width_ref = None;
                        prev.ref_width = None;
                    }
                    if prev.spec.on_resize_bid != spec.on_resize_bid {
                        prev.on_resize_ref = None;
                        prev.on_resize = None;
                    }
                    prev.spec = spec;
                    prev
                }
                None => ColumnState::new(spec),
            };
            self.columns.insert(entry.spec.name.clone(), entry);
        }
        // Identify columns that need compilation. Anything with an
        // un-resolved spec.bid for callback / source / width /
        // on_resize gets queued; columns whose state was reused from
        // the previous `apply_table` skip through.
        for (name, c) in self.columns.iter() {
            let needs = (c.callback.is_none() && c.spec.callback_value.is_some())
                || (c.source.is_none() && c.spec.source_bid != 0)
                || (c.width_ref.is_none() && c.spec.width_bid != 0)
                || (c.on_resize_ref.is_none() && c.spec.on_resize_bid != 0);
            if needs {
                pending.push(name.clone());
            }
        }
        self.mode = if self.columns.is_empty() && !self.row_paths.is_empty() {
            DisplayMode::Value
        } else {
            DisplayMode::Table
        };
        // Subscribe to every distinct column mentioned in `sort_by`,
        // so the row comparator has live cell values for each sort
        // key.
        self.cells.sort_col_dirty.store(false, Ordering::Relaxed);
        let mut sort_cols: LPooled<AHashSet<ArcStr>> = LPooled::take();
        sort_cols.extend(self.sort_by.iter().map(|s| s.column.clone()));
        for sort_col in sort_cols.iter() {
            self.subscribe_sort_column(sort_col);
        }
        // Source-based defaults are read at render time via
        // `source_value_for`; sparklines, by contrast, accumulate
        // history and want one synthetic data point per existing
        // value as the table reshapes.
        self.cells.dirty.store(false, Ordering::Relaxed);
        // Don't resort or seed sparklines here: per-column source
        // refs may still be pending compilation, and both paths
        // consult the source's parsed cache. Caller invokes
        // `resort_by_column` and `push_defaults_to_sparklines` after
        // `compile_pending_columns` returns.
        pending
    }

    /// Compile every column in `pending` (the list returned by
    /// `apply_table_sync`). Callers MUST skip this when `pending` is
    /// empty — `Handle::block_on` from inside a tokio task panics on
    /// an immediately-ready future, so the empty case must stay off
    /// the runtime.
    pub(super) async fn compile_pending_columns(
        &mut self,
        pending: LPooled<Vec<ArcStr>>,
    ) -> Result<()> {
        for name in pending.iter() {
            self.compile_column_refs(name).await?;
        }
        Ok(())
    }

    /// Compile any per-column `source` / `width` / `on_resize` ref
    /// and `on_edit` / `on_click` callable that hasn't been compiled
    /// yet. Idempotent: a column whose state was reused from the
    /// previous `apply_table` skips through with no work.
    async fn compile_column_refs(&mut self, name: &ArcStr) -> Result<()> {
        let (
            need_callback,
            cb_value,
            need_source,
            source_bid,
            need_width,
            width_bid,
            need_on_resize,
            on_resize_bid,
        ) = {
            let Some(c) = self.columns.get(name) else { return Ok(()) };
            (
                c.callback.is_none() && c.spec.callback_value.is_some(),
                c.spec.callback_value.clone(),
                c.source.is_none() && c.spec.source_bid != 0,
                c.spec.source_bid,
                c.width_ref.is_none() && c.spec.width_bid != 0,
                c.spec.width_bid,
                c.on_resize_ref.is_none() && c.spec.on_resize_bid != 0,
                c.spec.on_resize_bid,
            )
        };
        if need_callback {
            if let Some(v) = cb_value {
                if let Ok(cb) = self.gx.compile_callable(v).await {
                    if let Some(c) = self.columns.get_mut(name) {
                        c.callback = Some(cb);
                    }
                }
            }
        }
        if need_source {
            if let Ok(r) = self.gx.compile_ref(source_bid).await {
                if let Some(c) = self.columns.get_mut(name) {
                    c.source = Some(SourceEntry::new(r));
                }
            }
        }
        if need_width {
            if let Ok(r) = self.gx.compile_ref(width_bid).await {
                let w = r
                    .last
                    .as_ref()
                    .and_then(|v| v.clone().cast_to::<f64>().ok())
                    .map(|w| w as f32);
                if let Some(c) = self.columns.get_mut(name) {
                    c.ref_width = w;
                    c.width_ref = Some(r);
                }
            }
        }
        if need_on_resize {
            if let Ok(r) = self.gx.compile_ref(on_resize_bid).await {
                let cb = compile_callable_opt(&self.gx, &r).await.ok().flatten();
                if let Some(c) = self.columns.get_mut(name) {
                    c.on_resize = cb;
                    c.on_resize_ref = Some(r);
                }
            }
        }
        Ok(())
    }

    /// Apply a `sort_by` transition without tearing down every
    /// subscription. `old_cols` is the set of sort columns *before*
    /// `self.sort_by` was reassigned; the caller updates `self.sort_by`
    /// first and then invokes this. Subscribes any newly added sort
    /// columns and strips Sort roles from removed ones, dropping the
    /// underlying sub (and its `cells` entry) when no role remains.
    /// The previous behaviour — running `apply_table_sync` and thus
    /// `clear_subs` — wiped live Grid subs on every sort flip.
    pub(super) fn apply_sort_by_change(&mut self, old_cols: &AHashSet<ArcStr>) {
        let new_cols: LPooled<AHashSet<ArcStr>> =
            self.sort_by.iter().map(|s| s.column.clone()).collect();
        for col in new_cols.difference(old_cols) {
            self.subscribe_sort_column(col);
        }
        let mut inner = self.cells.inner.lock();
        let mut subs_to_drop: LPooled<Vec<SubId>> = LPooled::take();
        let mut cells_to_drop: LPooled<Vec<(Path, ArcStr)>> = LPooled::take();
        for col in old_cols.difference(&new_cols) {
            for row_path in self.row_paths.iter() {
                let key = (row_path.clone(), col.clone());
                let id = match inner.cells.get(&key).copied() {
                    Some(id) => id,
                    None => continue,
                };
                if let Some(roles) = inner.routing.get_mut(&id) {
                    roles.retain(|r| !matches!(r, SubRole::Sort));
                    // No Grid role for this (row, col) means the sub
                    // existed only to feed the sort comparator — drop
                    // its cells entry and dval. An on-screen row whose
                    // displayed column matches `col` will retain its
                    // Grid role and stay; an off-screen row's Grid was
                    // already stripped by `update_subscriptions`, so
                    // its only remaining role here is the Sort marker
                    // we just removed.
                    if roles.is_empty() {
                        subs_to_drop.push(id);
                        cells_to_drop.push(key);
                    }
                }
            }
        }
        for id in subs_to_drop.iter() {
            inner.routing.remove(id);
            inner.dvals.remove(id);
            inner.values.remove(id);
            inner.formatted.remove(id);
        }
        for key in cells_to_drop.iter() {
            inner.cells.remove(key);
        }
    }

    /// Subscribe every absolute row to `sort_col` via the shared
    /// channel and register routing entries. The dispatch task feeds
    /// `inner.values` as updates arrive.
    pub(super) fn subscribe_sort_column(&mut self, sort_col: &ArcStr) {
        let mut inner = self.cells.inner.lock();
        for row_path in self.row_paths.iter() {
            if !Path::is_absolute(row_path) {
                continue;
            }
            let dval = self.subscriber.subscribe(row_path.append(sort_col));
            let id = dval.id();
            // Push the role and register the dval before calling
            // `updates`: `BEGIN_WITH_LAST` can push a batch onto the
            // shared channel synchronously, and if the routing entry
            // isn't there yet the dispatch task drops the update.
            inner.routing.entry(id).or_insert_with(SubRoles::new).push(SubRole::Sort);
            inner.cells.insert((row_path.clone(), sort_col.clone()), id);
            // Only insert the dval if it isn't already owned —
            // overwriting would drop the previous Dval and cancel the
            // netidx subscription, but `subscribe()` may have returned
            // the same identity (netidx dedupes by path).
            inner.dvals.entry(id).or_insert_with(|| {
                dval.updates(UpdatesFlags::BEGIN_WITH_LAST, self.update_tx.clone());
                dval
            });
        }
    }

    /// Reconcile `cells.dvals` with the current visible window. Drops
    /// `Grid` roles for rows that scrolled out, drops the `Dval`
    /// itself when no role remains.
    pub(super) fn update_subscriptions(&mut self) {
        let (s, e) = self.subscription_row_range();
        let mut want: LPooled<AHashSet<Path>> = LPooled::take();
        want.extend((s..e).filter_map(|i| self.row_paths.get(i).cloned()));
        {
            let mut inner = self.cells.inner.lock();
            // Find SubIds that have a Grid role for a row outside the
            // visible window. Pooled scratch — number of subs stays in
            // the visible-window-range so allocations stay bounded.
            let mut to_strip: LPooled<Vec<SubId>> = LPooled::take();
            let mut cell_keys_to_drop: LPooled<Vec<(Path, ArcStr)>> = LPooled::take();
            for (id, roles) in inner.routing.iter() {
                let has_outside_grid = roles.iter().any(|r| match r {
                    SubRole::Grid { row_path, .. } => !want.contains(row_path),
                    _ => false,
                });
                if has_outside_grid {
                    to_strip.push(*id);
                }
            }
            for id in to_strip.iter() {
                if let Some(roles) = inner.routing.get_mut(id) {
                    // A SubId carries at most one Grid role (path/col
                    // identifies the sub uniquely), so remembering one
                    // dropped key is enough.
                    let mut dropped_grid: Option<(Path, ArcStr)> = None;
                    roles.retain(|r| match r {
                        SubRole::Grid { row_path, col_name, .. } => {
                            if !want.contains(row_path) {
                                dropped_grid = Some((row_path.clone(), col_name.clone()));
                                false
                            } else {
                                true
                            }
                        }
                        _ => true,
                    });
                    if let Some(key) = dropped_grid {
                        // Only drop the cells entry once nothing else
                        // needs it. A surviving Sort role keys back
                        // through (path, col) → SubId at sort time;
                        // dropping the entry here would silently break
                        // sort_value_for and leave off-screen rows
                        // comparing as "default-equal".
                        if roles.is_empty() {
                            cell_keys_to_drop.push(key);
                        }
                    }
                    if roles.is_empty() {
                        inner.routing.remove(id);
                        inner.dvals.remove(id);
                        inner.values.remove(id);
                        inner.formatted.remove(id);
                    }
                }
            }
            for key in cell_keys_to_drop.iter() {
                inner.cells.remove(key);
            }
        }
        // Spawn subs for visible rows that don't yet have a Grid role.
        // The cells map answers "is there already a Grid sub for this
        // (row, col)?" without scanning routing.
        let mut needs_subscribe: LPooled<Vec<Path>> = LPooled::take();
        {
            let inner = self.cells.inner.lock();
            for i in s..e {
                let Some(row_path) = self.row_paths.get(i) else { continue };
                if !Path::is_absolute(row_path) {
                    continue;
                }
                let already_subbed = match self.mode {
                    DisplayMode::Table => self
                        .displayed_columns()
                        .filter(|(_, state)| state.is_subscribed())
                        .all(|(c, _)| {
                            inner.cells.contains_key(&(row_path.clone(), c.clone()))
                        }),
                    DisplayMode::Value => inner
                        .cells
                        .contains_key(&(row_path.clone(), VALUE_COL_KEY.clone())),
                };
                if !already_subbed {
                    needs_subscribe.push(row_path.clone());
                }
            }
        }
        for row_path in needs_subscribe.iter() {
            self.subscribe_row(row_path);
        }
    }

    pub(super) fn subscribe_row(&mut self, row_path: &Path) {
        if !Path::is_absolute(row_path) {
            return;
        }
        let mut inner = self.cells.inner.lock();
        let tx = self.update_tx.clone();
        let subscribe_one =
            |inner: &mut SharedCellsInner,
             col_name: ArcStr,
             path: Path,
             sparkline_history_secs: Option<f64>| {
                let dval = self.subscriber.subscribe(path);
                let id = dval.id();
                inner.routing.entry(id).or_insert_with(SubRoles::new).push(
                    SubRole::Grid {
                        row_path: row_path.clone(),
                        col_name: col_name.clone(),
                        sparkline_history_secs,
                    },
                );
                inner.cells.insert((row_path.clone(), col_name), id);
                inner.dvals.entry(id).or_insert_with(|| {
                    dval.updates(UpdatesFlags::BEGIN_WITH_LAST, tx.clone());
                    dval
                });
            };
        match self.mode {
            DisplayMode::Table => {
                // Snapshot the displayed-column slice so the closure
                // can borrow `self` for `col_type_for` without
                // colliding with the in-flight `self.columns` iterator.
                let cols: LPooled<Vec<ArcStr>> =
                    self.displayed_columns().map(|(name, _)| name.clone()).collect();
                for col_name in cols.iter() {
                    let entry = self.columns.get(col_name);
                    if !entry.map(|e| e.is_subscribed()).unwrap_or(true) {
                        continue;
                    }
                    let sparkline_history_secs = match self.col_type_for(col_name) {
                        ColumnType::Sparkline { history_seconds, .. } => {
                            Some(*history_seconds)
                        }
                        _ => None,
                    };
                    let path = row_path.append(col_name);
                    subscribe_one(
                        &mut inner,
                        col_name.clone(),
                        path,
                        sparkline_history_secs,
                    );
                }
            }
            DisplayMode::Value => {
                let path = row_path.clone();
                subscribe_one(&mut inner, VALUE_COL_KEY.clone(), path, None);
            }
        }
    }

    /// Push current default values into sparkline histories for all
    /// sparkline columns. Called when column_types updates reactively
    /// to record the new values as data points.
    pub(super) fn push_defaults_to_sparklines(&self) {
        if self.mode != DisplayMode::Table {
            return;
        }
        let now = Instant::now();
        let mut inner = self.cells.inner.lock();
        for (col_name, _) in self.displayed_columns() {
            let history_secs = match self.col_type_for(col_name) {
                ColumnType::Sparkline { history_seconds, .. } => *history_seconds,
                _ => continue,
            };
            for row_path in self.row_paths.iter() {
                let row_name = row_basename(row_path);
                let f = match self.default_value_f64_for(col_name, row_name) {
                    Some(f) => f,
                    None => continue,
                };
                let key = (row_path.clone(), col_name.clone());
                let history = inner.sparklines.entry(key).or_insert_with(LPooled::take);
                history.push_back((now, f));
                let cutoff = now - Duration::from_secs_f64(history_secs);
                while history.front().map(|(t, _)| *t < cutoff).unwrap_or(false) {
                    history.pop_front();
                }
                if history.len() > MAX_SPARKLINE_POINTS {
                    decimate_sparkline(history);
                }
            }
        }
    }

    /// Sort key for `(row_idx, sort_col)`. Live subscription value
    /// wins; otherwise we fall back to the column's default value.
    /// Formats the raw `Value` to `ArcStr` only on read — we don't
    /// store formatted strings since the values map is consulted only
    /// during a re-sort, which is rare relative to subscription churn.
    pub(super) fn sort_value_for(&self, row_idx: usize, sort_col: &ArcStr) -> ArcStr {
        let row_path = &self.row_paths[row_idx];
        {
            let mut inner = self.cells.inner.lock();
            let id = inner.cells.get(&(row_path.clone(), sort_col.clone())).copied();
            if let Some(s) = id.and_then(|id| inner.formatted_for(id)) {
                return s;
            }
        }
        self.default_for(sort_col, row_basename(row_path))
    }

    /// Re-sort row_paths by sort column values. The cell index is
    /// keyed by (Path, col), so reordering row_paths is enough — no
    /// reindex and no subscription churn. Called when sort_col_dirty
    /// is set or during apply_table when sort_by is non-empty.
    pub(super) fn resort_by_column(&mut self) {
        if self.sort_by.is_empty() {
            return;
        }
        let n = self.row_paths.len();
        let n_keys = self.sort_by.len();
        let mut keys: LPooled<Vec<ArcStr>> = LPooled::take();
        keys.reserve(n * n_keys);
        for i in 0..n {
            for sb in self.sort_by.iter() {
                keys.push(self.sort_value_for(i, &sb.column));
            }
        }
        let mut indices: LPooled<Vec<usize>> = (0..n).collect();
        indices.sort_by(|&a, &b| {
            for (idx, sb) in self.sort_by.iter().enumerate() {
                let va = keys[a * n_keys + idx].as_str();
                let vb = keys[b * n_keys + idx].as_str();
                let cmp = match (numeric_key(va), numeric_key(vb)) {
                    (Some(na), Some(nb)) => {
                        na.partial_cmp(&nb).unwrap_or(std::cmp::Ordering::Equal)
                    }
                    _ => va.cmp(vb),
                };
                if cmp != std::cmp::Ordering::Equal {
                    return match sb.direction {
                        SortDirection::Ascending => cmp,
                        SortDirection::Descending => cmp.reverse(),
                    };
                }
            }
            std::cmp::Ordering::Equal
        });
        let mut old_paths: LPooled<Vec<Path>> = LPooled::take();
        old_paths.extend(self.row_paths.iter().cloned());
        for (new_i, &old_i) in indices.iter().enumerate() {
            self.row_paths[new_i] = old_paths[old_i].clone();
        }
        self.cells.dirty.store(true, Ordering::Relaxed);
    }
}
