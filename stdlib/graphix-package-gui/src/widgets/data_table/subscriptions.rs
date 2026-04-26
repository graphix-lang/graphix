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
    decimate_sparkline, format_value, numeric_key, parse_selection, row_basename,
    value_to_f64, ColumnRecompile, ColumnSpec, ColumnState, ColumnType,
    DefaultValueEntry, SortDirection,
};
use super::{
    compile_callable_opt, DataTableW, DisplayMode, MAX_SPARKLINE_POINTS, VALUE_COL_KEY,
};
use anyhow::Result;
use arcstr::ArcStr;
use compact_str::{format_compact, CompactString};
use futures::channel::mpsc;
use fxhash::{FxHashMap, FxHashSet};
use graphix_rt::{CallableId, GXExt, GXHandle};
use log::warn;
use netidx::{
    path::Path,
    protocol::valarray::ValArray,
    publisher::Value,
    subscriber::{Dval, Event, SubId, UpdatesFlags},
};
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
    pub(super) dvals: FxHashMap<SubId, Dval>,
    /// Most recent `Value` from each subscription. Stored raw — the
    /// dispatch task writes the netidx `Value` as-is and we format to
    /// `ArcStr` only at draw / sort time. A 100 Hz cell scrolled out
    /// of view costs one `Value::clone` per update (refcount bump),
    /// not one ArcStr allocation.
    pub(super) values: FxHashMap<SubId, Value>,
    /// Lazy `ArcStr` cache for display. Populated at render time when
    /// a cell is first formatted; the dispatch task evicts an entry
    /// whenever its `Value` changes. So a stable cell allocates one
    /// ArcStr the first time it is drawn and reuses it forever; a
    /// fast-updating off-screen cell never allocates because its
    /// invalidations are never followed by a render.
    pub(super) formatted: FxHashMap<SubId, ArcStr>,
    /// `(row_path, col_name)` → `SubId`. Cell index used by the render
    /// path to find the value for a given cell. Sort columns share
    /// `SubId`s with grid cells whenever the paths coincide (netidx
    /// dedupes `subscribe` by path), so an entry exists for every
    /// `(Path, col)` we have a live subscription on.
    pub(super) cells: FxHashMap<(Path, ArcStr), SubId>,
    /// `SubId` → roles played by that subscription. Multi-role because
    /// a cell that is also sorted on registers two roles against the
    /// same id; the dispatch task fans every update out to each role.
    pub(super) routing: FxHashMap<SubId, SubRoles>,
    /// Sparkline history: `(row_path, col_name)` → timestamped values.
    /// Identity-keyed so history survives row reordering. `LPooled`
    /// returns `VecDeque`s to the thread-local pool when sparkline
    /// cells churn (sort/scroll evicts and re-creates entries).
    pub(super) sparklines: FxHashMap<(Path, ArcStr), LPooled<VecDeque<(Instant, f64)>>>,
    /// Latest `on_update` callable id (if any). Written by the widget
    /// when the `on_update` ref resolves; read by the dispatch task at
    /// the start of each batch.
    pub(super) on_update: Option<CallableId>,
}

impl SharedCellsInner {
    fn new() -> Self {
        Self {
            dvals: FxHashMap::default(),
            values: FxHashMap::default(),
            formatted: FxHashMap::default(),
            cells: FxHashMap::default(),
            routing: FxHashMap::default(),
            sparklines: FxHashMap::default(),
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
                    let Event::Update(v) = event else { continue };
                    // Gate on `dvals` — it is the authoritative record of
                    // "this widget still owns a subscription with this id".
                    if !inner.dvals.contains_key(&sub_id) {
                        continue;
                    }
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

/// True when two `ColumnType`s agree on every field that is baked into
/// a `SubRole::Grid` (specifically sparkline history), AND on the
/// variant kind. A mismatch means existing subscription roles carry
/// stale metadata (e.g. Text→Sparkline: roles have
/// `sparkline_history_secs = None` but the new column wants Some) and
/// the full `apply_table` reconcile has to re-register them.
fn column_type_role_eq(a: &ColumnType, b: &ColumnType) -> bool {
    match (a, b) {
        (
            ColumnType::Sparkline { history_seconds: ha, .. },
            ColumnType::Sparkline { history_seconds: hb, .. },
        ) => ha == hb,
        _ => std::mem::discriminant(a) == std::mem::discriminant(b),
    }
}

impl<X: GXExt> DataTableW<X> {
    /// Sync half of the `column_types` reconcile: computes whether
    /// the change is structural, drops per-column state for removed
    /// columns, and records a per-column recompile list for any
    /// callable or ref whose source changed. No `.await`, so it's
    /// safe to call from a sync trait method (e.g. `handle_update`)
    /// without arranging for a non-async call stack.
    ///
    /// The returned recompile list is `.await`-consumed by
    /// `apply_column_recompile`. When the list is empty (e.g. only
    /// `display_name` changed) callers must skip the async phase
    /// entirely — `Handle::block_on` from inside a tokio task would
    /// panic even on a trivially-ready future.
    pub(super) fn apply_column_types_diff(
        &mut self,
        mut new_types: FxHashMap<ArcStr, ColumnSpec>,
    ) -> (bool, LPooled<Vec<ColumnRecompile>>) {
        // Structural = column set changed, a column flipped between
        // virtual and non-virtual (default_value_bid crossed 0/non-0),
        // or a column's `ColumnType` kind / sparkline history changed
        // such that its subscription role metadata is now wrong.
        // Anything else (display_name, callback, width, on_resize
        // swap) reconciles in place without an `apply_table` call.
        let old_spec_count =
            self.columns.values().filter(|c| c.spec.is_some()).count();
        let keys_same = old_spec_count == new_types.len()
            && self
                .columns
                .iter()
                .filter(|(_, c)| c.spec.is_some())
                .all(|(k, _)| new_types.contains_key(k));
        let virtuals_same = keys_same
            && new_types.iter().all(|(name, spec)| {
                self.columns
                    .get(name)
                    .and_then(|c| c.spec.as_ref())
                    .map(|old| {
                        (old.default_value_bid != 0) == (spec.default_value_bid != 0)
                    })
                    .unwrap_or(false)
            });
        let kinds_same = keys_same
            && new_types.iter().all(|(name, spec)| {
                self.columns
                    .get(name)
                    .and_then(|c| c.spec.as_ref())
                    .map(|old| column_type_role_eq(&old.typ, &spec.typ))
                    .unwrap_or(false)
            });
        let structural_change = !keys_same || !virtuals_same || !kinds_same;
        // Clear spec + compiled bits for columns whose spec is gone.
        // `user_widths` is intentionally kept: a re-added column reuses
        // the user's drag preference. Whether the entry stays in
        // `columns` depends on display state — `apply_table` rebuilds
        // the displayed prefix, so non-displayed entries with no spec
        // would be dead weight; drop those here. Displayed entries
        // (raw netidx columns) keep their slot with a now-empty spec.
        let mut to_drop: LPooled<Vec<ArcStr>> = LPooled::take();
        for (name, c) in self.columns.iter_mut() {
            if c.spec.is_some() && !new_types.contains_key(name) {
                c.clear_spec();
            }
        }
        // Second pass: drop entries that no longer have any reason to
        // exist (spec gone AND not in displayed prefix). Iterate in
        // reverse so swap_remove on tail entries doesn't disturb
        // displayed-prefix indices.
        for i in (0..self.columns.len()).rev() {
            if i < self.displayed_count {
                continue;
            }
            let dead = self
                .columns
                .get_index(i)
                .map(|(_, c)| c.spec.is_none())
                .unwrap_or(false);
            if dead {
                let key = self.columns.get_index(i).map(|(k, _)| k.clone()).unwrap();
                to_drop.push(key);
            }
        }
        for k in to_drop.iter() {
            self.columns.swap_remove(k);
        }
        // Install or update specs. New (never-seen) columns get
        // appended past the displayed prefix; `apply_table` will
        // promote them into display order if structural_change fires.
        let mut recompile: LPooled<Vec<ColumnRecompile>> = LPooled::take();
        for (name, spec) in new_types.drain() {
            let old_spec = self.columns.get(&name).and_then(|c| c.spec.as_ref());
            let cb_changed =
                old_spec.and_then(|s| s.callback_value.as_ref()) != spec.callback_value.as_ref();
            let on_resize_changed =
                old_spec.map(|s| s.on_resize_bid).unwrap_or(0) != spec.on_resize_bid;
            let width_changed =
                old_spec.map(|s| s.width_bid).unwrap_or(0) != spec.width_bid;
            let dv_changed =
                old_spec.map(|s| s.default_value_bid).unwrap_or(0) != spec.default_value_bid;
            // Queue any recompiles BEFORE moving `spec` into the entry.
            if cb_changed {
                if let Some(cb_val) = spec.callback_value.clone() {
                    recompile.push(ColumnRecompile::Callback {
                        name: name.clone(),
                        value: cb_val,
                    });
                }
            }
            if on_resize_changed && spec.on_resize_bid != 0 {
                recompile.push(ColumnRecompile::OnResize {
                    name: name.clone(),
                    bid: spec.on_resize_bid,
                });
            }
            if width_changed && spec.width_bid != 0 {
                recompile.push(ColumnRecompile::Width {
                    name: name.clone(),
                    bid: spec.width_bid,
                });
            }
            if dv_changed && spec.default_value_bid != 0 {
                recompile.push(ColumnRecompile::DefaultValue {
                    name: name.clone(),
                    bid: spec.default_value_bid,
                });
            }
            let entry = self.columns.entry(name).or_insert_with(ColumnState::default);
            if cb_changed {
                entry.callback = None;
            }
            if on_resize_changed {
                entry.on_resize = None;
                entry.on_resize_ref = None;
            }
            if width_changed {
                entry.width_ref = None;
                entry.ref_width = None;
            }
            if dv_changed {
                entry.default_value = None;
            }
            entry.spec = Some(spec);
        }
        (structural_change, recompile)
    }

    /// Async half of the reconcile: compile every entry in
    /// `recompile` (produced by `apply_column_types_diff`) and
    /// install the resulting callables/refs into the column's
    /// `ColumnState`.
    pub(super) async fn apply_column_recompile(
        &mut self,
        recompile: LPooled<Vec<ColumnRecompile>>,
    ) -> Result<()> {
        for item in recompile.iter() {
            match item {
                ColumnRecompile::Callback { name, value } => {
                    if let Ok(c) = self.gx.compile_callable(value.clone()).await {
                        if let Some(entry) = self.columns.get_mut(name) {
                            entry.callback = Some(c);
                        }
                    }
                }
                ColumnRecompile::OnResize { name, bid } => {
                    if let Ok(r) = self.gx.compile_ref(*bid).await {
                        let cb = compile_callable_opt(&self.gx, &r).await.ok().flatten();
                        if let Some(entry) = self.columns.get_mut(name) {
                            entry.on_resize = cb;
                            entry.on_resize_ref = Some(r);
                        }
                    }
                }
                ColumnRecompile::Width { name, bid } => {
                    if let Ok(r) = self.gx.compile_ref(*bid).await {
                        let w = r
                            .last
                            .as_ref()
                            .and_then(|v| v.clone().cast_to::<f64>().ok())
                            .map(|w| w as f32);
                        if let Some(entry) = self.columns.get_mut(name) {
                            entry.ref_width = w;
                            entry.width_ref = Some(r);
                        }
                    }
                }
                ColumnRecompile::DefaultValue { name, bid } => {
                    if let Ok(r) = self.gx.compile_ref(*bid).await {
                        if let Some(entry) = self.columns.get_mut(name) {
                            entry.default_value = Some(DefaultValueEntry::new(r));
                        }
                    }
                }
            }
        }
        Ok(())
    }

    /// Parse the table ref value and rebuild the table structure.
    /// No resolver call — the user passes the table directly.
    pub(super) fn apply_table(&mut self) {
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
        let table_val = match self.table_ref.last.as_ref() {
            Some(v) if *v != Value::Null => v.clone(),
            _ => return,
        };
        // Table is { rows: Array<string>, columns: Array<string> }
        // Fields alphabetical: columns, rows.
        //
        // Null bytes in incoming column names are stripped to prevent
        // collisions with `ROW_NAME_KEY` (which has a leading `\0`)
        // and similar internal sentinel keys used by width caches.
        let (raw_cols, raw_rows) = match table_val.cast_to::<[(ArcStr, Value); 2]>() {
            Ok([(_, cols_val), (_, rows_val)]) => {
                let mut cols_raw: LPooled<Vec<Value>> =
                    cols_val.cast_to::<LPooled<Vec<Value>>>().unwrap_or_default();
                let cols: LPooled<Vec<ArcStr>> = cols_raw
                    .drain(..)
                    .filter_map(|v| match v {
                        Value::String(s) if s.contains('\0') => Some({
                            let cleaned: CompactString =
                                s.chars().filter(|c| *c != '\0').collect();
                            cleaned.as_str().into()
                        }),
                        Value::String(s) => Some(s),
                        _ => None,
                    })
                    .collect();
                let mut rows_raw: LPooled<Vec<Value>> =
                    rows_val.cast_to::<LPooled<Vec<Value>>>().unwrap_or_default();
                let rows: LPooled<Vec<ArcStr>> = rows_raw
                    .drain(..)
                    .filter_map(|v| match v {
                        Value::String(s) => Some(s),
                        _ => None,
                    })
                    .collect();
                (cols, rows)
            }
            Err(_) => {
                warn!("failed to parse table value");
                return;
            }
        };
        // Convert row strings to Paths (for absolute netidx paths)
        // or keep as-is for virtual rows. `Path` is a newtype over
        // `ArcStr`, so map keys built from it clone via refcount bump.
        self.row_paths.extend(raw_rows.iter().map(|s| Path::from(s.clone())));
        // Rebuild the displayed prefix of `columns` so its order
        // matches: raw_cols (in netidx order), then spec'd virtuals
        // with a non-null default. Spec-only columns whose default is
        // null sit past the displayed boundary so their refs stay
        // alive without occupying a display slot. Take everything
        // out, then re-insert in the desired order, preserving the
        // existing `ColumnState` payload (spec, refs, callables) when
        // a column is already known.
        let mut existing: FxHashMap<ArcStr, ColumnState<X>> =
            self.columns.drain(..).collect();
        self.displayed_count = 0;
        for raw in raw_cols.iter() {
            let mut entry = existing.remove(raw).unwrap_or_default();
            entry.virtual_col = false;
            self.columns.insert(raw.clone(), entry);
            self.displayed_count += 1;
        }
        // Snapshot virtual candidates before mutating `existing`, so
        // we can drain in a stable order without juggling iterator
        // invalidation.
        let mut virtuals: LPooled<Vec<ArcStr>> = LPooled::take();
        virtuals.extend(existing.iter().filter_map(|(name, state)| {
            let has_default = state
                .default_value
                .as_ref()
                .map(|d| d.parsed.is_present())
                .unwrap_or(false);
            (state.spec.is_some() && has_default).then(|| name.clone())
        }));
        for name in virtuals.drain(..) {
            let mut entry = existing.remove(&name).unwrap();
            entry.virtual_col = true;
            self.columns.insert(name, entry);
            self.displayed_count += 1;
        }
        // Anything still left in `existing` is spec-only without a
        // current default — preserve it past the displayed boundary.
        for (name, mut entry) in existing.drain() {
            entry.virtual_col = false;
            self.columns.insert(name, entry);
        }
        self.mode = if self.displayed_count == 0 && !self.row_paths.is_empty() {
            DisplayMode::Value
        } else {
            DisplayMode::Table
        };
        // Subscribe to every distinct column mentioned in `sort_by`, so
        // the row comparator has live cell values for each sort key.
        self.cells.sort_col_dirty.store(false, Ordering::Relaxed);
        let mut sort_cols: LPooled<FxHashSet<ArcStr>> = LPooled::take();
        sort_cols.extend(self.sort_by.iter().map(|s| s.column.clone()));
        for sort_col in sort_cols.iter() {
            self.subscribe_sort_column(sort_col);
        }
        // Default values are computed at render time via `default_for`,
        // so we don't need to seed any value entries here.
        self.push_defaults_to_sparklines();
        self.cells.dirty.store(false, Ordering::Relaxed);
        if !self.sort_by.is_empty() {
            self.resort_by_column();
        }
    }

    /// Re-reconcile the displayed prefix of `columns` after a
    /// `default_value` ref transitions null↔non-null. `apply_table`
    /// is the only other place this logic lives, but full rebuild
    /// would drop every live subscription — we only want to flip
    /// whether a default-only column is visible. Returns whether any
    /// visible column set changed so the caller can propagate `dirty`.
    ///
    /// Columns inside the displayed prefix but with `virtual_col=false`
    /// are "real" columns from the last `apply_table`; those we leave
    /// alone even if their default presence changed.
    pub(super) fn reconcile_virtual_cols(&mut self) -> bool {
        let mut changed = false;
        let names: LPooled<Vec<ArcStr>> = self
            .columns
            .iter()
            .filter(|(_, c)| c.spec.is_some())
            .map(|(name, _)| name.clone())
            .collect();
        for name in names.iter() {
            let pos = match self.columns.get_index_of(name) {
                Some(p) => p,
                None => continue,
            };
            let displayed = pos < self.displayed_count;
            let entry = &self.columns[pos];
            let virtual_col = entry.virtual_col;
            let has_default = entry
                .default_value
                .as_ref()
                .map(|d| d.parsed.is_present())
                .unwrap_or(false);
            if displayed && !virtual_col {
                continue;
            }
            if has_default && !displayed {
                // Promote: move past the displayed boundary, mark
                // virtual, grow displayed_count.
                self.columns.move_index(pos, self.displayed_count);
                self.columns[self.displayed_count].virtual_col = true;
                self.displayed_count += 1;
                changed = true;
            } else if !has_default && virtual_col {
                // Demote: shrink the displayed prefix, then move the
                // entry to the just-vacated boundary slot.
                self.displayed_count -= 1;
                self.columns.move_index(pos, self.displayed_count);
                self.columns[self.displayed_count].virtual_col = false;
                changed = true;
            }
        }
        if changed {
            self.mode = if self.displayed_count == 0 && !self.row_paths.is_empty() {
                DisplayMode::Value
            } else {
                DisplayMode::Table
            };
        }
        changed
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
        let mut want: LPooled<FxHashSet<Path>> = LPooled::take();
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
                    roles.retain(|r| match r {
                        SubRole::Grid { row_path, col_name, .. } => {
                            if !want.contains(row_path) {
                                cell_keys_to_drop
                                    .push((row_path.clone(), col_name.clone()));
                                false
                            } else {
                                true
                            }
                        }
                        _ => true,
                    });
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
                        .filter(|(_, state)| !state.virtual_col)
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
                    if entry.map(|e| e.virtual_col).unwrap_or(false) {
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
