//! Subscription dispatch for the data table: cell and sort subscriptions,
//! table application, and sorting.
//!
//! `SharedCells` owns every netidx subscription this widget creates.
//! The dispatch task stores raw `Value`s keyed by `SubId`; the render
//! path looks cells up through `(Path, col) → SubId` and formats only
//! what is drawn.

use super::{
    DataTableW, DisplayMode, VALUE_COL_KEY,
    types::{
        ColumnState, ColumnType, SortDirection, SourceEntry, cell_path, format_value,
        numeric_key, parse_selection, parse_table_columns, push_sparkline_point,
        row_basename, value_to_f64,
    },
};
use ahash::{AHashMap, AHashSet};

use arcstr::ArcStr;
use futures::channel::mpsc;
use graphix_rt::{CallableId, GXExt, GXHandle};
use log::warn;
use netidx::{
    path::Path,
    protocol::valarray::ValArray,
    publisher::Value,
    subscriber::{Dval, Event, SubId, UpdatesFlags},
};
use netidx_derive::FromValue;
use nohash::IntMap;
use parking_lot::Mutex;
use poolshark::{global::GPooled, local::LPooled};
use smallvec::SmallVec;
use std::{
    cmp::Ordering as CmpOrdering,
    collections::VecDeque,
    sync::{
        Arc, Weak,
        atomic::{AtomicBool, Ordering},
    },
    time::Instant,
};

/// One role a subscription plays. netidx dedupes subscriptions by
/// path, so one `SubId` plays several roles when a cell is both
/// displayed and listed in `sort_by`.
#[derive(Clone, PartialEq)]
pub(super) enum SubRole {
    /// A cell shown. `col_name` is `VALUE_COL_KEY` for
    /// `DisplayMode::Value`; `sparkline_history_secs` is set for
    /// sparkline columns.
    Grid { row_path: Path, col_name: ArcStr, sparkline_history_secs: Option<f64> },
    /// A cell sorted by: the dispatch task sets `sort_col_dirty` on its
    /// every update. The key itself is read through the `cells` index.
    SortMarker { row_path: Path, col_name: ArcStr },
}

impl SubRole {
    fn is_grid_of(&self, row: &Path, col: &str) -> bool {
        matches!(self, SubRole::Grid { row_path, col_name, .. } if row_path == row && col_name == col)
    }

    fn is_sort_of(&self, row: &Path, col: &str) -> bool {
        matches!(self, SubRole::SortMarker { row_path, col_name } if row_path == row && col_name == col)
    }
}

/// All roles for a single `SubId`.
pub(super) type SubRoles = SmallVec<[SubRole; 2]>;

/// State of `SharedCells`, locked once per dispatch batch.
pub(super) struct SharedCellsInner {
    /// Owns every subscription; dropping a `Dval` cancels it.
    pub(super) dvals: IntMap<SubId, Dval>,
    /// Most recent raw `Value` from each subscription; formatted only
    /// at draw / sort time.
    pub(super) values: IntMap<SubId, Value>,
    /// Display-string cache, filled at render time and evicted by the
    /// dispatch task when the value changes.
    pub(super) formatted: IntMap<SubId, ArcStr>,
    /// `(row_path, col_name)` → `SubId`, one entry per live
    /// subscription.
    pub(super) cells: AHashMap<(Path, ArcStr), SubId>,
    /// Roles played by each subscription; every update fans out to
    /// each role.
    pub(super) routing: IntMap<SubId, SubRoles>,
    /// Sparkline history per `(row_path, col_name)`; keyed by identity
    /// so it survives row reordering.
    pub(super) sparklines: AHashMap<(Path, ArcStr), LPooled<VecDeque<(Instant, f64)>>>,
    /// Latest `on_update` callable id, read by the dispatch task at
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

    /// The display string for `id`, cached; `None` if no value is
    /// known.
    pub(super) fn formatted_for(&mut self, id: SubId) -> Option<ArcStr> {
        if let Some(s) = self.formatted.get(&id) {
            return Some(s.clone());
        }
        let v = self.values.get(&id)?;
        let s = format_value(v);
        self.formatted.insert(id, s.clone());
        Some(s)
    }

    /// Whether the cell `(row, col)` has a role `has`.
    fn has_role(&self, row: &Path, col: &ArcStr, has: impl Fn(&SubRole) -> bool) -> bool {
        self.cells
            .get(&(row.clone(), col.clone()))
            .and_then(|id| self.routing.get(id))
            .is_some_and(|roles| roles.iter().any(has))
    }

    /// The cell's live value, if it has one.
    pub(super) fn live_value(&self, row: &Path, col: &ArcStr) -> Option<&Value> {
        let id = self.cells.get(&(row.clone(), col.clone()))?;
        self.values.get(id)
    }
}

pub(super) struct SharedCells<X: GXExt> {
    pub(super) inner: Mutex<SharedCellsInner>,
    /// Set by the dispatch task when a grid cell changes; polled by
    /// `before_view`.
    pub(super) dirty: AtomicBool,
    /// Set by the dispatch task when a sort-column value changes;
    /// polled by `before_view`.
    pub(super) sort_col_dirty: AtomicBool,
    /// Runtime handle for firing `on_update` from the dispatch task.
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

/// The one background task processing every subscription update for
/// this widget. Holds the cells weakly and exits when the widget
/// drops. `on_update` is called under the lock, so once the widget
/// swaps the callable no call reaches the one it retires.
pub(super) fn spawn_dispatch_task<X: GXExt>(
    rt: &tokio::runtime::Handle,
    cells: &Arc<SharedCells<X>>,
    mut rx: mpsc::Receiver<GPooled<Vec<(SubId, Event)>>>,
) {
    let cells: Weak<SharedCells<X>> = Arc::downgrade(cells);
    rt.spawn(async move {
        use futures::StreamExt;
        while let Some(mut batch) = rx.next().await {
            let Some(cells) = cells.upgrade() else { return };
            let now = Instant::now();
            let mut grid_dirty = false;
            let mut sort_dirty = false;
            {
                let mut inner = cells.inner.lock();
                let inner = &mut *inner;
                for (sub_id, event) in batch.drain(..) {
                    if !inner.dvals.contains_key(&sub_id) {
                        continue;
                    }
                    let v = match event {
                        Event::Update(v) => Some(v),
                        Event::Unsubscribed => None,
                    };
                    match &v {
                        Some(v) => inner.values.insert(sub_id, v.clone()),
                        None => inner.values.remove(&sub_id),
                    };
                    inner.formatted.remove(&sub_id);
                    let Some(roles) = inner.routing.get(&sub_id) else { continue };
                    for role in roles.iter() {
                        match role {
                            SubRole::SortMarker { .. } => sort_dirty = true,
                            SubRole::Grid {
                                row_path,
                                col_name,
                                sparkline_history_secs,
                            } => {
                                grid_dirty = true;
                                let Some(v) = &v else { continue };
                                if let Some(hs) = sparkline_history_secs
                                    && let Some(f) = value_to_f64(v)
                                {
                                    let key = (row_path.clone(), col_name.clone());
                                    let h = inner
                                        .sparklines
                                        .entry(key)
                                        .or_insert_with(LPooled::take);
                                    push_sparkline_point(h, now, f, *hs);
                                }
                                if let Some(cid) = inner.on_update {
                                    let path =
                                        Value::String(cell_path(row_path, col_name));
                                    let _ = cells.gx.call(
                                        cid,
                                        ValArray::from_iter([path, v.clone()]),
                                    );
                                }
                            }
                        }
                    }
                }
            }
            if grid_dirty {
                cells.dirty.store(true, Ordering::Relaxed);
            }
            if sort_dirty {
                cells.sort_col_dirty.store(true, Ordering::Relaxed);
            }
            if (grid_dirty || sort_dirty)
                && let Some(w) = crate::REDRAW_WAKER.get()
            {
                w.wake();
            }
        }
    });
}

/// A row's sort key in one column: numbers before text, numbers in
/// Graphix's float order (NaN below every number), text by its bytes.
#[derive(PartialEq)]
enum SortKey {
    Num(f64),
    Text(ArcStr),
}

impl SortKey {
    fn of(s: ArcStr) -> Self {
        match numeric_key(&s) {
            Some(n) => SortKey::Num(n),
            None => SortKey::Text(s),
        }
    }

    fn cmp(&self, other: &Self) -> CmpOrdering {
        match (self, other) {
            (SortKey::Num(a), SortKey::Num(b)) => match (a.is_nan(), b.is_nan()) {
                (true, true) => CmpOrdering::Equal,
                (true, false) => CmpOrdering::Less,
                (false, true) => CmpOrdering::Greater,
                (false, false) => a.total_cmp(b),
            },
            (SortKey::Num(_), SortKey::Text(_)) => CmpOrdering::Less,
            (SortKey::Text(_), SortKey::Num(_)) => CmpOrdering::Greater,
            (SortKey::Text(a), SortKey::Text(b)) => a.cmp(b),
        }
    }
}

impl<X: GXExt> DataTableW<X> {
    /// Parse the table ref value and rebuild the rows and columns in
    /// place; the subscriptions are reconciled after, by
    /// `update_subscriptions`. The view, the selection and an edit stay
    /// where they are while their rows and columns do.
    ///
    /// Returns the names of columns whose refs/callables still need
    /// compiling.
    pub(super) fn apply_table_sync(&mut self) -> LPooled<Vec<ArcStr>> {
        let mut pending: LPooled<Vec<ArcStr>> = LPooled::take();
        self.table_rows.clear();
        self.cached_col_widths.lock().clear();
        self.selection =
            self.selection_ref.last.as_ref().map(parse_selection).unwrap_or_default();
        #[derive(FromValue)]
        struct Table {
            columns: Value,
            rows: Value,
        }
        let table = self
            .table_ref
            .last
            .as_ref()
            .filter(|v| **v != Value::Null)
            .and_then(|v| v.clone().cast_to::<Table>().ok());
        let (columns, rows) = match table {
            Some(Table { columns, rows }) => (columns, rows),
            None => {
                if self.table_ref.last.as_ref().is_some_and(|v| *v != Value::Null) {
                    warn!("failed to parse table value");
                }
                (Value::Null, Value::Null)
            }
        };
        let mut new_specs = parse_table_columns(&columns);
        let mut rows_raw: LPooled<Vec<Value>> =
            rows.cast_to::<LPooled<Vec<Value>>>().unwrap_or_default();
        self.table_rows.extend(rows_raw.drain(..).filter_map(|v| match v {
            Value::String(s) => Some(Path::from(s)),
            _ => None,
        }));
        let mut existing: LPooled<AHashMap<ArcStr, ColumnState<X>>> =
            self.columns.drain(..).collect();
        for spec in new_specs.drain(..) {
            let entry = match existing.remove(&spec.name) {
                Some(mut prev) => {
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
        for (name, c) in self.columns.iter() {
            let needs = (c.callback.is_none() && c.spec.callback_value.is_some())
                || (c.source.is_none() && c.spec.source_bid != 0)
                || (c.width_ref.is_none() && c.spec.width_bid != 0)
                || (c.on_resize_ref.is_none() && c.spec.on_resize_bid != 0);
            if needs {
                pending.push(name.clone());
            }
        }
        self.mode = if self.columns.is_empty() && !self.table_rows.is_empty() {
            DisplayMode::Value
        } else {
            DisplayMode::Table
        };
        self.sort_rows();
        // an edit stays open while its cell exists
        if let Some((row, col)) = &self.editing
            && (!self.table_rows.contains(row) || !self.columns.contains_key(col))
        {
            self.editing = None;
        }
        let first = (self.first_row, self.first_col);
        self.first_row = self.first_row.min(self.row_paths.len().saturating_sub(1));
        self.first_col = self.first_col.min(self.total_data_cols().saturating_sub(1));
        if first != (self.first_row, self.first_col) {
            self.scroll_dirty = true;
        }
        self.cells.sort_col_dirty.store(false, Ordering::Relaxed);
        self.cells.dirty.store(false, Ordering::Relaxed);
        pending
    }

    /// Compile every column returned by `apply_table_sync`. Callers skip it
    /// when `pending` is empty: their `Handle::block_on` panics inside a
    /// runtime context (a test's runtime), whatever the future.
    pub(super) async fn compile_pending_columns(
        &mut self,
        pending: LPooled<Vec<ArcStr>>,
    ) {
        for name in pending.iter() {
            self.compile_column_refs(name).await;
        }
    }

    /// Compile the column's refs and callables not compiled yet. A part
    /// that fails is logged and left out: the column shows without it.
    async fn compile_column_refs(&mut self, name: &ArcStr) {
        let Some(c) = self.columns.get(name) else { return };
        let spec = &c.spec;
        let callback = spec.callback_value.clone().filter(|_| c.callback.is_none());
        let bid = |missing: bool, bid: u64| (missing && bid != 0).then_some(bid);
        let source = bid(c.source.is_none(), spec.source_bid);
        let width = bid(c.width_ref.is_none(), spec.width_bid);
        let on_resize = bid(c.on_resize_ref.is_none(), spec.on_resize_bid);
        let gx = self.gx.clone();
        let fail = |what: &str, e: anyhow::Error| {
            warn!("data_table column {name}: {what}: {e:#}");
        };
        if let Some(v) = callback {
            match gx.compile_callable(v).await {
                Ok(f) => self.columns[name].callback = Some(f),
                Err(e) => fail("callback", e),
            }
        }
        if let Some(bid) = source {
            match gx.compile_ref(bid).await {
                Ok(r) => self.columns[name].source = Some(SourceEntry::new(r)),
                Err(e) => fail("source", e),
            }
        }
        if let Some(bid) = width {
            match gx.compile_ref(bid).await {
                Ok(r) => {
                    let c = &mut self.columns[name];
                    c.ref_width = r
                        .last
                        .as_ref()
                        .and_then(|v| v.clone().cast_to::<f64>().ok())
                        .map(|w| w as f32);
                    c.width_ref = Some(r);
                }
                Err(e) => fail("width", e),
            }
        }
        if let Some(bid) = on_resize {
            match gx.compile_ref(bid).await {
                Ok(r) => {
                    let mut f = None;
                    if let Some(v) = r.last.as_ref()
                        && let Err(e) = crate::widgets::set_callable(&gx, &mut f, v).await
                    {
                        fail("on_resize", e)
                    }
                    let c = &mut self.columns[name];
                    c.on_resize = f;
                    c.on_resize_ref = Some(r);
                }
                Err(e) => fail("on_resize", e),
            }
        }
    }

    /// Whether the column's cells come from netidx: a column the table
    /// lacks, which only `sort_by` can name, is read from netidx too.
    fn subscribes(&self, col: &str) -> bool {
        self.columns.get(col).is_none_or(|c| c.is_subscribed())
    }

    /// Reconcile the subscriptions with what the table shows and sorts
    /// by: drop every role nothing wants (a cell outside the window, a
    /// column gone or no longer from netidx, a sort column dropped),
    /// then subscribe what is missing. Sparkline histories of rows and
    /// columns gone are dropped.
    pub(super) fn update_subscriptions(&mut self) {
        let (s, e) = self.subscription_row_range();
        let mut window: LPooled<AHashSet<Path>> = LPooled::take();
        window.extend(self.row_paths[s.min(e)..e].iter().cloned());
        let mut rows: LPooled<AHashSet<Path>> = LPooled::take();
        rows.extend(self.table_rows.iter().cloned());
        let mut sort_cols: LPooled<AHashSet<ArcStr>> = LPooled::take();
        sort_cols.extend(
            self.sort_by.iter().map(|s| s.column.clone()).filter(|c| self.subscribes(c)),
        );
        let mut shown: LPooled<AHashMap<ArcStr, Option<f64>>> = LPooled::take();
        match self.mode {
            DisplayMode::Table => {
                for (name, c) in self.columns.iter().filter(|(_, c)| c.is_subscribed()) {
                    let hs = match &c.spec.typ {
                        ColumnType::Sparkline { history_seconds, .. } => {
                            Some(*history_seconds)
                        }
                        _ => None,
                    };
                    shown.insert(name.clone(), hs);
                }
            }
            DisplayMode::Value => {
                shown.insert(VALUE_COL_KEY.clone(), None);
            }
        }
        let mut inner = self.cells.inner.lock();
        let inner = &mut *inner;
        inner.routing.retain(|_, roles| {
            roles.retain(|r| match r {
                SubRole::Grid { row_path, col_name, .. } => {
                    window.contains(row_path) && shown.contains_key(col_name)
                }
                SubRole::SortMarker { row_path, col_name } => {
                    rows.contains(row_path) && sort_cols.contains(col_name)
                }
            });
            !roles.is_empty()
        });
        let routing = &inner.routing;
        inner.dvals.retain(|id, _| routing.contains_key(id));
        inner.values.retain(|id, _| routing.contains_key(id));
        inner.formatted.retain(|id, _| routing.contains_key(id));
        inner.cells.retain(|_, id| routing.contains_key(id));
        let columns = &self.columns;
        inner.sparklines.retain(|(row, col), _| {
            rows.contains(row)
                && columns
                    .get(col)
                    .is_some_and(|c| matches!(c.spec.typ, ColumnType::Sparkline { .. }))
        });
        for row in self.row_paths[s.min(e)..e].iter().filter(|p| Path::is_absolute(p)) {
            for (col, hs) in shown.iter() {
                if !inner.has_role(row, col, |r| r.is_grid_of(row, col)) {
                    let path =
                        if col == &VALUE_COL_KEY { row.clone() } else { row.append(col) };
                    let role = SubRole::Grid {
                        row_path: row.clone(),
                        col_name: col.clone(),
                        sparkline_history_secs: *hs,
                    };
                    self.subscribe_cell(inner, row, col, path, role);
                }
            }
        }
        for row in self.table_rows.iter().filter(|p| Path::is_absolute(p)) {
            for col in sort_cols.iter() {
                if !inner.has_role(row, col, |r| r.is_sort_of(row, col)) {
                    let role = SubRole::SortMarker {
                        row_path: row.clone(),
                        col_name: col.clone(),
                    };
                    self.subscribe_cell(inner, row, col, row.append(col), role);
                }
            }
        }
    }

    /// Subscribe `path` for the cell `(row, col)` in `role`; a role the
    /// subscription already plays (a row listed twice) is not added again.
    fn subscribe_cell(
        &self,
        inner: &mut SharedCellsInner,
        row: &Path,
        col: &ArcStr,
        path: Path,
        role: SubRole,
    ) {
        let dval = self.subscriber.subscribe(path);
        let id = dval.id();
        let roles = inner.routing.entry(id).or_insert_with(SubRoles::new);
        if !roles.contains(&role) {
            roles.push(role);
        }
        inner.cells.insert((row.clone(), col.clone()), id);
        inner.dvals.entry(id).or_insert_with(|| {
            dval.updates(UpdatesFlags::BEGIN_WITH_LAST, self.update_tx.clone());
            dval
        });
    }

    /// Push each sparkline column's fallback into the history of every
    /// cell that never subscribes: a virtual row's, or a stored source's. A
    /// subscribed cell's history holds only its own values.
    pub(super) fn push_defaults_to_sparklines(&self) {
        if self.mode != DisplayMode::Table {
            return;
        }
        let now = Instant::now();
        let mut inner = self.cells.inner.lock();
        for (col_name, c) in self.columns.iter() {
            let ColumnType::Sparkline { history_seconds, .. } = c.spec.typ else {
                continue;
            };
            for row_path in self.table_rows.iter() {
                if c.is_subscribed() && Path::is_absolute(row_path) {
                    continue;
                }
                let Some(f) =
                    self.default_value_f64_for(col_name, row_basename(row_path))
                else {
                    continue;
                };
                let key = (row_path.clone(), col_name.clone());
                let history = inner.sparklines.entry(key).or_insert_with(LPooled::take);
                push_sparkline_point(history, now, f, history_seconds);
            }
        }
    }

    /// Sort key text for `(row_path, sort_col)`: the live subscription
    /// value, else the column's default.
    pub(super) fn sort_value_for(&self, row_path: &Path, sort_col: &ArcStr) -> ArcStr {
        {
            let mut inner = self.cells.inner.lock();
            let id = inner.cells.get(&(row_path.clone(), sort_col.clone())).copied();
            if let Some(s) = id.and_then(|id| inner.formatted_for(id)) {
                return s;
            }
        }
        self.default_for(sort_col, row_basename(row_path))
    }

    /// The rows in display order: the table's own order sorted, stably,
    /// by the sort columns, or the table's order when there are none.
    pub(super) fn sort_rows(&mut self) {
        self.row_paths.clear();
        if self.sort_by.is_empty() {
            self.row_paths.extend(self.table_rows.iter().cloned());
            self.cells.dirty.store(true, Ordering::Relaxed);
            return;
        }
        let n_keys = self.sort_by.len();
        let mut keys: LPooled<Vec<SortKey>> = LPooled::take();
        for row in self.table_rows.iter() {
            for sb in self.sort_by.iter() {
                keys.push(SortKey::of(self.sort_value_for(row, &sb.column)));
            }
        }
        let mut order: LPooled<Vec<usize>> = (0..self.table_rows.len()).collect();
        order.sort_by(|&a, &b| {
            for (k, sb) in self.sort_by.iter().enumerate() {
                let cmp = keys[a * n_keys + k].cmp(&keys[b * n_keys + k]);
                if cmp != CmpOrdering::Equal {
                    return match sb.direction {
                        SortDirection::Ascending => cmp,
                        SortDirection::Descending => cmp.reverse(),
                    };
                }
            }
            CmpOrdering::Equal
        });
        self.row_paths.extend(order.iter().map(|&i| self.table_rows[i].clone()));
        self.cells.dirty.store(true, Ordering::Relaxed);
    }
}
