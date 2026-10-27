//! Subscription dispatch for the data table: cell and sort subscriptions,
//! table application, and sorting.
//!
//! `SharedCells` owns every netidx subscription this widget creates.
//! The dispatch task stores raw `Value`s keyed by `SubId`; the render
//! path looks cells up through `(Path, col) → SubId` and formats only
//! what is drawn.

use super::{
    DataTableW, DisplayMode, ROW_NAME_KEY, VALUE_COL_KEY,
    types::{
        ColumnState, ColumnType, SortDirection, SourceEntry, cell_path, format_value,
        numeric_key, parse_selection, parse_table_columns, push_sparkline_point,
        row_basename, value_to_f64,
    },
};
use ahash::{AHashMap, AHashSet};

use arcstr::ArcStr;
use futures::channel::mpsc;
use graphix_compiler::BindId;
use graphix_package_core::ExactNum;
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
    fn is_grid(&self, row: &Path, col: &str, history: Option<f64>) -> bool {
        matches!(self, SubRole::Grid { row_path, col_name, sparkline_history_secs } if row_path == row && col_name == col && *sparkline_history_secs == history)
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
    // CR claude for claude: [style/low] These histories are taken on tokio workers
    // (deliver_to_grid in the dispatch task) and dropped on the GUI thread
    // (update_subscriptions' retain): the asymmetric cross-thread case the pooling rule
    // gives to GPooled. As LPooled, the workers always allocate and the GUI thread's
    // pool fills with deques it never reuses. Use GPooled here. Found by reading; not
    // run. (gui-datatable-07)
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

    fn remove_grid(&mut self, row: &Path, col: &ArcStr) {
        let key = (row.clone(), col.clone());
        let Some(id) = self.cells.get(&key).copied() else { return };
        let Some(roles) = self.routing.get_mut(&id) else { return };
        roles.retain(|role| !matches!(role, SubRole::Grid { row_path, col_name, .. } if row_path == row && col_name == col));
        let keeps_cell = roles.iter().any(|role| match role {
            SubRole::Grid { row_path, col_name, .. }
            | SubRole::SortMarker { row_path, col_name } => {
                row_path == row && col_name == col
            }
        });
        if !keeps_cell {
            self.cells.remove(&key);
        }
        if roles.is_empty() {
            self.routing.remove(&id);
            self.dvals.remove(&id);
            self.values.remove(&id);
            self.formatted.remove(&id);
        }
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
                            SubRole::Grid { .. } => {
                                grid_dirty = true;
                                if let Some(v) = &v {
                                    deliver_to_grid(
                                        &cells.gx,
                                        &mut inner.sparklines,
                                        inner.on_update,
                                        role,
                                        v,
                                        now,
                                    )
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

/// Deliver `v` to the cell `role` shows in the grid: a point on its
/// sparkline, then `on_update`, whose call refuses a value its `#value:
/// Primitive` does not hold (a map).
fn deliver_to_grid<X: GXExt>(
    gx: &GXHandle<X>,
    sparklines: &mut AHashMap<(Path, ArcStr), LPooled<VecDeque<(Instant, f64)>>>,
    on_update: Option<CallableId>,
    role: &SubRole,
    v: &Value,
    now: Instant,
) {
    let SubRole::Grid { row_path, col_name, sparkline_history_secs } = role else {
        return;
    };
    if let Some(hs) = sparkline_history_secs
        && let Some(f) = value_to_f64(v)
    {
        let h = sparklines
            .entry((row_path.clone(), col_name.clone()))
            .or_insert_with(LPooled::take);
        push_sparkline_point(h, now, f, *hs);
    }
    if let Some(cid) = on_update {
        let path = Value::String(cell_path(row_path, col_name));
        let _ = gx.call(cid, ValArray::from_iter([path, v.clone()]));
    }
}

/// A row's sort key in one column: numbers before text, numbers
/// exactly ([`ExactNum`]), a duration or a datetime as its nanoseconds,
/// text by its bytes.
#[derive(PartialEq)]
enum SortKey {
    Num(ExactNum),
    Text(ArcStr),
}

impl SortKey {
    fn of_value(v: &Value) -> Self {
        match v {
            Value::Duration(v) => Self::Num(ExactNum::Int(v.as_nanos() as i128)),
            Value::DateTime(v) => Self::Num(ExactNum::Int(
                v.timestamp() as i128 * 1_000_000_000
                    + v.timestamp_subsec_nanos() as i128,
            )),
            Value::String(s) => {
                match ExactNum::parse(s).or_else(|| numeric_key(s).map(ExactNum::Float)) {
                    Some(n) => Self::Num(n),
                    None => Self::Text(s.clone()),
                }
            }
            v => match ExactNum::of(v) {
                Some(n) => Self::Num(n),
                None => Self::Text(format_value(v)),
            },
        }
    }

    fn cmp(&self, other: &Self) -> CmpOrdering {
        match (self, other) {
            (Self::Num(a), Self::Num(b)) => a.cmp(b),
            (Self::Num(_), Self::Text(_)) => CmpOrdering::Less,
            (Self::Text(_), Self::Num(_)) => CmpOrdering::Greater,
            (Self::Text(a), Self::Text(b)) => a.cmp(b),
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
        self.subscriptions_changed = true;
        self.selection =
            self.selection_ref.last.as_ref().map(parse_selection).unwrap_or_default();
        #[derive(FromValue)]
        #[value(shape = "graphix_compiler::rep::GraphixShape")]
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
        self.cached_col_widths.lock().retain(|name, _| {
            name == &ROW_NAME_KEY
                || name == &VALUE_COL_KEY
                || self.columns.contains_key(name)
        });
        for (name, c) in self.columns.iter() {
            let needs = (c.callback.is_none() && c.spec.callback_value.is_some())
                || (c.source.is_none() && c.spec.source_bid.is_some())
                || (c.width_ref.is_none() && c.spec.width_bid.is_some())
                || (c.on_resize_ref.is_none() && c.spec.on_resize_bid.is_some());
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
        let bid = |missing: bool, bid: Option<BindId>| bid.filter(|_| missing);
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
                        && let Err(e) = gx.update_callable(&mut f, v).await
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
        let full = std::mem::take(&mut self.subscriptions_changed);
        let mut sort_cols: LPooled<AHashSet<ArcStr>> = LPooled::take();
        if full {
            sort_cols.extend(
                self.sort_by
                    .iter()
                    .map(|s| s.column.clone())
                    .filter(|c| self.subscribes(c)),
            );
        }
        let mut shown: LPooled<AHashMap<ArcStr, Option<f64>>> = LPooled::take();
        match self.mode {
            DisplayMode::Table => {
                // CR claude for eric: [perf/medium] Every netidx column of every row in
                // the window is subscribed: `shown` holds all columns, and
                // handle_scroll reconciles only when the row window moves. Only rows
                // are virtualized for subscriptions. A 500-column table in a 40-row
                // view holds about 65k subscriptions while about 300 cells are drawn,
                // and a scrollbar jump re-subscribes all of them. Either window `shown`
                // to display_col_range plus a column buffer (and reconcile when
                // first_col changes in handle_scroll), or rule that whole rows stay
                // subscribed so on_update stays complete per row, and say so in the
                // book. Found by reading; not run. (gui-datatable-01)
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
        if full {
            inner.routing.retain(|_, roles| {
                roles.retain(|r| match &*r {
                    SubRole::Grid { row_path, col_name, .. } => {
                        window.contains(row_path)
                            && shown
                                .get(col_name)
                                .is_some_and(|hs| r.is_grid(row_path, col_name, *hs))
                    }
                    SubRole::SortMarker { row_path, col_name } => {
                        self.row_indices.contains_key(row_path)
                            && sort_cols.contains(col_name)
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
                self.row_indices.contains_key(row)
                    && columns.get(col).is_some_and(|c| {
                        matches!(c.spec.typ, ColumnType::Sparkline { .. })
                    })
            });
        } else {
            for row in self.subscription_window.difference(&window) {
                for col in shown.keys() {
                    inner.remove_grid(row, col);
                }
            }
        }
        for row in window.iter().filter(|p| {
            Path::is_absolute(p) && (full || !self.subscription_window.contains(*p))
        }) {
            for (col, hs) in shown.iter() {
                if !inner.has_role(row, col, |r| r.is_grid(row, col, *hs)) {
                    let path = Path::from(cell_path(row, col));
                    let role = SubRole::Grid {
                        row_path: row.clone(),
                        col_name: col.clone(),
                        sparkline_history_secs: *hs,
                    };
                    self.subscribe_cell(inner, row, col, path, role);
                }
            }
        }
        if full {
            for row in self.table_rows.iter().filter(|p| Path::is_absolute(p)) {
                for col in sort_cols.iter() {
                    if !inner.has_role(row, col, |r| r.is_sort_of(row, col)) {
                        let role = SubRole::SortMarker {
                            row_path: row.clone(),
                            col_name: col.clone(),
                        };
                        self.subscribe_cell(
                            inner,
                            row,
                            col,
                            Path::from(cell_path(row, col)),
                            role,
                        );
                    }
                }
            }
        }
        self.subscription_window = window;
    }

    /// Subscribe `path` for the cell `(row, col)` in `role`; a role the
    /// subscription already plays (a row listed twice) is not added again.
    /// A new subscription delivers its current value when it arrives; a
    /// role new to a standing one is delivered the value it holds.
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
        let added = !roles.contains(&role);
        if added {
            roles.push(role.clone());
        }
        inner.cells.insert((row.clone(), col.clone()), id);
        match inner.dvals.entry(id) {
            std::collections::hash_map::Entry::Vacant(e) => {
                dval.updates(UpdatesFlags::BEGIN_WITH_LAST, self.update_tx.clone());
                e.insert(dval);
            }
            std::collections::hash_map::Entry::Occupied(_) => {
                if added && let Some(v) = inner.values.get(&id) {
                    deliver_to_grid(
                        &self.cells.gx,
                        &mut inner.sparklines,
                        inner.on_update,
                        &role,
                        v,
                        Instant::now(),
                    )
                }
            }
        }
    }

    /// Push each sparkline column's fallback into the history of every
    /// cell that never subscribes: a virtual row's, or a stored source's. A
    /// subscribed cell's history holds only its own values.
    /// Give the sparkline cells their defaults' points: every cell of
    /// `fired`, the column whose source just gave a value, and a first
    /// point to any other cell that has none yet.
    // CR claude for claude: [readability/low] Two doc paragraphs are stacked above this
    // function; the first is an older wording of the second. Merge them into one: cells
    // that never subscribe get their fallback as a point, every such cell of `fired`,
    // and a first point for any other cell with no history. Found by reading; not run.
    // (gui-datatable-06)
    pub(super) fn push_defaults_to_sparklines(&self, fired: Option<&ArcStr>) {
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
                if fired != Some(col_name) && inner.sparklines.contains_key(&key) {
                    continue;
                }
                let history = inner.sparklines.entry(key).or_insert_with(LPooled::take);
                push_sparkline_point(history, now, f, history_seconds);
            }
        }
    }

    fn index_rows(&mut self) {
        self.row_indices.clear();
        for (i, row) in self.row_paths.iter().enumerate() {
            self.row_indices.entry(row.clone()).or_insert(i);
        }
        self.selection.reindex(&self.row_indices, self.mode, &self.columns);
    }

    /// The rows in display order: the table's own order sorted, stably,
    /// by the sort columns, or the table's order when there are none.
    pub(super) fn sort_rows(&mut self) {
        self.row_paths.clear();
        if self.sort_by.is_empty() {
            self.row_paths.extend(self.table_rows.iter().cloned());
            self.index_rows();
            self.cells.dirty.store(true, Ordering::Relaxed);
            return;
        }
        let n_keys = self.sort_by.len();
        let mut keys: LPooled<Vec<SortKey>> = LPooled::take();
        let inner = self.cells.inner.lock();
        for row in self.table_rows.iter() {
            for sb in self.sort_by.iter() {
                let value =
                    self.raw_cell_value(&inner, row, &sb.column).unwrap_or(&Value::Null);
                keys.push(SortKey::of_value(value));
            }
        }
        drop(inner);
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
        self.index_rows();
        self.cells.dirty.store(true, Ordering::Relaxed);
    }
}

#[cfg(test)]
mod sort_tests {
    use super::*;
    use std::time::Duration;

    #[test]
    fn raw_numeric_sort_keys_preserve_precision_and_units() {
        let less = |a, b| {
            assert_eq!(
                SortKey::of_value(&a).cmp(&SortKey::of_value(&b)),
                CmpOrdering::Less,
                "{a:?} < {b:?}"
            )
        };
        less(Value::U64((1 << 53) + 1), Value::U64((1 << 53) + 2));
        less(Value::U64(1 << 53), Value::U64((1 << 53) + 1));
        less(Value::I64(-((1 << 53) + 1)), Value::I64(-(1 << 53)));
        less(
            Value::from(Duration::from_millis(500)),
            Value::from(Duration::from_secs(2)),
        );
        less(Value::from(Duration::from_secs(2)), Value::from(Duration::from_secs(60)));
        less(Value::F64(f64::NAN), Value::F64(f64::NEG_INFINITY));
        less(Value::F64(f64::NEG_INFINITY), Value::I64(i64::MIN));
        less(Value::U64(u64::MAX), Value::F64(f64::INFINITY));
        less(Value::F64(-0.5), Value::I64(0));
        less(Value::I64(0), Value::F64(0.5));
        less(Value::from("Alice"), Value::from("Nan"));
        less(Value::from("Bob"), Value::from("inf"));
        less(Value::from("Bob"), Value::from("NaN"));
        less(Value::from("2"), Value::from("10"));
        let dec = |s: &str| Value::Decimal(triomphe::Arc::new(s.parse().unwrap()));
        less(dec("2.0"), dec("9.5"));
        less(dec("9.5"), dec("10.25"));
        less(dec("10.25"), Value::I64(11));
        less(Value::I64(10), dec("10.25"));
        less(dec("79228162514264337593543950334"), dec("79228162514264337593543950335"));
        less(dec("10.25"), Value::from("Alice"));
        less(dec("0.1"), dec("0.10000000000000000001"));
        less(Value::from("0.1"), Value::from("0.10000000000000000001"));
        less(dec("0.1000000000000000055511151231"), Value::F64(0.1));
        less(Value::F64(0.1), dec("0.1000000000000000055511151232"));
    }

    #[test]
    fn numeric_words_stay_text() {
        for word in
            ["Nan", "nan", "NaN", "inf", "Inf", "Infinity", "+inf", "-inf", "1e999"]
        {
            assert_eq!(numeric_key(word), None, "{word}");
        }
        assert_eq!(numeric_key("1.25"), Some(1.25));
    }
}

#[cfg(test)]
mod order_tests {
    use super::*;

    #[test]
    fn mixed_sort_keys_are_a_total_order() {
        let values = [
            Value::F64(f64::NAN),
            Value::F64(f64::NEG_INFINITY),
            Value::F64(-1e100),
            Value::I64(i64::MIN),
            Value::F64(-0.5),
            Value::F64(-0.0),
            Value::I64(0),
            Value::F64(0.0),
            Value::F64(0.5),
            Value::U64(1 << 53),
            Value::F64((1_u64 << 53) as f64),
            Value::U64((1 << 53) + 1),
            Value::I64(i64::MAX),
            Value::U64(u64::MAX),
            Value::F64(u64::MAX as f64),
            Value::F64(1e100),
            Value::F64(f64::INFINITY),
            Value::from("Nan"),
            Value::from("inf"),
            Value::Decimal(triomphe::Arc::new("0.5".parse().unwrap())),
            Value::Decimal(triomphe::Arc::new("0.1".parse().unwrap())),
            Value::F64(0.1),
            Value::Decimal(triomphe::Arc::new(
                "0.1000000000000000055511151231".parse().unwrap(),
            )),
            Value::from("0.10000000000000000001"),
            Value::Decimal(triomphe::Arc::new("-3".parse().unwrap())),
            Value::Decimal(triomphe::Arc::new(
                "79228162514264337593543950335".parse().unwrap(),
            )),
        ];
        let keys: LPooled<Vec<_>> = values.iter().map(SortKey::of_value).collect();
        for a in &*keys {
            for b in &*keys {
                assert_eq!(a.cmp(b), b.cmp(a).reverse());
                for c in &*keys {
                    if a.cmp(b).is_le() && b.cmp(c).is_le() {
                        assert!(a.cmp(c).is_le());
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod role_tests {
    use super::*;
    use arcstr::literal;

    #[test]
    fn grid_roles_include_the_history_window() {
        let row = Path::from("/r0");
        for history in [None, Some(60.0), Some(120.0)] {
            let role = SubRole::Grid {
                row_path: row.clone(),
                col_name: literal!("load"),
                sparkline_history_secs: history,
            };
            for desired in [None, Some(60.0), Some(120.0)] {
                assert_eq!(role.is_grid(&row, "load", desired), history == desired);
            }
        }
    }
}

#[cfg(test)]
mod window_tests {
    use super::*;

    #[test]
    fn remove_grid_keeps_sort_values_and_other_cells() {
        let mut inner = SharedCellsInner::new();
        let row = Path::from("/r");
        let col = ArcStr::from("c");
        let other = ArcStr::from("other");
        let id = SubId::new();
        let other_id = SubId::new();
        let grid = SubRole::Grid {
            row_path: row.clone(),
            col_name: col.clone(),
            sparkline_history_secs: None,
        };
        let sort = SubRole::SortMarker { row_path: row.clone(), col_name: col.clone() };
        inner.routing.insert(id, SubRoles::from_iter([grid.clone(), sort.clone()]));
        inner.cells.insert((row.clone(), col.clone()), id);
        inner.values.insert(id, Value::I64(12));
        inner.formatted.insert(id, ArcStr::from("12"));
        inner.cells.insert((row.clone(), other.clone()), other_id);
        inner.routing.insert(
            other_id,
            SubRoles::from_iter([SubRole::Grid {
                row_path: row.clone(),
                col_name: other.clone(),
                sparkline_history_secs: None,
            }]),
        );
        inner.values.insert(other_id, Value::I64(42));
        inner.remove_grid(&row, &col);
        assert_eq!(inner.live_value(&row, &col), Some(&Value::I64(12)));
        assert!(inner.routing[&id].as_slice() == [sort]);
        assert_eq!(inner.formatted_for(id).as_deref(), Some("12"));
        assert_eq!(inner.live_value(&row, &other), Some(&Value::I64(42)));
        inner.routing.insert(id, SubRoles::from_iter([grid]));
        inner.remove_grid(&row, &col);
        assert!(!inner.cells.contains_key(&(row.clone(), col)));
        assert!(!inner.values.contains_key(&id));
        assert!(!inner.formatted.contains_key(&id));
        assert!(!inner.routing.contains_key(&id));
        assert_eq!(inner.live_value(&row, &other), Some(&Value::I64(42)));
    }
}
