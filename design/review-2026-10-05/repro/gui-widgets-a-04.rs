//! gui-widgets-a-04: child/children refs rebuild the whole subtree on every
//! fire, even identical ones (widgets/mod.rs update_child! and the flex
//! children arm, grid.rs, stack.rs, window.rs window_ref/content_ref).
//!
//! The GUI rebuild needs no display through this harness (no window, no
//! GPU: widgets are compiled and driven through `handle_update` and
//! `on_message` as the event loop does). To run, copy this file to
//! stdlib/graphix-package-gui/tests/review_gui_widgets_a_04.rs, then:
//!   timeout -s KILL 2400 cargo test -p graphix-package-gui \
//!     --test review_gui_widgets_a_04 -- --nocapture
//!
//! Each test types "abc" into a text_editor whose `doc` starts as "hello"
//! (the cursor follows the typing: "ahello", "abhello", "abchello"), makes
//! the `children` ref of the editor's column fire, then types "Y".
//! Expected: the editor survives the fire and the text is "abcYhello".
//! Observed at c722befe (all three tests fail):
//!   identical: Home on Home -> children fired 1x, identical=true
//!     `instances` before the click [I64(2), I64(3), I64(4)], written by
//!     the click [I64(5), I64(6)]   (a fresh go_home call site: each site's
//!     creation moves it by 2, at startup and for the test's own site too)
//!     editor survived=false; typing Y gives "Yabchello"
//!   sibling: editor element unchanged=true, editor survived=false
//!     typing Y gives "Yabchello"
//!   append: 20 rows -> 21 editors; 0 of 20 old editors survived; ~237 expr
//!     ids minted by the append
use ahash::AHashMap;
use anyhow::{Context, Result, bail};
use arcstr::ArcStr;
use graphix_compiler::{
    BindId,
    env::Env,
    expr::{ExprId, VfsEntry, VfsResolver},
};
use graphix_package_core::testing::{self, TestCtx};
use graphix_package_gui::widgets::{self, GuiW, Message, MessageShell};
use graphix_rt::{Callable, CallableId, CompRes, GXEvent, GXHandle, NoExt, Ref};
use iced_widget::text_editor::{Action, Edit};
use netidx::{path::Path, protocol::valarray::ValArray, publisher::Value};
use poolshark::global::GPooled;
use std::time::Duration;
use tokio::sync::mpsc;

const REGISTER: &[&dyn graphix_package::Package<NoExt>] = &[
    &graphix_package_core::P,
    &graphix_package_array::P,
    &graphix_package_map::P,
    &graphix_package_str::P,
    &graphix_package_sys::P,
    &graphix_package_gui::P,
];

struct H {
    _ctx: TestCtx,
    gx: GXHandle<NoExt>,
    compiled: CompRes<NoExt>,
    rx: mpsc::Receiver<GPooled<Vec<GXEvent>>>,
    widget: GuiW<NoExt>,
    rt: tokio::runtime::Handle,
    watched: Vec<(ExprId, Vec<Value>)>,
    refs: Vec<Ref<NoExt>>,
    callables: Vec<Callable<NoExt>>,
}

fn find_bind_id(env: &Env, name: &str) -> Result<BindId> {
    let (module, var) = name.split_once("::").context("module::var")?;
    let suffix = format!("/{module}");
    for (scope, vars) in &env.binds {
        if Path::as_ref(&scope.0).ends_with(&suffix) {
            if let Some(bid) = vars.get(var) {
                return Ok(*bid);
            }
        }
    }
    bail!("no binding {name}")
}

impl H {
    async fn new(code: &str) -> Result<Self> {
        let (tx, mut rx) = mpsc::channel(100);
        let tbl = AHashMap::from_iter([(
            Path::from("/test.gx"),
            VfsEntry::from(ArcStr::from(code)),
        )]);
        let ctx =
            testing::init_with_resolvers(tx, REGISTER, vec![VfsResolver::new(tbl)])
                .await?;
        let gx = ctx.rt.clone();
        let compiled = gx.compile(arcstr::literal!("{ mod test; test::result }")).await?;
        let root = compiled.exprs[0].id;
        let initial = loop {
            let mut batch = tokio::time::timeout(Duration::from_secs(10), rx.recv())
                .await?
                .context("runtime closed")?;
            let hit = batch.drain(..).find_map(|e| match e {
                GXEvent::Updated(id, v) if id == root => Some(v),
                _ => None,
            });
            if let Some(v) = hit {
                break v;
            }
        };
        let widget = widgets::compile(gx.clone(), initial).await?;
        Ok(Self {
            _ctx: ctx,
            gx,
            compiled,
            rx,
            widget,
            rt: tokio::runtime::Handle::current(),
            watched: vec![],
            refs: vec![],
            callables: vec![],
        })
    }

    async fn drain(&mut self) -> Result<()> {
        let timeout = tokio::time::sleep(Duration::from_millis(300));
        tokio::pin!(timeout);
        loop {
            tokio::select! {
                biased;
                Some(mut batch) = self.rx.recv() => {
                    for e in batch.drain(..) {
                        if let GXEvent::Updated(id, v) = e {
                            for (wid, hist) in self.watched.iter_mut() {
                                if *wid == id {
                                    hist.push(v.clone());
                                }
                            }
                            let rt = self.rt.clone();
                            let w = &mut self.widget;
                            tokio::task::block_in_place(|| w.handle_update(&rt, id, &v))?;
                        }
                    }
                    timeout.as_mut().reset(
                        tokio::time::Instant::now() + Duration::from_millis(150),
                    );
                }
                _ = &mut timeout => break,
            }
        }
        Ok(())
    }

    async fn watch(&mut self, name: &str) -> Result<usize> {
        let bid = find_bind_id(&self.compiled.env, name)?;
        let r = self.gx.compile_ref(bid).await?;
        self.watched.push((r.id, r.last.iter().cloned().collect()));
        self.refs.push(r);
        Ok(self.watched.len() - 1)
    }

    fn hist(&self, w: usize) -> &[Value] {
        &self.watched[w].1
    }

    async fn callable(&mut self, name: &str) -> Result<CallableId> {
        let bid = find_bind_id(&self.compiled.env, name)?;
        let r = self.gx.compile_ref(bid).await?;
        let f = r.last.clone().context("no lambda")?;
        let c = self.gx.compile_callable(f).await?;
        let id = c.id();
        self.refs.push(r);
        self.callables.push(c);
        Ok(id)
    }

    async fn click(&mut self, id: CallableId) -> Result<()> {
        self.gx.call(id, ValArray::from_iter([Value::Null]))?;
        self.drain().await
    }

    fn send(&mut self, id: ExprId, a: Action) -> (bool, Vec<Message>) {
        let mut shell = MessageShell::new(iced_core::Point::ORIGIN);
        let hit = self.widget.on_message(&Message::EditorAction(id, a), &mut shell);
        (hit, shell.out.drain(..).collect())
    }

    fn alive(&mut self, id: ExprId) -> bool {
        self.send(id, Action::Scroll { lines: 0 }).0
    }

    /// The content-ref ids of the tree's text editors, found by offering a
    /// no-op editor action to every recently minted expr id.
    fn editors(&mut self, want: usize) -> Vec<ExprId> {
        let hi = ExprId::new().inner();
        let mut found = vec![];
        let mut k = hi;
        while found.len() < want && hi - k < 2_000_000 {
            k -= 1;
            let id = ExprId::from_inner(k);
            if self.alive(id) {
                found.push(id);
            }
        }
        found
    }

    /// Type `c` into editor `id` and deliver its on_edit call as the event
    /// loop does; the text the editor published.
    async fn type_char(&mut self, id: ExprId, c: char) -> Result<String> {
        let (hit, out) = self.send(id, Action::Edit(Edit::Insert(c)));
        if !hit {
            bail!("editor {id:?} is gone")
        }
        let mut text = String::new();
        for m in out {
            if let Message::Call(cid, args) = m {
                if let Some(Value::String(s)) = args.clone().into_iter().next() {
                    text = s.to_string();
                }
                self.gx.call(cid, args)?;
            }
        }
        self.drain().await?;
        Ok(text)
    }
}

const IDENTICAL: &str = r#"
use gui::{button::button, column::column, text::text, text_editor::text_editor};
let doc = "hello";
let page: [`Home, `About] = `Home;
let instances = 0;
let go_home = |c: null| {
  let born = 1;
  instances <- born ~ instances + 1;
  page <- c ~ `Home
};
let go_about = |c: null| page <- c ~ `About;
let children = select page {
  `Home => [
    button(#on_press: go_home, &text(&"Home")),
    button(#on_press: go_about, &text(&"About")),
    text_editor(#on_edit: |v| doc <- v, &doc)
  ],
  `About => [button(#on_press: go_home, &text(&"Home")), text(&"about page")]
};
let result = column(&children);
"#;

#[tokio::test(flavor = "multi_thread")]
async fn identical_refire_rebuilds_the_subtree() -> Result<()> {
    let mut h = H::new(IDENTICAL).await?;
    let children = h.watch("test::children").await?;
    let instances = h.watch("test::instances").await?;
    h.drain().await?;
    let go_home = h.callable("test::go_home").await?;
    h.drain().await?;
    let ed = h.editors(1)[0];
    let mut typed = vec![];
    for c in ['a', 'b', 'c'] {
        typed.push(h.type_char(ed, c).await?);
    }
    let fires = h.hist(children).len();
    let born = h.hist(instances).to_vec();
    h.click(go_home).await?;
    let hist = h.hist(children);
    let refired = hist.len() - fires;
    let identical = hist.len() >= 2 && hist[hist.len() - 1] == hist[hist.len() - 2];
    let born_after = h.hist(instances)[born.len()..].to_vec();
    let survived = h.alive(ed);
    let ed2 = if survived { ed } else { h.editors(1)[0] };
    let after = h.type_char(ed2, 'Y').await?;
    println!("identical: typed {typed:?}");
    println!(
        "identical: Home on Home -> children fired {refired}x, identical={identical}"
    );
    println!(
        "identical: `instances` before the click {born:?}, written by the click {born_after:?}"
    );
    println!("identical: editor survived={survived}; typing Y gives {after:?}");
    assert_eq!(after, "abcYhello", "the editor lost its cursor");
    Ok(())
}

const SIBLING: &str = r#"
use gui::{column::column, text::text, text_editor::text_editor};
let doc = "hello";
let mode: [`A, `B] = `A;
let flip = |c: null| mode <- c ~ `B;
let children = [
  text_editor(#on_edit: |v| doc <- v, &doc),
  select mode { `A => text(&"mode a"), `B => text(&"mode b") }
];
let result = column(&children);
"#;

#[tokio::test(flavor = "multi_thread")]
async fn sibling_change_rebuilds_an_unchanged_editor() -> Result<()> {
    let mut h = H::new(SIBLING).await?;
    let children = h.watch("test::children").await?;
    let flip = h.callable("test::flip").await?;
    h.drain().await?;
    let ed = h.editors(1)[0];
    for c in ['a', 'b', 'c'] {
        h.type_char(ed, c).await?;
    }
    h.click(flip).await?;
    let hist = h.hist(children);
    let first_same = match (&hist[hist.len() - 2], &hist[hist.len() - 1]) {
        (Value::Array(a), Value::Array(b)) => a[0] == b[0],
        _ => false,
    };
    let survived = h.alive(ed);
    let ed2 = if survived { ed } else { h.editors(1)[0] };
    let after = h.type_char(ed2, 'Y').await?;
    println!("sibling: editor element unchanged={first_same}, editor survived={survived}");
    println!("sibling: typing Y gives {after:?}");
    assert_eq!(after, "abcYhello", "the editor lost its cursor");
    Ok(())
}

const APPEND: &str = r#"
use gui::{column::column, text_editor::text_editor};
let rows = [ROWS];
let add = |c: null| rows <- c ~ array::push(rows, "new");
let children = array::map(rows, |s| text_editor(&s));
let result = column(&children);
"#;

#[tokio::test(flavor = "multi_thread")]
async fn append_rebuilds_every_row() -> Result<()> {
    const N: usize = 20;
    let rows = (0..N).map(|i| format!("\"r{i}\"")).collect::<Vec<_>>().join(", ");
    let mut h = H::new(&APPEND.replace("ROWS", &rows)).await?;
    let add = h.callable("test::add").await?;
    h.drain().await?;
    let old = h.editors(N);
    assert_eq!(old.len(), N);
    let before = ExprId::new().inner();
    h.click(add).await?;
    let minted = ExprId::new().inner() - before;
    let survivors = old.iter().filter(|id| h.alive(**id)).count();
    let now = h.editors(N + 1).len();
    println!(
        "append: {N} rows -> {now} editors; {survivors} of {N} old editors survived; \
         {minted} expr ids minted by the append"
    );
    assert_eq!(survivors, N, "every unchanged row was rebuilt");
    Ok(())
}
