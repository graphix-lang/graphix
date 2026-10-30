use super::{WakeBit, compiler::compile, dense_gate, gather, list, produce_constant};
use crate::{
    CFlag, CompileCtx, ExecCtx, Node, NodeView, Rt, Scope, TagValue, Update, defetyp,
    env::Env,
    err,
    expr::{Expr, ExprId},
    fusion::{
        self,
        emit::{
            BodyCx, CompiledExpr, emit_array_ref_node, emit_array_slice_node,
            emit_list_new_node, emit_seq_new_node,
        },
    },
    image::{
        ImageBuf,
        nodes::{
            NodeTag, decode_node, decode_nodes, encode_nodes, opt_node_decode,
            opt_node_encode, put_tag,
        },
    },
    typ::{TVar, Type},
    wrap,
};
use ahash::AHashSet;
use anyhow::Result;
use arcstr::ArcStr;
use enumflags2::BitFlags;
use netidx_core::pack::{Pack, PackError};
use netidx_value::{PBytes, Typ, ValArray, Value};
use poolshark::local::LPooled;
use std::{fmt::Debug, iter, marker::PhantomData, mem, ops::Range};
use triomphe::Arc;

defetyp!(ERR, ERR_TAG, "ArrayIndexError", "Error<`{}(string)>");

/// What makes two of a literal's element types the very same type: a
/// primitive by value, a cell by its binding cell, a composite by the
/// parts it shares, anything else by address.
#[derive(PartialEq, Eq, Hash)]
struct SameType(mem::Discriminant<Type>, u64, u64);

impl SameType {
    fn of(t: &Type) -> Self {
        fn at<T: ?Sized>(p: *const T) -> u64 {
            p.cast::<u8>().addr() as u64
        }
        let (a, b) = match t {
            Type::Primitive(p) => (p.bits(), 0),
            Type::TVar(tv) => (tv.cell_addr() as u64, 0),
            Type::Array(e) | Type::List(e) => (at(Arc::as_ptr(e)), 0),
            Type::Map { key, value } => (at(Arc::as_ptr(key)), at(Arc::as_ptr(value))),
            Type::Tuple(ts) => (at(Arc::as_ptr(ts)), 0),
            Type::Struct(fs) => (at(Arc::as_ptr(fs)), 0),
            t => (at(t), 0),
        };
        Self(mem::discriminant(t), a, b)
    }
}

/// The union of a literal's element types `ts`, `⊥` first: an element of
/// the very type of an earlier one adds nothing, and one type left is its
/// own union. A call through an element accepts each alike: functions
/// meet.
pub(super) fn element_union(env: &Env, ts: &[&Type]) -> Result<Type> {
    let mut seen: LPooled<AHashSet<SameType>> = LPooled::take();
    let mut distinct: LPooled<Vec<&Type>> = LPooled::take();
    distinct.extend(ts.iter().copied().filter(|t| seen.insert(SameType::of(t))));
    match &distinct[..] {
        [_, one] => Ok((*one).clone()),
        ts => Type::union_of_alternatives(env, ts),
    }
}

/// Bind the cell `slot` holds, a literal's own element cell, to `t`
/// without an occurs walk of `t`'s whole depth: nothing else reaches the
/// cell. False when it is not such a cell.
pub(super) fn bind_element(slot: &Arc<Type>, t: &Type) -> bool {
    !matches!(t, Type::Bottom) && TVar::bind_unshared(slot, t)
}

/// An array index or slice bound: any integer. An open cell is narrowed
/// to the integers, as an arithmetic operand is to the numbers.
pub(super) fn check_index<R: Rt>(env: &Env, i: &Node<R>) -> Result<()> {
    wrap!(i, super::op::constrain_operand(env, &Type::Primitive(Typ::integer()), i.typ()))
}

#[derive(Debug)]
pub struct ArrayRef<R: Rt> {
    /// wake catch-up: set by `sleep()`, taken by `dense_gate!`
    slept: WakeBit,
    pub source: Node<R>,
    pub i: Node<R>,
    pub(crate) spec: Arc<Expr>,
    pub typ: Type,
    pub etyp: Type,
    resident: TagValue,
}

impl<R: Rt> ArrayRef<R> {
    fn with(
        spec: Arc<Expr>,
        typ: Type,
        etyp: Type,
        source: Node<R>,
        i: Node<R>,
    ) -> Node<R> {
        Node::new(Self {
            slept: WakeBit::default(),
            source,
            i,
            spec,
            typ,
            etyp,
            resident: TagValue::phantom(),
        })
    }

    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<'_, R>,
        buf: &mut &[u8],
    ) -> Result<Node<R>, PackError> {
        let source = decode_node(ctx, buf)?;
        let i = decode_node(ctx, buf)?;
        let spec = Arc::new(Expr::decode(buf)?);
        let typ = Type::decode(buf)?;
        let etyp = Type::decode(buf)?;
        // CR claude for claude: [structure/low] This literal repeats the one in
        // ArrayRef::compile field for field, and about forty kinds do the same between
        // compile/init and image_decode. Examples: ArraySlice, MapRef, the error.rs and
        // data.rs nodes, Bind, LambdaInstance, Module, CachedArgsAsync (12 fields
        // twice), Filter/Queue/Hold/Throttle, and the net/time/watch/http builtins. A
        // runtime field added to one literal and defaulted differently in the other is
        // a cold-versus-warm divergence. Give each kind one constructor over its
        // encoded parts, as Select::new does, and call it from both paths. Found by
        // reading; not run. (x-dup-03)
        // 2026-10-27 claude: Partial: ArrayRef and ArraySlice now build through one
        // constructor (with) from compile and image_decode, as SeqLit already did. The
        // other kinds this CR lists (MapRef, the error.rs and data.rs nodes, Bind,
        // LambdaInstance, Module, CachedArgsAsync, Filter/Queue/Hold/Throttle, the
        // net/time/watch/http builtins) still repeat their literal; each is its own
        // edit across many files, left for a batch of its own.
        // 2026-10-27 codex: Eleven more kinds now have one with constructor shared by
        // compile and image_decode, one commit per kind: MapRef; Struct, StructWith,
        // StructRef, Tuple, Variant, Construct, TupleRef; Qop, SeqGuard, SeqAbort.
        // Runtime defaults, Qop's raised type, and SeqGuard's handler poll registration
        // are initialized there. The eleven node_constructors structural pins all
        // failed on the original duplicated literals and all pass now. graphix-shell
        // constructor_image::constructor_nodes_agree_cold_and_warm passes: the
        // non-fused program exercises these shapes, a map lookup and sequence guards,
        // writes a program image cold, loads it warm, and produces 19 in both runs. The
        // full examples_compile example compilation test also passed on the refactored
        // code. The behavioral image test is an equivalence check, not a claimed
        // pre-fix behavioral failure. Encoded fields and byte order are unchanged, so
        // no image format bump. Remaining: Catch and ErrorRelay, Bind, LambdaInstance,
        // Module, CachedArgsAsync, Filter/Queue/Hold/Throttle, and net/time/watch/http
        // builtins. The CR remains open for those kinds.
        // 2026-10-27 claude: Catch, ErrorRelay and Bind now build through one
        // constructor each (Catch::with, ErrorRelay::with, Bind::with; Bind had a third
        // site in lower_over_operands), commits 6852e38f6, f33713163, 6d51e8bb1, by the
        // local qwen lane, reviewed. lang::image and lang::errors pass. Remaining:
        // LambdaInstance, Module, CachedArgsAsync, Filter/Queue/Hold/Throttle, and the
        // net/time/watch/http builtins.
        // 2026-10-28 qwen: 2026-10-28 qwen: The five kinds this batch named already
        // build through one constructor at this base, so there was no code change to
        // make here; recording what is there and what remains. LambdaInstance
        // (node::lambda::GXLambda) has private with(id, instance_id, args, body, typ,
        // self_call, env, typing, charged) at lambda.rs:1123, called from new_with_body
        // (the compile path, shared by new/new_collection) and from image_decode;
        // Module has Module::with (module.rs:971) called by compile, image_decode and
        // typecheck0_instance; CachedArgsAsync::with (lib.rs:2279) is the one literal
        // for both its eval-init and image_decode, with
        // queued/inflight/replaced/abort/kept/panic_tag/ data_result/out derived there
        // (kept from T::CANCEL_SAFE); Filter::with (lib.rs:3079), Hold::with (3305) and
        // Throttle::with (3499) each hold the single literal used by init and
        // image_decode, and Queue already had one new (3189) before this batch. Grep
        // for each kind's runtime fields (self_call:, inflight:, kept:, triggered:,
        // last_v:) finds exactly one literal per kind. Encoded fields and byte order
        // are unchanged, so no image format bump. Verified at this tip: cargo test -p
        // graphix-tests --offline --lib lang::image is 31 passed, 0 failed, 1 ignored,
        // and cargo test -p graphix-shell --offline --test constructor_image
        // (constructor_nodes_agree_cold_and_warm) passes. Remaining: the
        // net/time/watch/http builtins, which this batch was told to leave.
        // 2026-10-28 qwen: 2026-10-28 qwen: The net, time, watch and http builtins now
        // build through one constructor each, called by init and image_decode; one
        // commit per file. time.rs: AfterIdle::with(timeout_v, last_v, eid),
        // Now::new(); Timer already had Timer::new(eid) used by both paths, so nothing
        // to change. watch.rs: CreateWatcher::with(poll_interval, batch_size),
        // WatchApply::with(interest, path), WatchStream::with(var, cached). net.rs:
        // Write::new() (it encodes nothing), Subscribe::with(var, target),
        // RpcCall::with(var, target), the list! macro's own with(var) — which covers
        // List and ListTable —, Publish::with(top_id, wid, on_write, write_typ) (each
        // path keeps its own ctx.record_ref(wid, top_id)), PublishRpc::with(var,
        // handler, cast_typ). http/lib.rs: HttpServe::with(var, handler), and
        // HttpRequestEv's ImageState::image_decode returns Self::default() — the shape
        // sys/tls.rs's started_image_state! already uses — so its literal is written
        // once, in Default. Every runtime default (WakeBit::default, out: phantom,
        // inbox, current, defaults, max_inflight, handler: false, listening/abort) now
        // lives in one place per kind; image_encode writes the same fields in the same
        // order, so no image format bump. Tests (sandbox refuses sockets, so the
        // net/http fixtures fail there; lists compared): cargo test -p
        // graphix-package-sys --offline --lib is 31 passed, 1 failed before and after,
        // the same test both times (loader::test::a_miss_records_its_probe, "resolver
        // server shutdown"). cargo test -p graphix-package-http --offline --lib has no
        // unit tests: 0 passed before and after. cargo test -p graphix-tests --offline
        // --lib lang::image is 32 passed, 1 ignored before and after. Since the http
        // package has no tests of its own, the http fixtures were compared too: cargo
        // test -p graphix-tests --offline --lib lib_tests::http is 8 passed, 90 failed
        // before and after with an identical 90-name failure list. No test changed. A
        // scan of every image_decode body in the tree for a struct literal finds none
        // left in these four files. Still repeating elsewhere under this CR (found by
        // the same scan, not touched here): graphix_package_core::Handler repeats its
        // literal in new (stdlib/graphix-package-core/src/handler.rs:193) and
        // image_decode (:316); graphix_compiler::node::genn::Callback repeats it in
        // through_binding (graphix-compiler/src/node/genn.rs:376) and image_decode
        // (:479); tui's SuspendEv rebuilds in ImageState::image_decode what Default
        // plus attach produce (stdlib/graphix-package-tui/src/lib.rs:810); CachedVals
        // and a few core wrappers rebuild a decode-time literal.
        // node::pattern::PatternNode and node::lambda::BuiltInLambda have a literal
        // only in their decode, so there is nothing to share there. x-dup-03 stays open
        // for the remaining kinds.
        // 2026-10-28 qwen: The four kinds this batch named now share one constructor
        // between creation and image_decode; one commit per file (five commits).
        // genn::Callback::with(call, function, args) is now the only literal, used by
        // of_value, through_binding and image_decode. Handler::with(proto, pid, ftyp,
        // scope, top_id) holds the single literal (pending/running/siblings default
        // there), called by new and image_decode; its Request sub-struct likewise got
        // Request::with(call, relay). tui's SuspendEv image_decode now builds through
        // Default plus attach (the code that produces the cold shape), so the decode
        // literal is gone and no field list repeats. In core lib.rs:
        // CachedVals::with(slots) (used by new/from_values/image_decode; as_data keeps
        // its evaluated through it), CachedArgs::with(woke_pending, cached, t)
        // (last_result: phantom lives there), OrderChecked::with(inner, check) (decode
        // order preserved: check still decodes first), Once::with(val),
        // Counted::with(n), Count::with(count), Range::with(var, args),
        // PrintCall::with(printer, dest) (both printer! macro paths). Iter, IterQ,
        // TypedRead, CachedArgsAsync, Filter/Hold/Throttle/Queue already had one
        // constructor at this base; nothing to change there. Encoded fields and byte
        // order are unchanged everywhere, so no image format bump. Tests (sandbox
        // refuses sockets; lists compared before and after): graphix-package-core --lib
        // 7 passed, 0 failed both runs; graphix-compiler --lib 54 passed, 0 failed both
        // runs; graphix-package-tui --lib 158 passed, 9 failed both runs with an
        // identical name list (the pty/browser input_test fixtures and
        // every_example_renders); graphix-tests --lib lang::image 32 passed, 1 ignored
        // both runs. No test changed. Final scan of every image_decode body in the tree
        // (string- and comment-aware, Self resolved to its impl target) finds repeated
        // literals left only outside this batch's kinds: graphix-compiler node/bind.rs
        // Place, ByRef, Deref; node/collection.rs Callback, MapQBase, FoldQBase;
        // node/data.rs Replace; node/error.rs CatchAction; node/lambda.rs
        // BuiltInLambda, Lambda and DefTable (three literals share discriminants:
        // Mutex::default(); an earlier note called BuiltInLambda decode-only, but init
        // builds its literal too); node/pattern.rs PatternNode (the compile path holds
        // a second literal, correcting an earlier note); node/seq_machine.rs
        // SeqMachine, SeqCapture; node/traits.rs Trait, Impl; node/mod.rs
        // ExplicitParens, TypeDef, Block, StringInterpolate, Connect, ConnectDeref,
        // TypeCast, Never, Any, Sample; node/module.rs the Body::Dynamic variant
        // literal (decode versus compile); node/callsite.rs the Arg literal in decode
        // where Arg::new exists, and Callee::Static; fusion/share.rs SharedRegion and
        // SlotShare in the share-table decode. stdlib: graphix-package-array's
        // function-builtin wrapper (queue/buf/pred), graphix-package-rand Rand, Pick,
        // Shuffle, graphix-package-sys io.rs's read wrapper (var/reading/out), core
        // queuefn.rs QueueState and its wrapper literal, core lib.rs RtVar (new versus
        // decode; no defaulted runtime field, and the two paths hold CompileCtx and
        // ExecCtx, so one constructor needs a context change first), graphix-package-db
        // subscribe.rs DbSubscribe and tree.rs TreeTypes/Database. ImagedRefs in
        // callsite.rs has a literal only in its decode, so nothing to share there.
        // x-dup-03 stays open for those.
        // 2026-10-28 qwen: Ten node kinds in graphix-compiler/src/node/mod.rs and
        // twelve builtin kinds in stdlib now build through one constructor shared by
        // their compile/init path and image_decode, one commit per file. mod.rs:
        // ExplicitParens::with, TypeDef::with, Block::with (new and compile both call
        // it, so lets_of is computed once), StringInterpolate::with (fork still decoded
        // last), Connect::with, ConnectDeref::with (each path keeps its own
        // ctx.record_ref), TypeCast::with (compile keeps src_ref false and check_keys
        // true, decode computes both), Never::with, Any::with, Sample::with. stdlib:
        // rand Rand::with(args), Pick::new and Shuffle::new (neither encodes anything);
        // sys io.rs IoLines::with(var); core queuefn.rs QueueState::with(pop_count,
        // count_ref, last_written_depth) with new() delegating to it, and
        // QueueFn::with(state, fid, ftyp, top_id, scope); array Group::with(pred); db
        // subscribe.rs DbSubscribe::with(args) plus the db_event_accessor wrapper's
        // with(top_id, cached, data), which covers DbOnChange, DbOnInsert and
        // DbOnRemove; db tree.rs TreeTypes::with(key, key_str, val_str) and
        // OpenTreeEv::with(types), used by Default, init and image_decode. This batch's
        // scan found one more of the same class and fixed it: sys dirs_mod.rs
        // born_builtin! builds its wrapper through with(once), covering the 19 dirs
        // builtins and sys_args. The db package's tests ran once with both db files
        // changed. Skipped, as agreed: core lib.rs RtVar. Its two literals hold no
        // defaulted runtime field, and one constructor would have to serve both
        // contexts -- new takes &mut CompileCtx and defers the registration through
        // record_ref, image_decode takes &mut ExecCtx and registers it with the runtime
        // -- so it needs a context change first, not just a shared literal. Needed
        // nothing: db tree.rs Database is built once, in try_open_db; queuefn.rs
        // WrapperApply is built once, in build_wrapper_apply, and its image_encode
        // refuses with NOT_QUIESCENT. Encoded fields and byte order are unchanged
        // everywhere, so no image format bump. Tests (sandbox refuses sockets; lists
        // compared before and after, RUSTC_WRAPPER=""): graphix-compiler --lib 54
        // passed both runs; graphix-package-core 7; graphix-package-rand 28;
        // graphix-package-array 0 (no unit tests); graphix-package-db 28;
        // graphix-package-sys 31 passed 1 failed both runs, the same test each time
        // (loader::test::a_miss_records_its_probe, "resolver server shutdown");
        // graphix-tests --lib lang::image 32 passed, 1 ignored both runs. The whole
        // graphix-tests --lib suite too, at this tip and at the batch's base: 11065
        // passed, 447 failed, 3 ignored in both, with an identical failure-name list --
        // every one a net, http, tcp, tls, json/toml/pack stream, modules, traits or
        // typecheck fixture that needs a socket or the module resolver server. No test
        // changed. Still repeating, for the next batch: node/bind.rs Place, ByRef,
        // Deref; node/collection.rs MapQBase, FoldQBase and Callback; node/data.rs
        // Replace; node/error.rs CatchAction; node/lambda.rs BuiltInLambda (make_init
        // versus image_decode), Lambda and DefTable; node/pattern.rs PatternNode;
        // node/seq_machine.rs SeqMachine and SeqCapture; node/traits.rs Trait and Impl;
        // node/module.rs the Body::Dynamic literal (decode at 1014, compile at 1088);
        // node/callsite.rs the Arg literal in image_decode at 2976 where Arg::new
        // exists, and Callee::Static; fusion/share.rs SharedRegion and SlotShare in the
        // share-table decode. x-dup-03 stays open for those.
        // 2026-10-28 qwen: The twenty-one kinds this batch named now build through one
        // constructor shared by their compile/init path and image_decode, plus four
        // more of the same class that this batch's scan found; fourteen commits, one
        // per file (error.rs and share.rs carry two). bind.rs: Place::with(root, steps,
        // scope), ByRef::with(spec, typ, referent, id) (resident phantom,
        // registered/keep None), Deref::with(spec, typ, child, top_id, addr) (resident
        // phantom; each path keeps its own record_ref of the address it read).
        // collection.rs: Callback::with(scope, id, typ, top_id, share, arg) (slot_spec
        // default), MapQBase::with nine parts, FoldQBase::with eight. data.rs:
        // Replace::with(index, name, n). error.rs: CatchAction::with(node, role)
        // (pending None), AbortRole::machine(manual, pc, clock) — the clock's owed flag
        // starts false there — and QopMode::raise(handler, top_id, flags), which this
        // batch's scan found holding values: SiteValues::default() in both decode and
        // compile. lambda.rs: BuiltInLambda::with(typ, name, apply) (make_init and
        // image_decode), Lambda::with(spec, def, typ) (resident stale(def)),
        // DefTable::with nine parts for renamed and image_decode. pattern.rs:
        // PatternNode::with(explicit_type_predicate, type_predicate,
        // structure_predicate, guard) — the compile path does hold a second literal,
        // correcting the earlier note. seq_machine.rs: SeqMachine::with(spec, id, pc,
        // pc_id, steps, expand) (current/tracked None), SeqCapture::with(spec, machine,
        // snapshot, live, live_id, is_live) (the AtomicBool lives there). traits.rs:
        // Trait::with(spec, def, defaults), Impl::with(spec, def, fulfils, trait_def,
        // body, prototypes). module.rs: Body::dynamic(source, sig_env) (Module::with
        // already covered the node). callsite.rs: Arg::image_decode now calls the
        // existing Arg::new, and Callee::to_static(apply, first_update) is the only
        // Callee::Static literal (bind_instance, the imaged body's first dispatch, and
        // both image_decode arms). fusion/share.rs: SharedRegion::with(root, kernel,
        // jit, facts, print), SlotShare::with(table, base) (all three sites),
        // SharedAttempt::refused(root, blocker) and args Parse::new(). Left alone with
        // a reason: AbortRole::Try and Body::Dynamic name only the parts each path
        // supplies, nothing defaulted; DefTable::record and the check's fallback write
        // ..Default::default() and then fill the maps, so they have no second literal
        // of their shape; Callee::Imaged and ImagedRefs are built only in their decode.
        // Encoded fields and byte order are unchanged, and every constructor takes its
        // arguments in the order the literal listed its fields, so decode order is
        // unchanged: no image format bump. Tests (sandbox refuses sockets;
        // RUSTC_WRAPPER=""; lists compared at this tip and with these twelve files
        // reverted to the batch's base): graphix-compiler --lib 54 passed, 0 failed in
        // both; graphix-package-args --lib has no unit tests, 0 in both; graphix-tests
        // --lib lang::image 32 passed, 1 ignored, 0 failed in both; graphix-tests --lib
        // lang::errors 259 passed, 0 failed at the tip; and graphix- tests --lib lang::
        // — 7819 passed, 96 failed, 1 ignored in both, with an identical failure-name
        // list (the dynamic-module, net, http, tcp and tls fixtures that need a socket
        // or the module resolver server). No test changed. Still repeating for the next
        // batch, from the same scan. Same class, worth fixing: node/lambda.rs's
        // LambdaDef literal (compile) and image/defs.rs's (def_decode) are one struct
        // built in two files with runtime defaults in both (no_loop, captures;
        // recursion and param_calls start empty on one path and carry decoded values on
        // the other, so a constructor needs one argument for each); node/mod.rs Sample
        // builds Banking::One { owed: false } and Banking::Each(Payments::new(id)) in
        // both image_decode and compile; graphix-types abstract_value.rs builds
        // GxAbstract with params_key: OnceLock::new() in both Pack::decode and wrap,
        // and reference.rs builds Reference with _keep: None in decode and _keep: keep
        // in wrap; graphix-package-core RtVar remains as agreed (its two paths hold
        // CompileCtx and ExecCtx, so one constructor needs a context change first).
        // Lower risk — every field is supplied by both paths, so nothing can diverge:
        // fusion/emit/lower.rs SiteLayout and CtxWord, fusion/emit/record.rs
        // RecordReloc and the RecordKind variants, and fusion/share.rs RegionPrint (the
        // walk builds an empty one and fills it). x-dup-03 stays open for those.
        // 2026-10-28 qwen: graphix-shell --test constructor_image
        // (constructor_nodes_agree_cold_and_warm) passes at this tip: the program that
        // exercises these shapes runs to the same production cold and from a restored
        // image.
        Ok(Self::with(spec, typ, etyp, source, i))
    }

    pub(crate) fn compile(
        ctx: &mut CompileCtx<R>,
        flags: BitFlags<CFlag>,
        spec: Arc<Expr>,
        scope: &Scope,
        top_id: ExprId,
        source: &Expr,
        i: &Expr,
    ) -> Result<Node<R>> {
        let source = compile(ctx, flags, source.clone(), scope, top_id)?;
        let i = compile(ctx, flags, i.clone(), scope, top_id)?;
        let etyp = match &source.typ() {
            Type::Array(et) => (**et).clone(),
            Type::Primitive(p) if *p == Typ::Bytes => Type::Primitive(Typ::U8.into()),
            _ => Type::empty_tvar(),
        };
        let typ = Type::Set(Arc::from_iter([etyp.clone(), ERR.clone()]));
        Ok(Self::with(spec, typ, etyp, source, i))
    }
}

/// An integer index or slice bound as an `i64`, the form both engines
/// index with. An unsigned value above `i64::MAX` saturates: it is out
/// of bounds for any length and never counts from the end.
pub(crate) fn index_i64(v: &Value) -> Option<i64> {
    match v {
        Value::I64(i) | Value::Z64(i) => Some(*i),
        Value::U64(u) | Value::V64(u) => Some(i64::try_from(*u).unwrap_or(i64::MAX)),
        v => v.clone().cast_to::<i64>().ok(),
    }
}

/// The offset `i` names in a sequence of `len` elements: non-negative
/// counts from the start, negative from the end. Unchecked against `len`.
fn offset(len: usize, i: i64) -> Option<usize> {
    usize::try_from(if i < 0 { len as i64 + i } else { i }).ok()
}

/// An indexed or sliced source is bytes only when its type says so; a
/// source not known yet (an open cell) is assumed to be an array.
fn known_bytes(env: &Env, source: &Type) -> Result<bool> {
    let bytes = Type::Primitive(Typ::Bytes.into());
    Ok(!source.with_deref(|t| t.is_none())
        && bytes.contains_with_flags(BitFlags::empty(), env, source)?)
}

/// The position index `i` names in a sequence of `len` elements.
pub(crate) fn index(len: usize, i: i64) -> Option<usize> {
    offset(len, i).filter(|j| *j < len)
}

/// The range `[start..end]` names in a sequence of `len` elements; an
/// absent bound is the start or the end.
fn slice_range(len: usize, start: Option<i64>, end: Option<i64>) -> Option<Range<usize>> {
    let bound = |b: Option<i64>, absent| match b {
        None => Some(absent),
        Some(i) => offset(len, i).filter(|j| *j <= len),
    };
    match (bound(start, 0), bound(end, len)) {
        (Some(i), Some(j)) if i <= j => Some(i..j),
        _ => None,
    }
}

/// `array[i]`, shared by the node-walk and the JIT: the bare element,
/// or the `ArrayIndexError` value when out of bounds.
pub(crate) fn array_index(elts: &ValArray, i: i64) -> Value {
    match index(elts.len(), i) {
        Some(i) => elts[i].clone(),
        None => err!(ERR_TAG, "array index out of bounds"),
    }
}

/// `src[i]`, shared by the node-walk and the JIT: [`array_index`] or
/// [`bytes_index`] by the source's form, else the `ArrayIndexError`.
pub(crate) fn value_index(src: &Value, i: i64) -> Value {
    match src {
        Value::Array(elts) => array_index(elts, i),
        Value::Bytes(b) => bytes_index(b, i),
        _ => err!(ERR_TAG, "expected an array"),
    }
}

/// `bytes[i]`, with the rules of [`array_index`]: the `Value::U8` or
/// the out-of-bounds error.
pub(crate) fn bytes_index(b: &PBytes, i: i64) -> Value {
    match index(b.len(), i) {
        Some(i) => Value::U8(b[i]),
        None => err!(ERR_TAG, "array index out of bounds"),
    }
}

/// `a[i..j]` / `a[i..]` / `a[..j]` / `a[..]` over an array or bytes,
/// shared by the node-walk and the JIT: the sub-array / sub-bytes, or
/// the `ArrayIndexError` value for a bound out of range.
pub(crate) fn array_slice(src: &Value, start: Option<i64>, end: Option<i64>) -> Value {
    let v = match src {
        Value::Array(elts) => slice_range(elts.len(), start, end)
            .and_then(|r| elts.subslice(r).ok())
            .map(Value::Array),
        Value::Bytes(b) => slice_range(b.len(), start, end)
            .map(|r| Value::Bytes(PBytes::new(b.slice(r)))),
        _ => return err!(ERR_TAG, "expected array"),
    };
    v.unwrap_or_else(|| err!(ERR_TAG, "slice out of bounds"))
}

impl<R: Rt> Update<R> for ArrayRef<R> {
    fn for_each_child<'a>(&'a self, f: &mut dyn FnMut(&'a Node<R>)) {
        f(&self.source);
        f(&self.i)
    }

    fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut Node<R>)) {
        f(&mut self.source);
        f(&mut self.i)
    }

    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::ArrayRef, buf);
        self.source.image_encode(buf)?;
        self.i.image_encode(buf)?;
        self.spec.encode(buf)?;
        self.typ.encode(buf)?;
        self.etyp.encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R>) -> &TagValue {
        let s = self.source.update(ctx);
        let i = self.i.update(ctx);
        let tag = s.tag().join(i.tag());
        dense_gate!(self, tag, tag.is_bottom());
        let v = s.with_value(|s| match i.with_value(index_i64) {
            None => err!(ERR_TAG, "expected an integer"),
            Some(i) => value_index(s, i),
        });
        self.resident.set(TagValue::tagged(v, tag))
    }

    // The element cell binds from the source as in the check; the
    // index needs no judging.
    super::typed_by_row!(source);

    fn typ(&self) -> &Type {
        &self.typ
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R>) {
        self.slept.set();
        self.source.sleep(ctx);
        self.i.sleep(ctx);
    }

    fn fuse(&mut self, ctx: &mut CompileCtx<R>) -> Result<Option<Node<R>>> {
        fusion::fuse_parts([&mut self.source, &mut self.i], ctx)
    }

    fn view(&self) -> NodeView<'_, R> {
        NodeView::ArrayRef(self)
    }

    fn emit_clif(&self, cx: &mut BodyCx) -> Result<CompiledExpr> {
        emit_array_ref_node(cx, &self.source, &self.i)
    }
}

#[derive(Debug)]
pub struct ArraySlice<R: Rt> {
    /// wake catch-up: set by `sleep()`, taken by `dense_gate!`
    slept: WakeBit,
    pub source: Node<R>,
    pub start: Option<Node<R>>,
    pub end: Option<Node<R>>,
    pub(crate) spec: Arc<Expr>,
    pub typ: Type,
    resident: TagValue,
}

impl<R: Rt> ArraySlice<R> {
    fn with(
        spec: Arc<Expr>,
        typ: Type,
        source: Node<R>,
        start: Option<Node<R>>,
        end: Option<Node<R>>,
    ) -> Node<R> {
        Node::new(Self {
            slept: WakeBit::default(),
            source,
            start,
            end,
            spec,
            typ,
            resident: TagValue::phantom(),
        })
    }

    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<'_, R>,
        buf: &mut &[u8],
    ) -> Result<Node<R>, PackError> {
        let source = decode_node(ctx, buf)?;
        let start = opt_node_decode(ctx, buf)?;
        let end = opt_node_decode(ctx, buf)?;
        let spec = Arc::new(Expr::decode(buf)?);
        let typ = Type::decode(buf)?;
        Ok(Self::with(spec, typ, source, start, end))
    }

    pub(crate) fn compile(
        ctx: &mut CompileCtx<R>,
        flags: BitFlags<CFlag>,
        spec: Arc<Expr>,
        scope: &Scope,
        top_id: ExprId,
        source: &Expr,
        start: &Option<Arc<Expr>>,
        end: &Option<Arc<Expr>>,
    ) -> Result<Node<R>> {
        let source = compile(ctx, flags, source.clone(), scope, top_id)?;
        let start = start
            .as_ref()
            .map(|e| compile(ctx, flags, (**e).clone(), scope, top_id))
            .transpose()?;
        let end = end
            .as_ref()
            .map(|e| compile(ctx, flags, (**e).clone(), scope, top_id))
            .transpose()?;
        let typ = Type::Set(Arc::from_iter([source.typ().clone(), ERR.clone()]));
        Ok(Self::with(spec, typ, source, start, end))
    }
}

impl<R: Rt> Update<R> for ArraySlice<R> {
    fn for_each_child<'a>(&'a self, f: &mut dyn FnMut(&'a Node<R>)) {
        f(&self.source);
        self.start.iter().chain(self.end.iter()).for_each(f)
    }

    fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut Node<R>)) {
        f(&mut self.source);
        self.start.iter_mut().chain(self.end.iter_mut()).for_each(f)
    }

    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::ArraySlice, buf);
        self.source.image_encode(buf)?;
        opt_node_encode(self.start.as_ref(), buf)?;
        opt_node_encode(self.end.as_ref(), buf)?;
        self.spec.encode(buf)?;
        self.typ.encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R>) -> &TagValue {
        let s = self.source.update(ctx);
        let start = self.start.as_mut().map(|n| n.update(ctx));
        let end = self.end.as_mut().map(|n| n.update(ctx));
        let tag = [start, end].iter().flatten().fold(s.tag(), |t, b| t.join(b.tag()));
        dense_gate!(self, tag, tag.is_bottom());
        let bound = |b: Option<&TagValue>| {
            b.map(|b| b.with_value(index_i64).ok_or(())).transpose()
        };
        let v = match (bound(start), bound(end)) {
            (Ok(start), Ok(end)) => s.with_value(|s| array_slice(s, start, end)),
            _ => err!(ERR_TAG, "expected an integer"),
        };
        self.resident.set(TagValue::tagged(v, tag))
    }

    // The element cell binds from the source as in the check; the
    // index needs no judging.
    super::typed_by_row!(source);

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R>) {
        self.slept.set();
        self.source.sleep(ctx);
        if let Some(start) = &mut self.start {
            start.sleep(ctx);
        }
        if let Some(end) = &mut self.end {
            end.sleep(ctx);
        }
    }

    fn fuse(&mut self, ctx: &mut CompileCtx<R>) -> Result<Option<Node<R>>> {
        fusion::fuse_parts(
            iter::once(&mut self.source)
                .chain(self.start.as_mut())
                .chain(self.end.as_mut()),
            ctx,
        )
    }

    fn typ(&self) -> &Type {
        &self.typ
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn view(&self) -> NodeView<'_, R> {
        NodeView::ArraySlice(self)
    }

    fn emit_clif(&self, cx: &mut BodyCx) -> Result<CompiledExpr> {
        emit_array_slice_node(cx, &self.source, self.start.as_ref(), self.end.as_ref())
    }
}

/// What a [`SeqLit`] builds: an array or a native list.
pub trait SeqKind: Debug + Send + Sync + 'static {
    /// A native list, else an array.
    const LIST: bool;

    /// The literal's type over its element type.
    fn typ(elem: Type) -> Type;

    /// The literal's value over its elements, in order.
    fn build(elems: impl ExactSizeIterator<Item = Value>) -> Value;

    fn empty() -> Value;

    fn view<R: Rt>(n: &SeqLit<R, Self>) -> NodeView<'_, R>
    where
        Self: Sized;

    fn emit<R: Rt>(cx: &mut BodyCx, n: &[Node<R>]) -> Result<CompiledExpr>;
}

#[derive(Debug)]
pub struct ArrayKind;

impl SeqKind for ArrayKind {
    const LIST: bool = false;

    fn typ(elem: Type) -> Type {
        Type::Array(Arc::new(elem))
    }

    fn build(elems: impl ExactSizeIterator<Item = Value>) -> Value {
        Value::Array(ValArray::from_iter_exact(elems))
    }

    fn empty() -> Value {
        Value::Array(ValArray::from([]))
    }

    fn view<R: Rt>(n: &Array<R>) -> NodeView<'_, R> {
        NodeView::Array(n)
    }

    fn emit<R: Rt>(cx: &mut BodyCx, n: &[Node<R>]) -> Result<CompiledExpr> {
        emit_seq_new_node(cx, None, n)
    }
}

#[derive(Debug)]
pub struct ListKind;

impl SeqKind for ListKind {
    const LIST: bool = true;

    fn typ(elem: Type) -> Type {
        Type::List(Arc::new(elem))
    }

    fn build(elems: impl ExactSizeIterator<Item = Value>) -> Value {
        list::from_iter(elems)
    }

    fn empty() -> Value {
        list::nil()
    }

    fn view<R: Rt>(n: &ListLit<R>) -> NodeView<'_, R> {
        NodeView::ListLit(n)
    }

    fn emit<R: Rt>(cx: &mut BodyCx, n: &[Node<R>]) -> Result<CompiledExpr> {
        emit_list_new_node(cx, n)
    }
}

pub type Array<R> = SeqLit<R, ArrayKind>;
pub type ListLit<R> = SeqLit<R, ListKind>;

/// An array or list literal, `[a, b]` or `[<a, b>]`.
#[derive(Debug)]
pub struct SeqLit<R: Rt, K: SeqKind> {
    /// wake catch-up: set by `sleep()`, taken by `dense_gate!`
    slept: WakeBit,
    pub(crate) spec: Arc<Expr>,
    pub typ: Type,
    pub n: Box<[Node<R>]>,
    resident: TagValue,
    kind: PhantomData<K>,
    pub(crate) fork: crate::cost::ForkSite,
}

impl<R: Rt, K: SeqKind> SeqLit<R, K> {
    fn with(
        spec: Arc<Expr>,
        typ: Type,
        n: Box<[Node<R>]>,
        fork: crate::cost::ForkSite,
    ) -> Node<R> {
        Node::new(Self {
            slept: WakeBit::default(),
            spec,
            typ,
            n,
            resident: TagValue::phantom(),
            kind: PhantomData,
            fork,
        })
    }

    pub(crate) fn compile(
        ctx: &mut CompileCtx<R>,
        flags: BitFlags<CFlag>,
        spec: Arc<Expr>,
        scope: &Scope,
        top_id: ExprId,
        args: &Arc<[Expr]>,
    ) -> Result<Node<R>> {
        let n = args
            .iter()
            .map(|e| compile(ctx, flags, e.clone(), scope, top_id))
            .collect::<Result<_>>()?;
        Ok(Self::with(spec, K::typ(Type::empty_tvar()), n, Default::default()))
    }

    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<'_, R>,
        buf: &mut &[u8],
    ) -> Result<Node<R>, PackError> {
        let spec = Arc::new(Expr::decode(buf)?);
        let typ = Type::decode(buf)?;
        let n = decode_nodes(ctx, buf)?.into_boxed_slice();
        Ok(Self::with(spec, typ, n, crate::cost::ForkSite::image_decode(buf)?))
    }
}

impl<R: Rt, K: SeqKind> Update<R> for SeqLit<R, K> {
    fn for_each_child<'a>(&'a self, f: &mut dyn FnMut(&'a Node<R>)) {
        self.n.iter().for_each(f)
    }

    fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut Node<R>)) {
        self.n.iter_mut().for_each(f)
    }

    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(if K::LIST { NodeTag::ListLit } else { NodeTag::Array }, buf);
        self.spec.encode(buf)?;
        self.typ.encode(buf)?;
        encode_nodes(&self.n, buf)?;
        self.fork.image_encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R>) -> &TagValue {
        if self.n.is_empty() {
            return produce_constant(ctx.event, &mut self.resident, K::empty);
        }
        let (tag, prods) = gather(ctx, &mut self.n, &mut self.fork);
        dense_gate!(self, tag, tag.is_bottom());
        let v = K::build(prods.into_iter().map(|tv| tv.value_cloned()));
        self.resident.set(TagValue::tagged(v, tag))
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn typ(&self) -> &Type {
        &self.typ
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R>) {
        self.slept.set();
        self.n.iter_mut().for_each(|n| n.sleep(ctx))
    }

    fn fuse(&mut self, ctx: &mut CompileCtx<R>) -> Result<Option<Node<R>>> {
        fusion::fuse_parts(self.n.iter_mut(), ctx)
    }

    super::typed_by_row!();

    fn view(&self) -> NodeView<'_, R> {
        K::view(self)
    }

    fn emit_clif(&self, cx: &mut BodyCx) -> Result<CompiledExpr> {
        K::emit(cx, &self.n)
    }
}

impl<R: Rt> ArrayRef<R> {
    fn typecheck0_with(
        &mut self,
        ctx: &mut CompileCtx<R>,
        child: &mut super::Child<'_, R>,
        check: bool,
    ) -> Result<()> {
        wrap!(self.source, child(&mut self.source, ctx))?;
        wrap!(self.i, child(&mut self.i, ctx))?;
        let source_typ = self.source.typ();
        if known_bytes(&ctx.env, source_typ)? {
            let byte = Type::Primitive(Typ::U8.into());
            wrap!(self, self.etyp.check_contains(&ctx.env, &byte))?;
        } else {
            let at = Type::Array(Arc::new(self.etyp.clone()));
            wrap!(self, at.check_contains(&ctx.env, source_typ))?;
        }
        match check {
            true => check_index(&ctx.env, &self.i),
            false => Ok(()),
        }
    }
}

impl<R: Rt> ArraySlice<R> {
    fn typecheck0_with(
        &mut self,
        ctx: &mut CompileCtx<R>,
        child: &mut super::Child<'_, R>,
        check: bool,
    ) -> Result<()> {
        wrap!(self.source, child(&mut self.source, ctx))?;
        let source_typ = self.source.typ();
        if check && !known_bytes(&ctx.env, source_typ)? {
            let at = Type::Array(Arc::new(Type::empty_tvar()));
            wrap!(self, at.check_contains(&ctx.env, source_typ))?;
        }
        // `typ` copied the source's type at compile; a source whose type
        // is decided in its typecheck0 (a select) is related here
        let Type::Set(members) = &self.typ else { unreachable!() };
        wrap!(self, members[0].check_contains(&ctx.env, source_typ))?;
        for i in [self.start.as_mut(), self.end.as_mut()].into_iter().flatten() {
            wrap!(i, child(i, ctx))?;
            if check {
                check_index(&ctx.env, i)?;
            }
        }
        Ok(())
    }
}

impl<R: Rt, K: SeqKind> SeqLit<R, K> {
    fn typecheck0_with(
        &mut self,
        ctx: &mut CompileCtx<R>,
        child: &mut super::Child<'_, R>,
        check: bool,
    ) -> Result<()> {
        for n in &mut self.n {
            wrap!(n, child(n, ctx))?
        }
        if !check {
            return Ok(());
        }
        let bottom = Type::Bottom;
        let mut ts: LPooled<Vec<&Type>> = LPooled::take();
        ts.push(&bottom);
        ts.extend(self.n.iter().map(|n| n.typ()));
        let union = wrap!(self, element_union(&ctx.env, &ts))?;
        if let Type::Array(e) | Type::List(e) = &self.typ
            && bind_element(e, &union)
        {
            return Ok(());
        }
        let rtype = match &union {
            Type::Bottom => K::typ(Type::empty_tvar()),
            t => K::typ(t.clone()),
        };
        self.typ.check_contains(&ctx.env, &rtype).map_err(|e| match &self.typ {
            Type::Array(slot) | Type::List(slot) => TVar::refusal_over(slot, &union, e),
            _ => e,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::time::{Duration, Instant};

    // each literal element carries a type of its own; equal primitives
    // are one member however many elements there are
    #[test]
    fn a_literals_element_union_is_linear() {
        let env = Env::default();
        let ts: Vec<Type> =
            (0..400_000).map(|_| Type::Primitive(Typ::I64.into())).collect();
        let refs: Vec<&Type> = iter::once(Type::BOTTOM).chain(ts.iter()).collect();
        let start = Instant::now();
        let union = element_union(&env, &refs).unwrap();
        let took = start.elapsed();
        assert!(matches!(union, Type::Primitive(p) if p == Typ::I64), "{union}");
        assert!(took < Duration::from_secs(2), "{took:?}");
    }
}
