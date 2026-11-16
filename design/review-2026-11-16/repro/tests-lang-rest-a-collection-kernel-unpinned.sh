#!/bin/sh
# Title: collection.rs::bottom_source_runs_no_slot and
#        collection_is_quiet_on_a_stale_resize claim kernel behaviour, but run_delta
#        asserts nothing about fusion
# Command: sh design/review-2026-11-16/repro/tests-lang-rest-a-collection-kernel-unpinned.sh
# Expected (for the pin to be cheap): each map the fixtures speak of builds under
#           `#[native]` today, so `#[native]` can be written on it.
# Observed: both builds succeed (no "#[native] expression did not fully fuse"), so
#           the maps fuse now; nothing in the fixtures fails if they stop fusing.
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
cat > "$D/a.gx" <<'EOF'
let result = {
    let t = array::iter([0, 1, 2]);
    let phantom = 0;
    let retained = 0;
    let a = {
        catch(e) phantom <- e ~ phantom + 1;
        #[native] array::map(array::map([1, 2], |z| z + t), |x| {
            let e: [i64, Error<`E>] = error(`E);
            e? + x
        })
    };
    let b = {
        catch(e) retained <- e ~ retained + 1;
        let src = select t { 1 => never<Array<i64>>(), _ => [1, 2] };
        #[native] array::map(src, |x| {
            let e: [i64, Error<`E>] = select t { 1 => error(`E), n => n };
            e? + x
        })
    };
    (phantom, retained)
};
result
EOF
cat > "$D/b.gx" <<'EOF'
let result = {
    let n = 0;
    n <- select n { 8 => never(), v => v + 1 };
    let k = uniq(select n { 0 => 0, 1 => 0, 2 => 0, 3 => 2, _ => 2 });
    let s = uniq(select n { 0 => 1, 1 => 1, 2 => 0, 3 => 0, _ => 1 });
    let xs = select k { 0 => [1, 2, 3], _ => [1] };
    select s {
        0 => array::len(xs) - 100,
        _ => { let m = #[native] array::map(xs, |x| x); array::len(m) * 100 + (m ~ n) }
    }
};
result
EOF
for p in a b; do
  echo "== $p"
  timeout -s KILL 30 "$G" --no-cache --expand "$D/$p.gx" > "$D/out" 2>&1
  echo "graphix exit $?"
  tail -2 "$D/out"
done
rm -rf "$D"
