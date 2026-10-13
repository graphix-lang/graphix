// gui-datatable-03: data_table sort keys come from formatted text.
//
// The widget's private SortKey cannot be reached from an integration test,
// so this file composes the exact production pieces: `format_value`
// (types.rs: a String bare, else `fmt_naked`), `numeric_key` (types.rs:
// `s.parse::<f64>()`) and `SortKey::cmp` (subscriptions.rs), copied as
// they stand at 7324207f, then sorts rows ascending as `sort_rows` does.
//
// Run (copied to stdlib/graphix-package-gui/tests/review_gui_datatable_03.rs):
//      timeout -s KILL 2400 cargo test -p graphix-package-gui \
//        --test review_gui_datatable_03 -- --nocapture
// Expected: every case sorts in value order.
// Observed: durations sort as [1.m, 2.s, 500.ms] (longest first),
//   u64 2^53+1 and 2^53 tie and keep table order (2^53+1 first),
//   strings sort as [nan, 5, inf, apple, zebra]: "nan"/"inf" are numbers.
use arcstr::ArcStr;
use netidx::publisher::Value;
use std::{cmp::Ordering, time::Duration};

fn format_value(v: &Value) -> ArcStr {
    match v {
        Value::Null => ArcStr::new(),
        Value::String(s) => s.clone(),
        _ => ArcStr::from(v.to_string_naked()),
    }
}

enum SortKey {
    Num(f64),
    Text(ArcStr),
}

impl SortKey {
    fn of(s: ArcStr) -> Self {
        match s.parse::<f64>().ok() {
            Some(n) => SortKey::Num(n),
            None => SortKey::Text(s),
        }
    }

    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (SortKey::Num(a), SortKey::Num(b)) => match (a.is_nan(), b.is_nan()) {
                (true, true) => Ordering::Equal,
                (true, false) => Ordering::Less,
                (false, true) => Ordering::Greater,
                (false, false) => a.total_cmp(b),
            },
            (SortKey::Num(_), SortKey::Text(_)) => Ordering::Less,
            (SortKey::Text(_), SortKey::Num(_)) => Ordering::Greater,
            (SortKey::Text(a), SortKey::Text(b)) => a.cmp(b),
        }
    }
}

fn sorted(vals: &[Value]) -> Vec<usize> {
    let keys: Vec<SortKey> = vals.iter().map(|v| SortKey::of(format_value(v))).collect();
    let mut order: Vec<usize> = (0..vals.len()).collect();
    order.sort_by(|&a, &b| keys[a].cmp(&keys[b]));
    order
}

#[test]
fn sort_keys_follow_values() {
    let mut fails = vec![];
    let durs = [
        Value::from(Duration::from_secs(60)),
        Value::from(Duration::from_secs(2)),
        Value::from(Duration::from_millis(500)),
    ];
    let texts: Vec<ArcStr> = durs.iter().map(format_value).collect();
    let got = sorted(&durs);
    println!("durations formatted {texts:?}, ascending rows {got:?}");
    if got != vec![2, 1, 0] {
        fails.push(format!("durations ascending {got:?}, want [2, 1, 0]"));
    }
    let big = [Value::U64((1 << 53) + 1), Value::U64(1 << 53)];
    let got = sorted(&big);
    println!("u64 2^53+1, 2^53 ascending rows {got:?}");
    if got != vec![1, 0] {
        fails.push(format!("big u64 ascending {got:?}, want [1, 0]"));
    }
    let strs = [
        Value::from("zebra"),
        Value::from("inf"),
        Value::from("5"),
        Value::from("nan"),
        Value::from("apple"),
    ];
    let got = sorted(&strs);
    println!("strings zebra, inf, 5, nan, apple ascending rows {got:?}");
    // numbers before text, text by bytes: 5, apple, inf, nan, zebra
    if got != vec![2, 4, 1, 3, 0] {
        fails.push(format!("strings ascending {got:?}, want [2, 4, 1, 3, 0]"));
    }
    assert!(fails.is_empty(), "{fails:#?}");
}
