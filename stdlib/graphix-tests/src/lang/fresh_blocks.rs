//! `fresh e { .. }`: a block that starts over at each fire of an event and
//! runs live between (`design/fresh_blocks.md`). Starting over is a birth:
//! the block's state is new, its constants fire, and what it reads fires
//! with the value that stands.

use graphix_package_core::{
    run,
    testing::{prints, refused},
};

/// A clock of eight steps and an event `k` at steps 2, 5 and 7.
macro_rules! clocked {
    ($body:literal) => {
        concat!(
            "{ let step = 0; step <- select step { n if n < 8 => n + 1, _ => never() }; ",
            "let k = select step { 2 => null, 5 => null, 7 => null, _ => never() }; ",
            $body,
            " }"
        )
    };
}

// State starts over at each event, and the block runs live between them.
run!(
    state_starts_over_and_the_block_runs_live,
    clocked!("let v = fresh k { count(step) }; array::group(v, |i, _| i == 7)"),
    prints("[i64:1, i64:2, i64:3, i64:1, i64:2, i64:1, i64:2]")
);

// A count of the event itself is always one, where `on k { count(k) }`
// counts the events.
run!(
    a_count_of_the_event_is_always_one,
    clocked!(
        "let a = fresh k { count(k) }; let b = on k { count(k) }; \
         array::group((a, b), |i, _| i == 3)"
    ),
    prints("[[i64:1, i64:1], [i64:1, i64:2], [i64:1, i64:3]]")
);

// Before its event first fires the block is nothing and does nothing; a
// constant in it acts at each start.
run!(
    a_constant_acts_at_each_start_and_not_before,
    clocked!(
        "let out = 0; fresh k { out <- 7 }; \
         array::group((step ~ out, count(out)), |i, _| i == 9)"
    ),
    prints(
        "[[i64:0, i64:1], [i64:0, i64:1], [i64:0, i64:1], [i64:7, i64:2], [i64:7, i64:2], \
         [i64:7, i64:2], [i64:7, i64:3], [i64:7, i64:3], [i64:7, i64:4]]"
    )
);

// What the block reads fires with the value that stands: `lvl` was last
// written at step 1, and a stateful builtin over it has it at each start.
run!(
    what_it_reads_fires_with_the_value_that_stands,
    clocked!(
        "let lvl = 0; lvl <- select step { 1 => 5, _ => never() }; \
         let v = fresh k { uniq(lvl) }; array::group(v, |i, _| i == 3)"
    ),
    prints("[i64:5, i64:5, i64:5]")
);

// `fresh let` names the event's value.
run!(
    the_event_is_named,
    clocked!(
        "let v = fresh let (a, b) = k ~ (step, 2) { a * b }; array::group(v, |i, _| i == 3)"
    ),
    prints("[i64:4, i64:10, i64:14]")
);

// The block's state is new wherever it stands: an arm inside the block
// that slept through a start is not the arm of the block's last life.
// The even arm counts 1, 2 in the first life and 1 again in each later
// one.
run!(
    a_sleeping_arm_in_the_block_starts_over_too,
    clocked!(
        "let v = fresh k { select step % 2 { 0 => count(step), _ => -1 } }; \
         array::group(v, |i, _| i == 7)"
    ),
    prints("[i64:1, i64:-1, i64:2, i64:-1, i64:1, i64:-1, i64:1]")
);

// An on block in a fresh block keeps its state for one life: keys since
// the block last started. `key` is bottom between its fires.
run!(
    an_on_block_inside_counts_within_one_life,
    clocked!(
        "let key = select step { 3 => null, 8 => null, _ => never() }; \
         let v = fresh k { let n = 0; on key { n <- n + 1 }; n }; \
         array::group(v, |i, _| i == 5)"
    ),
    prints("[i64:0, i64:1, i64:0, i64:0, i64:1]")
);

// In a fresh block a capture is a level: a start fires it with the value
// that stands. An on block clocked by a key held in a binding runs once at
// each start, with no key pressed since step 1.
run!(
    a_capture_that_stands_fires_at_the_start,
    clocked!(
        "let key = 0; key <- select step { 1 => 10, _ => never() }; \
         let v = fresh k { let n = 0; on key { n <- n + 1 }; n }; \
         array::group(v, |i, _| i == 6)"
    ),
    prints("[i64:0, i64:1, i64:0, i64:1, i64:0, i64:1]")
);

// A later start takes the kernels the first one built, and a map crosses
// into a kernel unread, its type kept as written.
run!(
    a_later_start_takes_the_kernels_of_the_first,
    clocked!(
        "let v = fresh k { let n = 0; \
         on k { n <- n + map::fold({\"a\" => 1, \"b\" => 2}, 0, |acc, (_, x)| acc + x) }; n }; \
         array::group(v, |i, _| i == 6)"
    ),
    prints("[i64:0, i64:3, i64:0, i64:3, i64:0, i64:3]")
);

// A fresh block in an on block is its own: the outer block samples
// nothing in it, and its body runs live.
run!(
    a_fresh_block_in_an_on_block_runs_live,
    clocked!(
        "let j = select step { 3 => null, 6 => null, _ => never() }; let n = 0; \
         on j { fresh k { n <- step } }; array::group(n, |i, _| i == 8)"
    ),
    prints("[i64:0, i64:2, i64:3, i64:4, i64:5, i64:6, i64:7, i64:8]")
);

// A fresh block in a fresh block starts over with it.
run!(
    a_nested_block_starts_over_with_its_outer,
    clocked!(
        "let j = select step { 3 => null, _ => never() }; \
         let v = fresh k { let r = fresh any(k, j) { count(step) }; r + 100 }; \
         array::group(v, |i, _| i == 7)"
    ),
    prints("[i64:101, i64:101, i64:102, i64:101, i64:102, i64:101, i64:102]")
);

// In a select arm the event is the arm's bind: each visit starts the
// arm's body over, right at the entry and live after it. `data` moved to
// 2 while the arm slept, and the second visit shows it at once.
run!(
    an_arm_starts_over_at_each_visit,
    "{ let step = 0; step <- select step { n if n < 8 => n + 1, _ => never() }; \
       let tab: [`A, `B] = `A; \
       tab <- select step { 1 => `B, 3 => `A, 5 => `B, 7 => `A, _ => never() }; \
       let data = 1; data <- select step { 2 => 2, 4 => 3, 6 => 4, _ => never() }; \
       let view = select uniq(tab) { \
         t@ `A => fresh t { (uniq(data), count(step)) }, \
         `B => never() }; \
       array::group(view, |i, _| i == 5) }",
    prints(
        "[[i64:1, i64:1], [i64:1, i64:2], [i64:2, i64:1], [i64:3, i64:2], [i64:4, i64:1]]"
    )
);

// A `?` in the block raises to the catch around it, at each start.
run!(
    a_raise_in_the_block_reaches_the_catch_around_it,
    clocked!(
        "let hits = 0; catch(e) hits <- e ~ hits + 1; fresh k { error(`Bad)? }; \
         array::group(hits, |i, _| i == 4)"
    ),
    prints("[i64:0, i64:1, i64:2, i64:3]")
);

run!(
    a_block_with_no_event_is_refused,
    "{ let x = 0; fresh { x } }",
    refused("a fresh block names the event it starts over at")
);

run!(
    fresh_is_no_name,
    "{ let fresh = 1; fresh }",
    refused("can't use keyword as a function or variable name")
);

run!(
    live_in_the_block_is_refused,
    "{ let k = 1; let x = 0; fresh k { live(x) } }",
    refused("it means nothing outside a seq block")
);

run!(
    an_until_in_the_block_is_refused,
    "{ let k = 1; let x = 0; fresh k { until x > 0; x } }",
    refused("`until` is only legal in a seq block")
);
