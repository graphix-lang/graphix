//! `on e { .. }`: a block clocked to one event (`design/on_blocks.md`).
//! Every binding outside the block that its text reads is its value when
//! the event fires, a constant acts once an event, and nothing is paid
//! late.

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

// `on x { y <- z }` is `y <- x ~ z`.
run!(
    a_connect_is_sampled_on_the_event,
    clocked!(
        "let z = step * 10; let y = -1; on k { y <- z }; array::group(y, |i, _| i == 4)"
    ),
    prints("[i64:-1, i64:20, i64:50, i64:70]")
);

// A read of a binding the block also writes is its value before the event.
run!(
    a_binding_read_and_written_counts_the_events,
    clocked!("let n = 0; on k { n <- n + 1 }; array::group(n, |i, _| i == 4)"),
    prints("[i64:0, i64:1, i64:2, i64:3]")
);

// What reads no variable is fired by the event: a connect's constant, a
// constant in an arm, the block's own value.
run!(
    a_constant_acts_once_an_event,
    clocked!("let n = 5; on k { n <- 0 }; array::group(n, |i, _| i == 4)"),
    prints("[i64:5, i64:0, i64:0, i64:0]")
);

run!(
    the_blocks_value_fires_at_each_event,
    clocked!("let r = on k { 7 }; array::group(r, |i, _| i == 3)"),
    prints("[i64:7, i64:7, i64:7]")
);

// A read with no value at the event stops what reads it and nothing else,
// and is not paid when the value arrives.
run!(
    a_read_with_no_value_stops_only_what_reads_it,
    clocked!(
        "let late = never(); late <- select step { 6 => 100, _ => never() }; \
         let a = 0; let b = 0; on k { a <- a + 1; b <- late }; \
         array::group((a, b), |i, _| i == 4)"
    ),
    prints("[[i64:0, i64:0], [i64:1, i64:0], [i64:2, i64:0], [i64:3, i64:100]]")
);

// `on let` names the event's value.
run!(
    the_events_value_is_named,
    clocked!(
        "let ev = select step { 3 => (1, \"a\"), 6 => (2, \"b\"), _ => never() }; \
         let got = (0, \"\"); on let (n, s) = ev { got <- (n * 10, s) }; \
         array::group(got, |i, _| i == 3)"
    ),
    prints(r#"[[i64:0, ""], [i64:10, "a"], [i64:20, "b"]]"#)
);

// A call none of whose arguments fires is issued by the event.
run!(
    a_call_over_constants_is_issued_at_each_event,
    clocked!(
        "let n = 0; let bump = |x| n <- x ~ n + x; on k { bump(10) }; \
         array::group(n, |i, _| i == 4)"
    ),
    prints("[i64:0, i64:10, i64:20, i64:30]")
);

// The seq rule: a lambda written in the block reads the event's values
// too, so nothing in the block moves between two events.
run!(
    a_lambda_written_in_the_block_reads_the_events_values,
    clocked!(
        "let scale = step; let r = on k { array::map([1, 2], |x| x * scale) }; \
         array::group(r, |i, _| i == 3)"
    ),
    prints("[[i64:2, i64:4], [i64:5, i64:10], [i64:7, i64:14]]")
);

// State inside the block is in the clock's domain: a count there counts
// the events, where `k ~ count(step)` counts every step.
run!(
    a_stateful_call_in_the_block_sees_the_events,
    clocked!(
        "let c = on k { count(step) }; let d = k ~ count(step); \
         array::group((c, d), |i, _| i == 3)"
    ),
    prints("[[i64:1, i64:3], [i64:2, i64:6], [i64:3, i64:8]]")
);

// A read through a reference follows its target, and is taken at the
// event as a variable is: the write does not feed itself.
run!(
    a_read_through_a_reference_is_taken_at_the_event,
    clocked!(
        "let v = 1; let r = &mut v; on k { *r <- *r + 10 }; array::group(v, |i, _| i == 4)"
    ),
    prints("[i64:1, i64:11, i64:21, i64:31]")
);

// `live(e)` reads where it stands, as in a seq.
run!(
    live_reads_between_events,
    clocked!("let r = on k { live(step) }; array::group(r, |i, _| i == 5)"),
    prints("[i64:0, i64:1, i64:2, i64:3, i64:4]")
);

// A handler: the answer and the writes of the arm the event selects, each
// once an event.
run!(
    a_handler_answers_and_writes_in_the_events_cycle,
    clocked!(
        "let m = 0; let s = 0; \
         let h = |c| on c { select c { \
           2 => { m <- m + 1; `Stop }, \
           5 => { s <- 9; m <- 0; `Stop }, \
           _ => `Continue } }; \
         let ans = h(k ~ step); \
         array::group((k ~ ans, m, s), |i, _| i == 5)"
    ),
    prints(
        r#"[["Stop", i64:0, i64:0], ["Stop", i64:1, i64:0], ["Stop", i64:1, i64:0], ["Stop", i64:0, i64:9], ["Continue", i64:0, i64:9]]"#
    )
);

// A read through a reference is its target's value at the event wherever
// it stands, as a variable's is: in a lambda the block writes, which a
// later fire of the target does not call again.
run!(
    a_reference_read_in_a_lambda_is_taken_at_the_event,
    clocked!(
        "let r = &step; let out = -1; on k { let g = |q: i64| q + *r; out <- g(0) }; \
         array::group(out, |i, _| i == 4)"
    ),
    prints("[i64:-1, i64:2, i64:5, i64:7]")
);

// And in an arm that wakes between two events: the arm slept through the
// events at 5 and 7, and reads the value they took.
run!(
    a_reference_read_in_an_arm_that_wakes_late_is_the_last_events,
    clocked!(
        "let r = &step; let out = -1; \
         on k { out <- select live(step) % 2 { 0 => *r, _ => -5 } }; \
         array::group(out, |i, _| i == 9)"
    ),
    prints("[i64:-1, i64:-5, i64:2, i64:-5, i64:2, i64:-5, i64:5, i64:-5, i64:7]")
);

// An on block nested in another is clocked by its own event alone.
run!(
    a_nested_block_takes_its_own_clock,
    clocked!(
        "let j = select step { 3 => null, _ => never() }; let n = 0; \
         on k { on j { n <- step } }; array::group(n, |i, _| i == 2)"
    ),
    prints("[i64:0, i64:3]")
);

run!(
    a_catch_in_the_block_is_refused,
    "{ let k = 1; let x = 0; on k { catch(e) x <- 0; x <- 1 } }",
    refused("catch is not allowed inside an on block")
);

run!(
    an_until_in_the_block_is_refused,
    "{ let k = 1; let x = 0; on k { until x > 0; x <- 1 } }",
    refused("`until` is only legal in a seq block")
);

run!(
    on_is_no_name,
    "{ let on = 1; on }",
    refused("can't use keyword as a function or variable name")
);

run!(
    a_block_with_no_event_is_refused,
    "{ let x = 0; on { x <- 1 } }",
    refused("an on block names the event that clocks it")
);
