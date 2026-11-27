//! The await twin: a seq whose steps `await` a stream, beside the answers
//! the rule says its awaits give (`design/seq_blocks.md`), with an
//! in-program verdict ([`crate::TWIN_TAG`]).
//!
//! What an `await` gives is decided in the seq machine and its fire
//! tracker, which every engine runs the same, so no comparison of engines
//! sees a bug there. The generator draws a schedule (the cycles the
//! stream fires in, the cycles the seq is triggered in, the seq's steps),
//! works out by the rule what every await of every run takes, and the
//! program compares its runs with that.
//!
//! The rule as modelled: a run started in cycle `t` is owed the stream's
//! fires from `t` on. An `await` is entered the cycle after the step
//! before it completes (the first, the cycle after `t`). Entered in cycle
//! `e`, it takes the latest fire the run has not read up to `e` and
//! completes in `e`; with none it takes the next fire and completes
//! then. An `until live(c) >= x` completes in cycle `x`.

use super::chance;
use crate::{mutate::Rng, schedule::Schedule};

/// A way the model is made wrong, to measure what the twin sees.
#[derive(Clone, Copy, PartialEq, Debug)]
#[cfg_attr(not(test), allow(dead_code))]
pub(crate) enum Fault {
    None,
    /// A run is owed a fire from before it was started.
    Leftover,
    /// Two awaits in a row may take one fire.
    Shared,
}

#[derive(Clone, Copy)]
enum Step {
    Await,
    /// `until live(c) >= x`, with `x` this many cycles after the step
    /// before it completes.
    Wait(u64),
}

/// One run: the cycle each wait ends in, the fire each await takes, and
/// the cycle the last step completes in. `None` where an await never
/// completes.
fn run(
    fires: &[u64],
    t: u64,
    steps: &[Step],
    fault: Fault,
) -> Option<(Vec<u64>, Vec<u64>, u64)> {
    // every fire up to `read` is read, or not the run's; `done` is the
    // cycle the step before completed in
    let mut read = match fault {
        Fault::Leftover => None,
        Fault::None | Fault::Shared => t.checked_sub(1),
    };
    let mut done = t;
    let (mut waits, mut got) = (Vec::new(), Vec::new());
    for s in steps {
        match s {
            Step::Wait(n) => {
                done += n;
                waits.push(done)
            }
            Step::Await => {
                let entered = done + 1;
                let unread = |f: &&u64| read.is_none_or(|r| **f > r);
                let pending =
                    fires.iter().filter(unread).filter(|f| **f <= entered).max();
                let fire = match pending {
                    Some(f) => *f,
                    None => *fires.iter().find(|f| **f > entered)?,
                };
                got.push(fire);
                done = fire.max(entered);
                read = match fault {
                    Fault::Shared => done.checked_sub(1),
                    Fault::None | Fault::Leftover => Some(done),
                };
            }
        }
    }
    Some((waits, got, done))
}

pub(crate) fn gen_await_twin_faulty(rng: &mut Rng, fault: Fault) -> String {
    // the stream is written in cycle `w` and fires in `w + 1` with `w`
    let mut writes: Vec<u64> = Vec::new();
    let mut w = 1 + rng.below(4) as u64;
    while w < 90 {
        writes.push(w);
        w += match rng.below(10) {
            0..=3 => 1,
            4..=6 => 2,
            _ => 3 + rng.below(6) as u64,
        };
    }
    let fires: Vec<u64> = writes.iter().map(|w| w + 1).collect();
    let steps: Vec<Step> = (0..rng.below(4))
        .map(|_| match chance(rng, 0.3) {
            true => Step::Wait(2 + rng.below(5) as u64),
            false => Step::Await,
        })
        .chain([Step::Await])
        .collect();
    // each run as it will go, and as the model under `fault` says it goes
    let mut runs: Vec<(u64, Vec<u64>, Vec<u64>)> = Vec::new();
    let mut t = 2 + rng.below(8) as u64;
    for _ in 0..1 + rng.below(3) {
        let Some((waits, _, done)) = run(&fires, t, &steps, Fault::None) else { break };
        let Some((_, said, _)) = run(&fires, t, &steps, fault) else { break };
        runs.push((t, waits, said));
        t = done + 3 + rng.below(8) as u64;
    }
    let end = t + 4;
    let per_run = |of: &dyn Fn(usize) -> String| match runs.len() {
        0 | 1 => of(0),
        n => {
            let arms: String =
                (0..n - 1).map(|i| format!("{i} => {}, ", of(i))).collect();
            format!("select t {{ {arms}_ => {} }}", of(n - 1))
        }
    };
    let (mut body, mut vals) = (String::new(), Vec::new());
    let mut wait = 0;
    for s in &steps {
        match s {
            Step::Wait(_) => {
                let k = wait;
                let ends = per_run(&|i| runs.get(i).map_or(0, |r| r.1[k]).to_string());
                body.push_str(&format!("until live(c) >= {ends}; "));
                wait += 1
            }
            Step::Await => {
                let v = format!("v{}", vals.len());
                body.push_str(&format!("let {v} = await(ev); "));
                vals.push(v)
            }
        }
    }
    let arms = |at: &mut dyn Iterator<Item = (u64, String)>| {
        at.map(|(c, v)| format!("{c} => {v}, ")).collect::<String>()
    };
    let stream = arms(&mut writes.iter().filter(|w| **w < end).map(|w| (*w, "c".into())));
    let triggers = arms(&mut runs.iter().enumerate().map(|(i, r)| (r.0, i.to_string())));
    let said = runs
        .iter()
        .map(|(_, _, said)| {
            let row: Vec<String> = said.iter().map(|f| (f - 1).to_string()).collect();
            format!("[{}]", row.join(", "))
        })
        .collect::<Vec<_>>()
        .join(", ");
    let code = format!(
        "{{
  let c = 0;
  c <- select c < {end} {{ true => c + 1, false => never() }};
  let ev = never<i64>();
  ev <- select c {{ {stream}_ => never() }};
  let t = select c {{ {triggers}_ => never() }};
  let got = seq t {{ {body}[{vals}] }};
  let runs: Array<Array<i64>> = [];
  runs <- got ~ array::push(runs, got);
  let said: Array<Array<i64>> = [{said}];
  select c {{
    {end} => select runs == said {{ true => `Held, false => `{tag} }},
    _ => never()
  }}
}}",
        vals = vals.join(", "),
        tag = crate::TWIN_TAG,
    );
    let sched =
        Schedule { max_cycles: end + 64, max_events: 4096, ..Schedule::default() };
    sched.render(&code)
}

/// An await twin: it settles on the tag where a run's awaits do not give
/// what the rule says.
pub fn gen_await_twin(rng: &mut Rng) -> String {
    gen_await_twin_faulty(rng, Fault::None)
}

#[cfg(test)]
mod test {
    use super::*;

    /// The model: a fire from before the run is not the run's, one from
    /// its trigger's cycle is, the latest unread fire is taken at an
    /// entry, and one fire completes one await.
    #[test]
    fn the_model_follows_the_rule() {
        let two = [Step::Await, Step::Await];
        // fires at 3 and 9; the run starts at 5: the first await waits for 9
        assert_eq!(run(&[3, 9], 5, &[Step::Await], Fault::None).unwrap().1, [9]);
        // a fire in the trigger's cycle is the run's
        assert_eq!(run(&[5, 9], 5, &[Step::Await], Fault::None).unwrap().1, [5]);
        // a wait until 12 leaves 7 and 9 unread: the latest is taken
        let waited = [Step::Wait(7), Step::Await];
        assert_eq!(run(&[7, 9, 20], 5, &waited, Fault::None).unwrap().1, [9]);
        // two awaits take two fires, and one fire leaves the second waiting
        assert_eq!(run(&[6, 7], 5, &two, Fault::None).unwrap().1, [6, 7]);
        assert!(run(&[6], 5, &two, Fault::None).is_none());
        // the faults say otherwise
        assert_eq!(run(&[3, 9], 5, &[Step::Await], Fault::Leftover).unwrap().1, [3]);
        assert_eq!(run(&[6, 9], 5, &two, Fault::Shared).unwrap().1, [6, 6]);
    }
}
