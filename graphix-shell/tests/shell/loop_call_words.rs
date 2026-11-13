//! A loop's body keeps, a slot, a nested loop's one word and a called
//! kernel's block, and nothing for a call beside them: `g`'s fold claims
//! its length word and nothing for `f`, and the region's map claims its
//! own length word and the anchor of `g`'s blocks.

use super::common;
use common::graphix;

const PROGRAM: &str = "\
let f = |x: i64| x * 2 + 1;
let g = |ys: Array<i64>| array::fold(ys, 0, |a, y| a + f(y));
let n = sys::time::after_idle(duration:1.ms, 4);
let r = array::map([[1, 2], [n]], |ys| g(ys) + f(n));
sys::exit(select r { [17, 18] => 0, _ => 1 })
";

#[test]
fn a_call_in_a_loop_claims_its_callee_block_alone() {
    let dir = tempfile::tempdir().expect("tempdir");
    let ran =
        graphix(dir.path(), PROGRAM, &["--no-cache"], &[("GRAPHIX_DBG_KERNELS", "1")]);
    assert!(ran.status.success(), "the program computed the wrong value: {}", ran.stderr);
    let words = |name: &str, field: &str| -> usize {
        let defined = ran.stderr.lines().filter_map(|l| {
            let (kernel, layout) = l.strip_prefix("KERNEL DEFINED ")?.split_once(": ")?;
            let word = layout.split(' ').find_map(|w| w.strip_prefix(field))?;
            kernel.starts_with(name).then(|| word.parse::<usize>().expect("a count"))
        });
        defined.sum()
    };
    assert_eq!(words("f", "site_words="), 0, "`f` keeps no state:\n{}", ran.stderr);
    assert_eq!(words("g", "site_words="), 1, "`g` keeps its fold's length alone");
    assert_eq!(
        words("region_", "state_words="),
        2,
        "the map keeps its length and the anchor of `g`'s blocks alone"
    );
}
