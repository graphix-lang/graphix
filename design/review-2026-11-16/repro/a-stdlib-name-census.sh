#!/bin/sh
# The names an author reaches for first, checked one a line
#
# Each line below the marker is a one-expression program. The loop runs
# `graphix --check` over each and prints OK or NO with the check's last line.
#
# command:
#   sh design/review-2026-11-16/repro/a-stdlib-name-census.sh /path/to/graphix
# observed at HEAD e1be2fbf2: 3 of 128 check (rand::rand, opt::or_default and
#   min; nothing of array, list, map, str, sys or math). Many are only guesses;
#   the ones that say something about the design:
#   - str::starts_with("ab", "a"), str::contains("ab", "a"),
#     str::split("a,b", ","), str::join(",", [..]): "missing required argument
#     pfx/part/pat/sep" (the operand is a required label and the subject is last,
#     where array, list, map and opt take the subject first and positionally)
#   - array::reverse, contains, sort_by, is_empty, first, last: not defined
#     (list has reverse, is_empty, head)
#   - map::keys, values, contains_key, to_array, from_array, is_empty, find: not defined
#   - math::abs(-1), math::min(1, 2), math::pow(2, 3): "f64 does not contain i64"
#   - sys::env*, sys::cwd, sys::time::format/parse/year, sys::fs::exists/copy/rename,
#     re::replace: not defined
G=${1:-graphix}
T=$(mktemp -d)
sed '1,/^# ---- programs ----$/d' "$0" | while IFS= read -r line; do
  printf '%s\n' "$line" > "$T/g.gx"
  if out=$(timeout -s KILL 30 "$G" --check "$T/g.gx" 2>&1); then
    echo "OK   $line"
  else
    echo "NO   $line   :: $(echo "$out" | tail -1 | cut -c1-100)"
  fi
done
rm -rf "$T"
exit 0
# ---- programs ----
array::reverse([1,2,3])
array::contains([1,2,3], 2)
array::any([1,2,3], |x| x > 2)
array::all([1,2,3], |x| x > 2)
array::sum([1,2,3])
array::min([1,2,3])
array::max([1,2,3])
array::sort_by([1,2,3], |x| x)
array::take([1,2,3], 2)
array::skip([1,2,3], 2)
array::slice([1,2,3], 0, 2)
array::first([1,2,3])
array::last([1,2,3])
array::position([1,2,3], |x| x == 2)
array::index_of([1,2,3], 2)
array::is_empty([1,2,3])
array::range(0, 3)
array::chunks([1,2,3], 2)
array::group_by([1,2,3], |x| x % 2)
array::partition([1,2,3], |x| x > 1)
array::count([1,2,3], |x| x > 1)
array::repeat(1, 3)
array::get([1,2,3], 1)
array::join([1,2,3], ",")
array::nth([1,2,3], 1)
array::head([1,2,3])
array::for_each([1,2,3], |x| println(x))
list::push([<1>], 2)
list::last([<1>])
list::contains([<1>], 1)
list::dedup([<1>])
list::rotate([<1>])
list::window(#n: 2, [<1>], 2)
list::push_front([<1>], 2)
map::keys({"a" => 1})
map::values({"a" => 1})
map::contains_key({"a" => 1}, "a")
map::contains({"a" => 1}, "a")
map::has({"a" => 1}, "a")
map::from_array([("a", 1)])
map::from_pairs([("a", 1)])
map::to_array({"a" => 1})
map::entries({"a" => 1})
map::is_empty({"a" => 1})
map::find({"a" => 1}, |kv| kv.1 == 1)
map::find_map({"a" => 1}, |kv| null)
map::flat_map({"a" => 1}, |kv| {"b" => 2})
map::merge({"a" => 1}, {"b" => 2})
map::update({"a" => 1}, "a", |v| v + 1)
str::chars("ab")
str::repeat("ab", 2)
str::is_empty("")
str::find(#pat: "a", "ab")
str::index_of(#pat: "a", "ab")
str::pad_left(#n: 5, "ab")
str::from(1)
str::to_string(1)
str::format("{}", 1)
str::trim_matches(#pat: "a", "ab")
str::reverse("ab")
str::starts_with("ab", "a")
str::contains("ab", "a")
str::split("a,b", ",")
str::join(",", ["a", "b"])
str::parse_int("1")
str::to_i64("1")
math::abs(-1)
math::min(1, 2)
math::pow(2, 3)
math::floor(1.5) + 1
math::to_int(1.5)
math::round_to(1.555, 2)
sys::env("HOME")
sys::env::var("HOME")
sys::env::get("HOME")
sys::getenv("HOME")
sys::time::format(sys::time::now(null), "%Y")
sys::time::parse("2020-01-01")
sys::time::sleep(duration:1.s)
sys::time::unix(sys::time::now(null))
sys::time::year(sys::time::now(null))
sys::fs::exists("/tmp")
sys::fs::copy("/tmp/a", "/tmp/b")
sys::fs::rename("/tmp/a", "/tmp/b")
sys::fs::append(#path: "/tmp/a", "x")
sys::fs::read_dir("/tmp")
sys::fs::list("/tmp")
sys::fs::read_lines("/tmp/a")
sys::fs::read("/tmp/a")
sys::fs::write(#path: "/tmp/a", "x")
sys::fs::read_to_string("/tmp/a")
sys::hostname()
sys::pid()
sys::cwd()
sys::process::run("ls")
sys::process::exec("ls")
sys::process::output(sys::process::options("ls"))
sys::io::read_line(sys::io::stdin(null))
sys::io::println("x")
sys::io::write(sys::io::stdout(null), "x")
json::parse("1")
json::write(1)
json::to_string(1)
json::from_str("1")
toml::parse("a = 1")
toml::write({a: 1})
re::replace(#pat: "a", #rep: "b", "aa")
re::match(#pat: "a", "aa")
re::find_all(#pat: "a", "aa")
rand::int(0, 10)
rand::rand(#clock: null)
rand::uuid()
opt::unwrap_or(null, 1)
opt::get_or(null, 1)
opt::or_default(null, 1)
opt::some(1)
opt::is_null(null)
buffer::to_hex(buffer::from_string("a"))
buffer::to_base64(buffer::from_string("a"))
buffer::slice(buffer::from_string("abc"), 0, 2)
core::hash("a")
is_ok(1)
ok(1)
not(true)
abs(-1)
len([1])
to_string(1)
min(1, 2)
