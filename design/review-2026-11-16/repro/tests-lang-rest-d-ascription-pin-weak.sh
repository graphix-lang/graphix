#!/bin/sh
# lang::errors::catch_ascription_refuses_open_raise / _open_callback pin only
# "does not contain" and call with `B, which a call-site check refuses too.
# The rule (a declared error parameter stays rigid) refuses the definition:
# the same refusal stands with a call the handler fits (`A, or `|| 1`).
# Command: sh tests-lang-rest-d-ascription-pin-weak.sh
# Observed: all four print "this error reaches a catch whose handler was
#   checked to take Error<ErrChain<`A>>: type mismatch ..."; the pins would
#   pass equally if only the `B call were refused.
BIN=/home/eric/tmp/review-2026-11-16/bin/graphix
D=$(mktemp -d)
for arg in '`B' '`A'; do
  printf 'let run = |r: Result<i64, '"'"'e>| {\n    catch(e: Error<ErrChain<`A>>) never(e);\n    r? + 1\n};\nrun(error(%s))\n' "$arg" > $D/r.gx
  timeout -s KILL 60 $BIN --no-cache --check $D/r.gx 2>&1 | tail -1
done
for arg in '|| error(`B)?' '|| 1'; do
  printf 'let run = |g: fn() -> i64 throws '"'"'e| {\n    catch(e: Error<ErrChain<`A>>) never(e);\n    g() + 1\n};\nrun(%s)\n' "$arg" > $D/c.gx
  timeout -s KILL 60 $BIN --no-cache --check $D/c.gx 2>&1 | tail -1
done
rm -rf $D
