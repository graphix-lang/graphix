#!/bin/sh
# An input's end leaves an open cell in the type of a binding it made: a
# cell settle_input binds to a conjunct's witness takes a fresh copy of the
# conjunct's generic cell (Witness::shared), and the walk that settles the
# input's cells never reaches the copy.
#
# command:
#   sh design/review-2026-11-16/repro/t-settle-input-end-leaves-witness-copy-open.sh
#
# expected: `fs` settled when its input ended, as `let fs = [|w: Array<'e>| w, |w| w]`
#   is (`Array<fn(w: Array<'e: _>) -> Array<'e: _>>`):
#     -: Array<fn(w: Array<'a1: _>) -> Array<'a1: _>>
#   and the later call refused as a settled binding's is ("settled when its
#   input ended; declare fs's type where it is bound").
#
# observed:
#   -: Array<fn(w: Array<'a1: unbound>) -> Array<'a1: unbound>>
#   [Abstract(|w| f(w)), Abstract(|w| f(w))]
#   error: in expr fs[0]$([1])
#   ... this input would decide a type of an earlier input: annotate `fs` ...
G=${GRAPHIX:-/home/eric/tmp/review-2026-11-16/bin/graphix}
printf '%s\n' \
  "let f = 'a0: Array<'a1> |x0: 'a0| x0" \
  'let fs = [|w| f(w), |w| f(w)]' \
  'fs' \
  'fs[0]$([1])' \
  | timeout -s KILL 30 "$G" --no-cache 2>&1
