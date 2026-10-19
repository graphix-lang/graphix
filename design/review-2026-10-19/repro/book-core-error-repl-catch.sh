#!/bin/bash
# Title: book/src/core/error.md's REPL transcripts are refused by the REPL, and print stale forms
# Command: bash design/review-2026-10-19/repro/book-core-error-repl-catch.sh
# Expected (error.md): `catch(e) println(e)` on one line, then `a[15]?` prints the ErrChain
#   as error:[["cause", null], ["error", ["ArrayIndexError", ...]], ...]
# Observed: "this error reaches a catch that saw no error when it was checked: annotate the
#   catch with the errors it takes"; same for the div0 and `v?` transcripts. With the catch
#   annotated, the value prints as error:{cause: null, error: `ArrayIndexError(..), ..}
#   (struct and variant forms, no i32: prefix), and error(`Foo) prints error:`Foo.
G=/home/eric/tmp/review-2026-10-19/bin/graphix
run() { printf "$1" | timeout -s KILL 4 $G --no-cache 2>&1 | grep -v '^Welcome\|^Press\|^$'; echo ---; }
run 'let a = [1, 2, 3, 4]\ncatch(e) println(e)\na[15]?\n'
run 'let div0 = { catch(e) println(e ~ "never triggered"); |x| (x /? 0)? }\ncatch(e) println(e)\ndiv0(0)\n'
run 'catch(e) println((e.0).error)\nlet m = {"a" => 1}\nlet v = map::get(m, "b")\nv?\n'
run 'let a = [1, 2, 3, 4]\ncatch(e: Error<ErrChain<`ArrayIndexError(string)>>) println(e)\na[15]?\n'
run 'error(`Foo)\n'
