; -- Comments -------------------------------------------------------
(line_comment) @comment.line
(doc_comment) @comment.block.documentation

; -- Attributes -----------------------------------------------------
; The brackets and the name color as one unit; an argument inside them
; is an ordinary expression and picks up its colors from the rules
; below.
(attribute ["#[" "]"] @attribute)
(attribute name: (identifier) @attribute)

; -- Keywords -------------------------------------------------------
[
  "select"
  "seq"
  "try"
  "with"
  "seqq"
  "until"
  "abort"
  "flush("
  "if"
  "catch"
] @keyword.control

[
  "let"
  "rec"
  "mut"
  "mod"
  "use"
  "type"
  "val"
  "fn"
  "cast"
  "never"
  "any"
  "with"
  "throws"
  "as"
  "trait"
  "impl"
  "for"
  "pub"
] @keyword

; -- Operators ------------------------------------------------------
["<-" "=>" "->"] @keyword.operator

[
  "=="
  "!="
  "<"
  ">"
  "<="
  ">="
  "&&"
  "||"
  "!"
  "+"
  "-"
  "*"
  "/"
  "%"
  "+?"
  "-?"
  "*?"
  "/?"
  "%?"
  "~"
  "~!"
  "?"
  "$"
  "&"
  "="
] @operator

; Pure punctuation
["::" ":" "." "," ";"] @punctuation.delimiter
["(" ")" "[" "]" "{" "}" "|"] @punctuation.bracket

; Sigils — these are part of larger constructs (labeled args, variants,
; variadic args, builtin refs) but we still tag them as operators so
; they have a color when not covered by a wrapping rule.
["#" "@" "`"] @operator

; -- Types ----------------------------------------------------------
; CR claude for claude: [bug/low] This capture never fires for a scalar primitive other
; than `null`. The grammar's ref_type/type_path accepts _binding_name, so `i64`,
; `string`, `f64` and the rest parse as (ref_type (type_path ...)) in every type
; position. In both tree-sitter and Helix, `let a: i64`, `|n: i64| -> string` and
; `Map<string, i64>` show the primitives uncoloured. The anonymous constructor tokens of
; array_type, map_type, error_type and abstract_body (`Array`, `Map`, `Error`,
; `Abstract`) have no capture either, so `let xs: Array<i64>` has no colour on its type
; at all. Fix the root in grammar.js: keep the _scalar_primitive words out of type_path
; (or give primitive_type precedence there) so they parse as primitive_type. Add
; ["Array" "Map" "Error" "Abstract"] @type.builtin here. Probe:
; design/review-2026-10-05/repro/ide-tooling.r2-04.py (rows Abstract<i64> and Array<i64>
; WRONG). (x-rerun-4-06)
(primitive_type) @type.builtin
(type_variable) @type.parameter
(type_identifier) @type

; -- Variants (constructors) ----------------------------------------
(variant       (type_identifier) @constructor)
(variant_pattern (type_identifier) @constructor)
(variant_type  (type_identifier) @constructor)

; Builtin references ('foo)
(builtin_ref) @function.builtin

; -- Parameters & labeled args --------------------------------------
; Tag the wrapper node so the leading sigil ('#'/'@') is also colored.
(labeled_param) @variable.parameter
(labeled_arg) @variable.parameter
(labeled_arg_shorthand) @variable.parameter
(variadic_param) @variable.parameter

; The path-root keywords: `self` (also group-self), `super`,
; `package`, and the glob marker.
(use_path "self" @keyword)
(use_path "super" @keyword)
(use_path "package" @keyword)
(use_path "as" @keyword)
(module_path "self" @keyword)
(module_path "super" @keyword)
(module_path "package" @keyword)
(use_glob) @operator

; -- Variables ------------------------------------------------------
(pattern_bind name: (identifier) @variable)
(catch_stmt binding: (identifier) @variable)
(reference (module_path (identifier) @variable))

; Module path: every identifier followed by `::` is a namespace; the
; last one stays the @variable above. use_path is the use-tree twin of
; module_path (its `::` chain is inlined so the parser never forks per
; segment).
(module_path (identifier) @namespace . "::")
(use_path (identifier) @namespace . "::")

; -- Functions ------------------------------------------------------
; A call: the last identifier of the callee path. It comes after the
; @variable and @namespace rules because later captures win. The
; `apply` node has no named field for its callee, so the reference is
; matched positionally, as its first child.
(apply
  (reference
    (module_path
      (identifier) @function .)))

; Module / type definitions
(module name: (identifier) @namespace)
(type_def name: (type_identifier) @type)

; Sig items (interface declarations in `.gxi` files; legal anywhere at
; the top level under the unified grammar). Captures mirror the impl-
; side rules so colors are consistent across file kinds.
(sig_bind name: (identifier) @variable)
(sig_type_def name: (type_identifier) @type)
(sig_module name: (identifier) @namespace)
(sig_use path: (use_path (identifier) @namespace . "::"))

; -- Struct fields --------------------------------------------------
(struct_field         name: (identifier) @variable.other.member)
(struct_pattern_field name: (identifier) @variable.other.member)
(struct_type_field    name: (identifier) @variable.other.member)
(struct_ref (identifier) @variable.other.member)

; -- Numbers --------------------------------------------------------
; The grammar produces a single `(number)` node covering int/float/hex/
; bin/oct/duration/scientific. Distinguish via #match? predicates so
; themes that style numeric subscopes can pick them up. Order: general
; scope first, specific scopes after — later wins.
(number) @constant.numeric

((number) @constant.numeric.integer
  (#match? @constant.numeric.integer "^[+-]?[0-9_]+$"))

((number) @constant.numeric.integer
  (#match? @constant.numeric.integer "^0[xXbBoO]"))

; CR claude for claude: [bug/low] Later captures win, so a hex literal containing e or E
; (`0xFE`) ends up @constant.numeric.float, and one ending in d (`0xd`) is taken by the
; duration rule below. Anchor the float rule on a decimal head (`^-?[0-9]+(\.|[eE])`),
; and keep radix literals out of the duration rule (`^0[xXbBoO]`). Several tokens also
; have no capture: `Array`, `Map`, `Error` and `Abstract` in types,
; self_type/self_param, the dynamic-module words (dynamic, sandbox, sig, source) and a
; duration's unit (the hidden `_unit_extension`, so `500.ms` colours only `500`). Add
; `["Array" "Map" "Error" "Abstract"] @type.builtin`, `[(self_type) (self_param)]
; @variable.builtin` and the module keywords, and expose the unit as a visible node to
; capture. (ide-tooling-06)
((number) @constant.numeric.float
  (#match? @constant.numeric.float "[.eE]"))

; Duration literals (1s, 500ms, 2h, etc.) keep the general @constant.numeric
((number) @constant.numeric
  (#match? @constant.numeric "[smhd]$"))

; -- Other constants ------------------------------------------------
(boolean) @constant.builtin.boolean
(null)    @constant.builtin

; -- Strings --------------------------------------------------------
; Tag the whole string node; inner rules (interpolation, escape) win
; for their sub-ranges via "later capture wins" semantics.
(string)              @string
(value_string)        @string
(raw_string)          @string
(triple_string)       @string
(string_content)      @string
(value_string_content) @string

(escape_sequence) @constant.character.escape
(template_escape) @constant.character.escape

; A normal string marks the interpolation; a triple-quoted template
; marks the splice instead, so both delimiters get the same color.
(interpolation
  "[" @punctuation.special
  "]" @punctuation.special)

(template_splice
  "\\[" @punctuation.special
  "]" @punctuation.special)
