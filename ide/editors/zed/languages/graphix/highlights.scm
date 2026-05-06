; -- Comments -------------------------------------------------------
(line_comment) @comment.line
(doc_comment) @comment.block.documentation

; -- Keywords -------------------------------------------------------
[
  "select"
  "if"
  "try"
  "catch"
] @keyword.control

[
  "let"
  "rec"
  "mod"
  "use"
  "type"
  "val"
  "fn"
  "cast"
  "any"
  "with"
  "where"
  "throws"
  "as"
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
  "~"
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
(primitive_type) @type.builtin
(type_variable) @type.parameter
(type_identifier) @type

; -- Variants (constructors) ----------------------------------------
(variant       (type_identifier) @constructor)
(variant_pattern (type_identifier) @constructor)
(variant_type  (type_identifier) @constructor)

; -- Functions ------------------------------------------------------
; A direct call: tag the leaf identifier of the callee path as @function.
; A namespaced call (mod::name(...)) — the @namespace rule below runs
; later and re-tags the leading module identifier, so we end up with
; namespace::function. Order in this file matters: later wins.
;
; Note: the `apply` node has no named field for its callee, so we match
; positionally — the reference is the first child by grammar definition.
(apply
  (reference
    (module_path
      (identifier) @function)))

; Builtin references ('foo)
(builtin_ref) @function.builtin

; -- Parameters & labeled args --------------------------------------
; Tag the wrapper node so the leading sigil ('#'/'@') is also colored.
(labeled_param) @variable.parameter
(labeled_arg) @variable.parameter
(labeled_arg_shorthand) @variable.parameter
(variadic_param) @variable.parameter

; -- Variables ------------------------------------------------------
(pattern_bind name: (identifier) @variable)
(reference (module_path (identifier) @variable))

; Module path: the FIRST identifier is a namespace; subsequent ones
; remain @variable / @function from the rules above.
(module_path . (identifier) @namespace)

; Module / type definitions
(module name: (identifier) @namespace)
(type_def name: (type_identifier) @type)

; Sig items (interface declarations in `.gxi` files; legal anywhere at
; the top level under the unified grammar). Captures mirror the impl-
; side rules so colors are consistent across file kinds.
(sig_bind name: (identifier) @variable)
(sig_type_def name: (type_identifier) @type)
(sig_module name: (identifier) @namespace)
(sig_use path: (module_path . (identifier) @namespace))

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
(string_content)      @string
(value_string_content) @string
(raw_string_content)  @string

(escape_sequence) @constant.character.escape

(interpolation
  "[" @punctuation.special
  "]" @punctuation.special)
