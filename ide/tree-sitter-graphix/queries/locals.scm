; Scopes
(module) @local.scope
(lambda) @local.scope
(block) @local.scope
(select) @local.scope
(seq_block) @local.scope
(match_arm) @local.scope
(catch_stmt) @local.scope

; Definitions
(pattern_bind
  name: (identifier) @local.definition)

(catch_stmt
  binding: (identifier) @local.definition)

(type_def
  name: (type_identifier) @local.definition)

(module
  name: (identifier) @local.definition)

; References
; CR claude for claude: [bug/low] tree-sitter-highlight, which Helix ports, colors a
; resolved @local.reference with its definition's highlight, and a pattern_bind
; definition is @variable. So every call of a let-bound function (f(y) after let f = |x|
; ..) loses highlights.scm's @function call rule and shows as a variable. Because this
; captures every segment of a path, array::map also takes the color of a local `map`.
; Fix: anchor this capture to one-segment paths, (module_path . (identifier)
; @local.reference .). In highlights.scm, capture a let-bound lambda's name as
; @function: (let_binding pattern: (structure_pattern (pattern_bind name: (identifier)
; @function)) value: (lambda)). Both changes were checked in a scratch copy. probe:
; design/review-2026-10-19/repro/ide-tooling-locals-calls-as-variables.sh
; (ide-tooling-03)
(reference
  (module_path
    (identifier) @local.reference))
