; Scopes
(module) @local.scope
(lambda) @local.scope
(do_block) @local.scope
(select) @local.scope
(seq_block) @local.scope
(match_arm) @local.scope
(catch_stmt) @local.scope

; Definitions
(let_binding
  pattern: (structure_pattern
    (pattern_bind
      name: (identifier) @local.definition)))

(pattern_bind
  name: (identifier) @local.definition)

(lambda_param
  (structure_pattern
    (pattern_bind
      name: (identifier) @local.definition)))

(catch_stmt
  binding: (identifier) @local.definition)

(type_def
  name: (type_identifier) @local.definition)

(module
  name: (identifier) @local.definition)

; References
(reference
  (module_path
    (identifier) @local.reference))
