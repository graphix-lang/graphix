; Indent on opening brackets
[
  (module_body)
  (do_block)
  (select)
  (struct)
  (map)
  (array)
  (tuple)
  (struct_type)
  (tuple_type)
  (signature)
  (apply_args)
  (lambda)
] @indent

; Outdent on closing brackets
[
  "}"
  "]"
  ")"
] @outdent

; Extend (continue current indent)
(match_arm) @extend
