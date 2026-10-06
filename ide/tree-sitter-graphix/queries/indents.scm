; Indent on opening brackets
[
  (sig_block)
  (block)
  (select)
  (seq_block)
  (struct)
  (map)
  (array)
  (tuple)
  (struct_type)
  (tuple_type)
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
