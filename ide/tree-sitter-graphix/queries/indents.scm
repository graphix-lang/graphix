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
  (try_with)
  (list)
  (trait_def)
  (impl_def)
  (use_group)
  (union_type)
  (struct_pattern)
  (variant)
  (variant_pattern)
  (variant_type)
] @indent

; Outdent on closing brackets
[
  "}"
  "]"
  ")"
] @outdent

; Extend (continue current indent)
(match_arm) @extend
