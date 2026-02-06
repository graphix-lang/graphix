; Comments
(line_comment) @comment
(doc_comment) @comment.documentation

; Keywords
[
  "mod"
  "use"
  "let"
  "rec"
  "type"
  "fn"
  "select"
  "if"
  "try"
  "catch"
  "cast"
  "any"
  "with"
  "where"
  "throws"
  "as"
] @keyword

; Operators
[
  "+"
  "-"
  "*"
  "/"
  "%"
  "=="
  "!="
  "<"
  ">"
  "<="
  ">="
  "&&"
  "||"
  "!"
  "~"
  "?"
  "$"
  "&"
  "<-"
  "=>"
  "="
  "->"
  ":"
  "::"
  "."
  ","
  ";"
  "@"
  "`"
  "#"
] @operator

; Punctuation
[
  "("
  ")"
  "["
  "]"
  "{"
  "}"
  "|"
] @punctuation.bracket

; Types
(type_identifier) @type
(primitive_type) @type.builtin
(type_variable) @type.parameter

; Functions
(apply
  function: (reference
    (module_path
      (identifier) @function)))

(lambda) @function

; Parameters
(labeled_param
  (identifier) @parameter)
(labeled_param_with_default
  (identifier) @parameter)
(labeled_arg
  (identifier) @parameter)
(labeled_arg_shorthand
  (identifier) @parameter)
(variadic_param
  (identifier) @parameter)

; Variables
(let_binding
  pattern: (structure_pattern
    (pattern_bind
      name: (identifier) @variable.definition)))

(pattern_bind
  name: (identifier) @variable)

(reference
  (module_path
    (identifier) @variable))

; Module paths - first component is the module
(module_path
  . (identifier) @module)

; Struct fields
(struct_field
  name: (identifier) @property)
(struct_ref
  (identifier) @property)
(struct_pattern_field
  name: (identifier) @property)
(struct_type_field
  name: (identifier) @property)

; Variants (constructors)
(variant
  (type_identifier) @constructor)
(variant_pattern
  (type_identifier) @constructor)
(variant_type
  (type_identifier) @constructor)

; Literals
(number) @number
(boolean) @constant.builtin
(null) @constant.builtin

; Strings
(string) @string
(raw_string) @string
(string_content) @string
(raw_string_content) @string
(escape_sequence) @string.escape
(interpolation
  "[" @punctuation.special
  "]" @punctuation.special)

; Builtin references
(builtin_ref
  (identifier) @function.builtin)

; Module definition
(module
  name: (identifier) @module.definition)

; Type definition
(type_def
  name: (type_identifier) @type.definition)
