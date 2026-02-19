/**
 * @file Tree-sitter grammar for Graphix programming language
 * @author Graphix Authors
 * @license MIT
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

module.exports = grammar({
  name: 'graphix',

  extras: $ => [
    /\s/,
    $.line_comment,
  ],

  externals: $ => [
    $._string_content,
    $._value_extension,  // greedy value chars after a literal in type ascription
    $._bare_value,       // bare values like base64 (external to beat line_comment on //)
    $._error_sentinel,   // never used in grammar; valid only during error recovery
  ],

  inline: $ => [
    $._expression_inner,
  ],

  precedences: $ => [
    [
      'unary',
      'multiplicative',
      'additive',
      'comparison',
      'equality',
      'and',
      'or',
      'sample',
    ],
  ],

  conflicts: $ => [
    [$._expression, $.map_ref],
    [$._expression, $.lambda],
    [$._expression, $.apply],
    [$._expression, $.array_ref, $.array_slice],
    [$._type, $.type_path],
    [$.union_type, $.array_pattern],
    [$.array_pattern, $.slice_prefix_pattern],
    [$.primitive_type, $.null],
    [$.connect],
    [$.lambda, $.map_ref],
    [$.lambda, $.apply],
    [$.lambda, $.array_ref, $.array_slice],
    [$.variant_type],
    [$.wildcard_type, $.structure_pattern],

    [$.struct_type, $.struct_pattern],
    [$.variant_type, $.variant_pattern],
    [$.variant_type_args, $.variant_pattern],
    [$.lambda, $._primary_expression],
    [$._expression, $.or_never],
    [$.lambda, $.or_never],
    [$._expression, $.qop],
    [$.lambda, $.qop],
    [$.type_path, $.pattern_bind],
    [$.pattern_bind, $.type_ascription],
    [$._typed_value, $.module_path],
    [$._typed_value, $._primary_expression],
    [$._typed_value, $.type_ascription],
    [$.type_ascription, $._primary_expression],
    [$.string, $.value_string],
    [$.value_string, $.interpolation],
    [$.module],
  ],

  word: $ => $.identifier,

  rules: {
    source_file: $ => seq(
      optional($._expression),
      repeat(seq(';', optional($._expression))),
    ),

    _expression: $ => choice(
      $.module,
      $.use,
      $.type_def,
      $.let_binding,
      $.try_catch,
      $.lambda,
      $.connect,
      $._arithmetic_expression,
      $.by_ref,
      $.deref,
      $._primary_expression,
    ),

    _expression_inner: $ => choice(
      $.module,
      $.use,
      $.type_def,
      $.let_binding,
      $.connect,
      $.lambda,
      $.try_catch,
      $._arithmetic_expression,
      $.by_ref,
      $.deref,
      $.parenthesized_expression,
      $._primary_expression,
    ),

    // Comments
    line_comment: $ => token(seq('//', /[^\/\n]/, /.*/)),

    doc_comment: $ => token(seq('///', /.*/)),

    // Module and use
    module: $ => choice(
      // Bodyless module declaration: mod name
      seq('mod', field('name', $.identifier)),
      // Static module: mod name { ... }
      seq(
        'mod',
        field('name', $.identifier),
        optional($.signature),
        field('body', $.module_body),
      ),
      // Dynamic module: mod name dynamic { sandbox ...; sig { ... }; source expr }
      seq(
        'mod',
        field('name', $.identifier),
        'dynamic',
        '{',
        field('sandbox', $.sandbox),
        ';',
        field('sig', $.sig_block),
        ';',
        'source',
        field('source', $._expression),
        optional(';'),
        '}',
      ),
    ),

    module_body: $ => seq(
      '{',
      optional(seq(
        $._expression,
        repeat(seq(';', optional($._expression))),
      )),
      '}',
    ),

    sandbox: $ => choice(
      seq('sandbox', 'unrestricted'),
      seq('sandbox', 'blacklist', '[', commaSep1($.module_path), ']'),
      seq('sandbox', 'whitelist', '[', commaSep1($.module_path), ']'),
    ),

    sig_block: $ => seq(
      'sig',
      '{',
      optional(seq(
        $.sig_item,
        repeat(seq(';', $.sig_item)),
        optional(';'),
      )),
      '}',
    ),

    signature: $ => seq(
      ':',
      '{',
      repeat($.sig_item),
      '}',
    ),

    sig_item: $ => choice(
      $.sig_type_def,
      $.sig_bind,
      $.sig_module,
      $.sig_use,
    ),

    sig_type_def: $ => seq(
      optional($.doc_comment),
      'type',
      field('name', $.type_identifier),
      optional($.type_params),
      '=',
      field('type', $._type),
    ),

    sig_bind: $ => seq(
      optional($.doc_comment),
      'val',
      field('name', $.identifier),
      ':',
      field('type', $._type),
    ),

    sig_module: $ => seq(
      optional($.doc_comment),
      'mod',
      field('name', $.identifier),
    ),

    sig_use: $ => seq(
      optional($.doc_comment),
      'use',
      field('path', $.module_path),
    ),

    use: $ => seq(
      'use',
      field('path', $.module_path),
    ),

    // Type definitions
    type_def: $ => seq(
      'type',
      field('name', $.type_identifier),
      optional($.type_params),
      '=',
      field('type', $._type),
    ),

    type_params: $ => seq(
      '<',
      commaSep1($.type_param),
      '>',
    ),

    type_param: $ => seq(
      $.type_variable,
      optional(seq(':', $._type)),
    ),

    // Types
    _type: $ => choice(
      $.primitive_type,
      $.type_identifier,
      $.type_variable,
      $.wildcard_type,
      $.array_type,
      $.map_type,
      $.tuple_type,
      $.struct_type,
      $.variant_type,
      $.union_type,
      $.function_type,
      $.ref_type,
      $.by_ref_type,
      $.abstract_type,
      $.error_type,
      $.parenthesized_type,
    ),

    wildcard_type: $ => '_',

    parenthesized_type: $ => seq('(', $._type, ')'),

    primitive_type: $ => choice(
      'bool', 'string', 'bytes', 'null',
      'i8', 'u8', 'i16', 'u16',
      'i32', 'u32', 'v32', 'z32',
      'i64', 'u64', 'v64', 'z64',
      'f32', 'f64',
      'decimal', 'datetime', 'duration',
      'Any',
    ),

    type_variable: $ => seq("'", $.identifier),

    array_type: $ => seq('Array', '<', $._type, '>'),

    map_type: $ => seq('Map', '<', $._type, ',', $._type, '>'),

    tuple_type: $ => seq('(', commaSep2($._type), ')'),

    struct_type: $ => seq('{', commaSep($.struct_type_field), '}'),

    struct_type_field: $ => seq(
      field('name', $.identifier),
      ':',
      field('type', $._type),
    ),

    variant_type: $ => seq('`', $.type_identifier, optional($.variant_type_args)),

    variant_type_args: $ => seq('(', commaSep($._type), ')'),

    // Union type: [Type1, Type2, ...] or [`A, `B, `C]
    union_type: $ => seq('[', commaSep($._type), ']'),

    function_type: $ => prec.left(seq(
      'fn',
      optional(seq('<', commaSep1($.constraint), '>')),
      '(',
      optional($.fn_type_args),
      ')',
      optional(seq('->', $._type)),
      optional($.throws_clause),
      optional($.constraints_clause),
    )),

    fn_type_args: $ => seq(
      commaSep1($.fn_type_arg),
      optional(seq(',', $.fn_type_varg)),
    ),

    fn_type_arg: $ => seq(
      optional($.fn_type_label),
      $._type,
    ),

    fn_type_label: $ => seq(optional('?'), '#', $.identifier, ':'),

    fn_type_varg: $ => seq('@', $.identifier, ':', $._type),

    labeled_param: $ => seq('#', $.identifier),

    throws_clause: $ => seq('throws', $._type),

    constraints_clause: $ => prec.left(seq(
      'where',
      commaSep1($.constraint),
    )),

    constraint: $ => seq($.type_variable, ':', $._type),

    ref_type: $ => prec.right(seq(
      $.type_path,
      optional($.type_arguments),
    )),

    type_path: $ => seq(
      choice($.type_identifier, $.identifier),
      repeat(seq('::', choice($.type_identifier, $.identifier))),
    ),

    type_arguments: $ => seq('<', commaSep1($._type), '>'),

    by_ref_type: $ => seq('&', $._type),

    abstract_type: $ => seq('?', $.type_identifier),

    error_type: $ => seq('Error', '<', $.string, '>'),

    // Let binding - low precedence so RHS captures full expression
    let_binding: $ => prec.right(-1, seq(
      'let',
      optional('rec'),
      field('pattern', $.structure_pattern),
      optional(seq(':', field('type', $._type))),
      '=',
      field('value', $._expression),
    )),

    // Patterns
    structure_pattern: $ => choice(
      '_',
      $.type_ascription,
      $.literal,
      $.value_string,
      $.pattern_bind,
      $.tuple_pattern,
      $.array_pattern,
      $.slice_prefix_pattern,
      $.slice_suffix_pattern,
      $.variant_pattern,
      $.struct_pattern,
    ),

    pattern_bind: $ => seq(
      optional(seq(field('all', $.identifier), '@')),
      field('name', $.identifier),
    ),

    tuple_pattern: $ => seq(
      optional(seq(field('all', $.identifier), '@')),
      '(',
      commaSep1($.structure_pattern),
      ')',
    ),

    array_pattern: $ => seq(
      optional(seq(field('all', $.identifier), '@')),
      '[',
      commaSep($.structure_pattern),
      ']',
    ),

    slice_prefix_pattern: $ => seq(
      optional(seq(field('all', $.identifier), '@')),
      '[',
      repeat(seq($.structure_pattern, ',')),
      optional($.identifier),
      '..',
      ']',
    ),

    slice_suffix_pattern: $ => seq(
      optional(seq(field('all', $.identifier), '@')),
      '[',
      optional($.identifier),
      '..',
      ',',
      commaSep1($.structure_pattern),
      ']',
    ),

    variant_pattern: $ => seq(
      optional(seq(field('all', $.identifier), '@')),
      '`',
      $.type_identifier,
      optional(seq('(', commaSep($.structure_pattern), ')')),
    ),

    struct_pattern: $ => seq(
      optional(seq(field('all', $.identifier), '@')),
      '{',
      commaSep($.struct_pattern_field),
      optional('..'),
      '}',
    ),

    struct_pattern_field: $ => seq(
      field('name', $.identifier),
      optional(seq(':', field('pattern', $.structure_pattern))),
    ),

    // Match pattern (for select)
    // Format: [Type as] structure_pattern [if guard]
    pattern: $ => seq(
      optional(seq($._type, 'as')),
      $.structure_pattern,
      optional(seq('if', $._expression)),
    ),

    // Lambda: constraints|params| -> rtype throws body
    lambda: $ => seq(
      optional($.lambda_constraints),
      '|',
      optional($.lambda_params),
      '|',
      optional(seq('->', $._type)),
      optional($.throws_clause),
      $._expression_inner,
    ),

    lambda_constraints: $ => commaSep1($.constraint),

    lambda_params: $ => commaSep1($.lambda_param),

    lambda_param: $ => choice(
      // Variadic param: @args
      $.variadic_param,
      // Labeled param with optional pattern: #foo or #foo x or #foo: Type or #foo: Type = default
      seq(
        $.labeled_param_with_default,
        optional($.structure_pattern),
      ),
      // Simple pattern: x or x: Type
      seq(
        $.structure_pattern,
        optional(seq(':', $._type)),
      ),
    ),

    variadic_param: $ => seq('@', $.identifier, optional(seq(':', $._type))),

    labeled_param_with_default: $ => prec.left(seq(
      optional('?'),
      '#',
      $.identifier,
      optional(seq(':', $._type)),
      optional(seq('=', $._expression)),
    )),

    // Try-catch - low precedence so handler captures full expression
    try_catch: $ => prec.right(-1, seq(
      'try',
      $._expression,
      repeat(seq(';', $._expression)),
      'catch',
      '(',
      field('binding', $.identifier),
      optional(seq(':', $._type)),
      ')',
      '=>',
      field('handler', $._expression),
    )),

    // Connect (assignment to variable) - low precedence so RHS captures full expression
    connect: $ => prec.right(-1, seq(
      optional('*'),
      field('target', $.module_path),
      '<-',
      field('value', $._expression),
    )),

    // Arithmetic/binary expressions
    _arithmetic_expression: $ => choice(
      $.binary_expression,
      $.unary_expression,
      $.apply,
      $.select,
      $.any,
      $.cast,
      $.do_block,
    ),

    // Type ascription: Type:literal (e.g., i8:0, duration:0.5s, bytes:AQID==)
    // Note: type comes FIRST, then colon, then value.
    // Allows both identifier (for non-keyword types like 'error') and
    // primitive_type (for keyword types like 'i8', 'f32', 'datetime').
    type_ascription: $ => seq(
      choice($.identifier, $.primitive_type),
      ':',
      choice($._typed_value, $.string, $.value_string, $.raw_string),
    ),

    // Non-string values that can appear after the colon in type ascription.
    // Strings are handled separately in type_ascription to share nodes with
    // expression contexts, enabling GLR to resolve struct/map ambiguity.
    _typed_value: $ => choice(
      // Nested type ascription (e.g., error:i8:0)
      $.type_ascription,
      // Literal followed by optional external scanner that greedily consumes
      // adjacent value characters (letters, digits, +, /, =, ., -). The scanner
      // returns false when there's nothing to consume, which lets the parser
      // skip the optional. Because external tokens are tried BEFORE internal
      // tokens, the scanner effectively resolves the shift-reduce conflict
      // at runtime: match -> shift, no match -> reduce.
      seq($.literal, optional($._value_extension)),
      // Bare values like base64 bytes (e.g. AQID==, //8A==)
      // External token so it's tried before line_comment can match //
      $._bare_value,
      // Fallback for values that look like identifiers
      $.identifier,
    ),


    binary_expression: $ => choice(
      prec.left('or', seq($._expression, '||', $._expression)),
      prec.left('and', seq($._expression, '&&', $._expression)),
      prec.left('equality', seq($._expression, '==', $._expression)),
      prec.left('equality', seq($._expression, '!=', $._expression)),
      prec.left('comparison', seq($._expression, '<', $._expression)),
      prec.left('comparison', seq($._expression, '>', $._expression)),
      prec.left('comparison', seq($._expression, '<=', $._expression)),
      prec.left('comparison', seq($._expression, '>=', $._expression)),
      prec.left('additive', seq($._expression, '+', $._expression)),
      prec.left('additive', seq($._expression, '-', $._expression)),
      prec.left('multiplicative', seq($._expression, '*', $._expression)),
      prec.left('multiplicative', seq($._expression, '/', $._expression)),
      prec.left('multiplicative', seq($._expression, '%', $._expression)),
      prec.left('sample', seq($._expression, '~', $._expression)),
    ),

    unary_expression: $ => prec('unary', choice(
      seq('!', $._expression),
    )),

    // Apply (function call)
    apply: $ => prec.left(seq(
      $._primary_expression,
      $.apply_args,
    )),

    apply_args: $ => seq(
      '(',
      commaSep($.apply_arg),
      ')',
    ),

    apply_arg: $ => choice(
      // Labeled arg shorthand: #name (passes variable with same name)
      $.labeled_arg_shorthand,
      // Labeled arg with value: #name: expr
      seq($.labeled_arg, $._expression),
      // Positional argument
      $._expression,
    ),

    labeled_arg: $ => seq('#', $.identifier, ':'),

    labeled_arg_shorthand: $ => seq('#', $.identifier),

    // Select (pattern matching)
    select: $ => seq(
      'select',
      field('value', $._expression),
      '{',
      commaSep1($.match_arm),
      '}',
    ),

    match_arm: $ => seq(
      field('pattern', $.pattern),
      '=>',
      field('body', $._expression),
    ),

    // Any expression
    any: $ => seq(
      'any',
      '(',
      commaSep($._expression),
      ')',
    ),

    // Cast
    cast: $ => seq(
      'cast',
      '<',
      $._type,
      '>',
      '(',
      $._expression,
      ')',
    ),

    // Do block
    do_block: $ => seq(
      '{',
      repeat1(seq($._expression, ';')),
      $._expression,
      optional(';'),
      '}',
    ),

    // By-reference and dereference
    by_ref: $ => prec('unary', seq('&', $._expression)),

    deref: $ => prec('unary', seq('*', $._expression)),

    // Parenthesized expression
    parenthesized_expression: $ => seq('(', $._expression, ')'),

    // Primary expressions
    _primary_expression: $ => choice(
      $.literal,
      // Trailing-dot float (e.g., 50.) — token.immediate ensures the dot must be
      // adjacent, and longest-match ensures '..' (2 chars) beats '.' (1 char),
      // so this never consumes a dot from the '..' range operator.
      seq($.literal, token.immediate('.')),
      $.reference,
      $.builtin_ref,
      $.string,
      $.raw_string,
      $.type_ascription,
      $.array,
      $.tuple,
      $.struct,
      $.map,
      $.struct_with,
      $.variant,
      $.struct_ref,
      $.tuple_ref,
      $.array_ref,
      $.array_slice,
      $.map_ref,
      $.qop,
      $.or_never,
      $.parenthesized_expression,
    ),

    // Literals
    literal: $ => choice(
      $.number,
      $.boolean,
      $.null,
    ),

    number: $ => token(prec(1, choice(
      // Duration literals (must come before floats/integers to match correctly)
      /\d+(\.\d*)?[smhd]/,
      // Scientific notation
      /[+-]?\d+(\.\d+)?[eE][+-]?\d+/,
      // Floats (require at least one digit after decimal to avoid consuming '..' operator)
      /[+-]?\d+\.\d+/,
      // Hexadecimal
      /0x[0-9a-fA-F]+/,
      // Binary
      /0b[01]+/,
      // Octal
      /0o[0-7]+/,
      // Integers (last to avoid matching prefixes of above)
      /[+-]?\d+/,
    ))),

    boolean: $ => choice('true', 'false'),

    null: $ => choice('null', 'ok'),

    // Strings (including interpolated strings)
    // Interpolating string: [expr] is interpolation.
    // The external scanner produces string_content tokens that stop at '[', '"', '\'.
    string: $ => seq(
      '"',
      repeat(choice(
        $.escape_sequence,
        alias($._string_content, $.string_content),
        $.interpolation,
      )),
      '"',
    ),

    // Non-interpolating string for value contexts (e.g., error:"[", bytes:"...").
    // Uses the SAME external content token as string (so the lexer produces one
    // token that both GLR branches can accept). The difference: value_string
    // also accepts literal '[' as a parser token, while string treats '[' as
    // interpolation start. This lets the parser handle the distinction.
    value_string: $ => prec.dynamic(-1, seq(
      '"',
      repeat(choice(
        $.escape_sequence,
        alias($._string_content, $.value_string_content),
        '[',
        ']',
      )),
      '"',
    )),

    escape_sequence: $ => token.immediate(seq(
      '\\',
      choice(
        /[\\\"nrt0\[\]]/,
        /x[0-9a-fA-F]{2}/,
        /u\{[0-9a-fA-F]+\}/,
      ),
    )),

    interpolation: $ => seq(
      '[',
      $._expression,
      ']',
    ),

    raw_string: $ => seq(
      "r'",
      optional($.raw_string_content),
      "'",
    ),

    raw_string_content: $ => token.immediate(/[^']*/),

    // Reference
    reference: $ => $.module_path,

    // Builtin reference (e.g., 'array_map)
    builtin_ref: $ => seq("'", $.identifier),

    module_path: $ => seq(
      optional('/'),
      $.identifier,
      repeat(seq('::', $.identifier)),
    ),

    // Array
    array: $ => seq(
      '[',
      commaSep($._expression),
      ']',
    ),

    // Tuple
    tuple: $ => seq(
      '(',
      commaSep2($._expression),
      ')',
    ),

    // Struct
    struct: $ => seq(
      '{',
      commaSep1($.struct_field),
      '}',
    ),

    // prec.dynamic(1) ensures GLR prefers struct_field over type_ascription
    // when both could match (e.g., {a: "hello"} is a struct, not map with type_ascription)
    struct_field: $ => prec.dynamic(10, seq(
      field('name', $.identifier),
      optional(seq(':', field('value', $._expression))),
    )),

    // Map
    map: $ => seq(
      '{',
      commaSep($.map_entry),
      '}',
    ),

    map_entry: $ => seq(
      field('key', $._expression),
      '=>',
      field('value', $._expression),
    ),

    // Struct with (update)
    struct_with: $ => seq(
      '{',
      $._expression,
      'with',
      commaSep1($.struct_field),
      '}',
    ),

    // Variant
    variant: $ => prec.left(seq(
      '`',
      $.type_identifier,
      optional(seq('(', commaSep($._expression), ')')),
    )),

    // Accessors
    struct_ref: $ => seq(
      $._primary_expression,
      '.',
      $.identifier,
    ),

    tuple_ref: $ => seq(
      $._primary_expression,
      '.',
      $.number,
    ),

    array_ref: $ => seq(
      $._primary_expression,
      '[',
      $._expression,
      ']',
    ),

    // Note: '...' (three dots) is accepted as a range operator because the
    // Graphix printer emits trailing-dot floats like f32:0. which, when followed
    // by the .. range operator, produces three consecutive dots (f32:0...).
    // Tree-sitter's context-free tokenizer would greedily match '..' first,
    // leaving the trailing dot as an error. Accepting '...' avoids this.
    array_slice: $ => seq(
      $._primary_expression,
      '[',
      optional($._expression),
      choice('..', '...'),
      optional($._expression),
      ']',
    ),

    map_ref: $ => seq(
      $._primary_expression,
      '{',
      $._expression,
      '}',
    ),

    // Qop and or_never - postfix operators
    qop: $ => prec.left(seq(
      choice($._primary_expression, $._arithmetic_expression),
      '?',
    )),

    or_never: $ => prec.left(seq(
      choice($._primary_expression, $._arithmetic_expression),
      '$',
    )),

    // Identifiers
    identifier: $ => /[a-z_][a-zA-Z0-9_]*/,

    type_identifier: $ => /[A-Z][a-zA-Z0-9_]*/,
  },
});

/**
 * Creates a rule to match comma-separated occurrences of the rule.
 * @param {RuleOrLiteral} rule
 * @returns {SeqRule}
 */
function commaSep(rule) {
  return optional(commaSep1(rule));
}

/**
 * Creates a rule to match one or more comma-separated occurrences of the rule.
 * @param {RuleOrLiteral} rule
 * @returns {SeqRule}
 */
function commaSep1(rule) {
  return seq(rule, repeat(seq(',', rule)), optional(','));
}

/**
 * Creates a rule to match two or more comma-separated occurrences of the rule.
 * @param {RuleOrLiteral} rule
 * @returns {SeqRule}
 */
function commaSep2(rule) {
  return seq(rule, ',', rule, repeat(seq(',', rule)), optional(','));
}
