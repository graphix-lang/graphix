#include "tree_sitter/parser.h"

enum TokenType {
  STRING_CONTENT,
  VALUE_EXTENSION,
  BARE_VALUE,
  RAW_STRING,
  TRIPLE_CONTENT,
  LIST_CLOSE,
  UNIT_EXTENSION,
  DOC_COMMENT,
  ERROR_SENTINEL,
};

static bool is_ident_char(int32_t c) {
  return (c >= 'A' && c <= 'Z') ||
         (c >= 'a' && c <= 'z') ||
         (c >= '0' && c <= '9') ||
         c == '_';
}

// A base64 character, as a `bytes:` value holds them.
static bool is_value_char(int32_t c) {
  return is_ident_char(c) || c == '=' || c == '+' || c == '/';
}

static void consume_value_chars(TSLexer *lexer) {
  while (is_value_char(lexer->lookahead)) {
    lexer->advance(lexer, false);
  }
}

// The VALUE_EXTENSION run: the rest of a `bytes:` value after its leading
// literal. Shared with the raw-string probe's fallback (a non-raw `r` is
// a valid extension prefix already consumed).
static void scan_value_extension_tail(TSLexer *lexer) {
  consume_value_chars(lexer);
  lexer->mark_end(lexer);
}

// BARE_VALUE pattern 3 with the first ident char already consumed:
// ident chars until a special char commits, anything else refuses.
static bool scan_bare_ident_tail(TSLexer *lexer) {
  while (true) {
    int32_t c = lexer->lookahead;
    if (c == '=' || c == '+' || c == '/') {
      lexer->advance(lexer, false);
      consume_value_chars(lexer);
      lexer->mark_end(lexer);
      lexer->result_symbol = BARE_VALUE;
      return true;
    }
    if (is_ident_char(c)) {
      lexer->advance(lexer, false);
    } else {
      return false;
    }
  }
}

// A literal's unit in a non-bytes ascription: letters (`ms`, `s`, `M`),
// optionally after one dot (`500.ms`), or a lone trailing dot (`0.`)
// that does not begin a `..` range.
static bool scan_unit_extension(TSLexer *lexer) {
  bool any = false;
  if (lexer->lookahead == '.') {
    lexer->advance(lexer, false);
    if (lexer->lookahead == '.') return false;
    any = true;
  }
  while ((lexer->lookahead >= 'a' && lexer->lookahead <= 'z') ||
         (lexer->lookahead >= 'A' && lexer->lookahead <= 'Z')) {
    lexer->advance(lexer, false);
    any = true;
  }
  if (!any) return false;
  lexer->mark_end(lexer);
  lexer->result_symbol = UNIT_EXTENSION;
  return true;
}

void *tree_sitter_graphix_external_scanner_create(void) {
  return NULL;
}

void tree_sitter_graphix_external_scanner_destroy(void *p) { (void)p; }

unsigned tree_sitter_graphix_external_scanner_serialize(void *p, char *b) {
  (void)p; (void)b;
  return 0;
}

void tree_sitter_graphix_external_scanner_deserialize(void *p, const char *b, unsigned n) {
  (void)p; (void)b; (void)n;
}

bool tree_sitter_graphix_external_scanner_scan(
    void *p,
    TSLexer *lexer,
    const bool *valid_symbols) {
  (void)p;

  // During error recovery all external tokens are valid.
  // The error sentinel is never used in the grammar, so if it's valid
  // we know we're in error recovery — bail out.
  if (valid_symbols[ERROR_SENTINEL]) return false;

  // The tokens that may follow whitespace skip it here: externals run
  // before the internal whitespace skip, which never re-runs this
  // scanner. String content holds whitespace, so none is skipped where
  // content is valid. A refusal restores the lexer, skipped whitespace
  // included.
  bool skipped_ws = false;
  bool line_start = false;
  uint32_t run = 0;
  if ((valid_symbols[LIST_CLOSE] || valid_symbols[DOC_COMMENT] ||
       valid_symbols[RAW_STRING]) &&
      !valid_symbols[STRING_CONTENT] && !valid_symbols[TRIPLE_CONTENT]) {
    while (lexer->lookahead == ' ' || lexer->lookahead == '\t' ||
           lexer->lookahead == '\n' || lexer->lookahead == '\r') {
      if (lexer->lookahead == '\n') {
        line_start = true;
        run = 0;
      } else {
        run++;
      }
      lexer->advance(lexer, true);
      skipped_ws = true;
    }
  }

  // The `>]` list closer is one terminal, distinct from the comparison
  // `>`, so a list's last element may end in any binary operator.
  // Adjacency is required only between `>` and `]`. On a `>` not
  // followed by `]` the internal lexer reads the comparison.
  if (valid_symbols[LIST_CLOSE] && lexer->lookahead == '>') {
    lexer->advance(lexer, false);
    if (lexer->lookahead == ']') {
      lexer->advance(lexer, false);
      lexer->mark_end(lexer);
      lexer->result_symbol = LIST_CLOSE;
      return true;
    }
    return false;
  }

  // `///` to the end of its line, only on a line of its own. A leading
  // byte-order mark, which the lexer skips, still counts as a column,
  // so one column more than the whitespace is a line start too.
  if (valid_symbols[DOC_COMMENT] && lexer->lookahead == '/') {
    if (!line_start) line_start = lexer->get_column(lexer) <= run + 1;
    for (int i = 0; i < 3; i++) {
      if (lexer->lookahead != '/') return false;
      lexer->advance(lexer, false);
    }
    if (!line_start) return false;
    while (!lexer->eof(lexer) && lexer->lookahead != '\n') {
      lexer->advance(lexer, false);
    }
    lexer->mark_end(lexer);
    lexer->result_symbol = DOC_COMMENT;
    return true;
  }

  // Where string content is valid, whitespace and a bare `r` are
  // content (ascribed and pattern strings never interpolate), so the
  // raw-string probe stays out of the content scan.
  if (valid_symbols[RAW_STRING] &&
      !valid_symbols[STRING_CONTENT] && !valid_symbols[TRIPLE_CONTENT]) {
    // r"..." with 0..N hashes: r#"..."#, r##"..."##, ... Content is
    // VERBATIM (no escapes); it ends at the FIRST '"' followed by the
    // opener's hash count. An identifier starting with 'r' falls
    // through to the internal lexer.
    if (lexer->lookahead == 'r') {
      lexer->advance(lexer, false);
      if (lexer->lookahead == '#' || lexer->lookahead == '"') {
        int hashes = 0;
        while (lexer->lookahead == '#') {
          lexer->advance(lexer, false);
          hashes++;
        }
        if (lexer->lookahead == '"') {
          lexer->advance(lexer, false);
          while (!lexer->eof(lexer)) {
            if (lexer->lookahead == '"') {
              lexer->advance(lexer, false);
              int seen = 0;
              while (seen < hashes && lexer->lookahead == '#') {
                lexer->advance(lexer, false);
                seen++;
              }
              if (seen == hashes) {
                lexer->mark_end(lexer);
                lexer->result_symbol = RAW_STRING;
                return true;
              }
              // Not the terminator — the quote and hashes were content.
              continue;
            }
            lexer->advance(lexer, false);
          }
        }
        return false;
      }
      // A non-raw `r` is a valid prefix of the value-extension and
      // bare-value tokens: continue as those where they are valid.
      // Adjacency tokens never follow skipped whitespace.
      if (!skipped_ws && valid_symbols[VALUE_EXTENSION]) {
        lexer->result_symbol = VALUE_EXTENSION;
        lexer->mark_end(lexer);
        scan_value_extension_tail(lexer);
        return true;
      }
      if (!skipped_ws && valid_symbols[BARE_VALUE]) {
        return scan_bare_ident_tail(lexer);
      }
      return false;
    }
  }

  // The tokens below are adjacent to what precedes them, so none
  // follows skipped whitespace.
  if (skipped_ws) return false;

  if (valid_symbols[UNIT_EXTENSION] && scan_unit_extension(lexer)) {
    return true;
  }

  if (valid_symbols[VALUE_EXTENSION]) {
    // Greedily consume adjacent base64 characters after a literal in a
    // `bytes:` value. This runs BEFORE whitespace is consumed, so if the
    // first character is not a value char we return false, letting the
    // parser skip the optional and continue with other branches (e.g.,
    // struct_field in GLR mode). Refusing rather than returning a
    // zero-length token keeps GLR branches that don't expect this token.
    if (!is_value_char(lexer->lookahead)) {
      return false;
    }

    lexer->result_symbol = VALUE_EXTENSION;
    lexer->mark_end(lexer);
    scan_value_extension_tail(lexer);
    return true;
  }

  if (valid_symbols[BARE_VALUE]) {
    // Bare values in type ascription context. External token so it's tried
    // BEFORE the internal lexer — this prevents `//` in values like
    // `bytes://8A==` from being matched as a line comment.
    //
    // Three patterns, mirroring the original internal bare_value rule:
    //   1. [A-Z][A-Za-z0-9_=+/]*       — starts with uppercase
    //   2. [=+/][A-Za-z0-9_=+/]*       — starts with = + /
    //   3. [A-Za-z0-9_]+[=+/][A-Za-z0-9_=+/]* — ident chars then special
    int32_t first = lexer->lookahead;

    // Pattern 1: starts with uppercase letter
    if (first >= 'A' && first <= 'Z') {
      lexer->advance(lexer, false);
      consume_value_chars(lexer);
      lexer->mark_end(lexer);
      lexer->result_symbol = BARE_VALUE;
      return true;
    }

    // Pattern 2: starts with = + /
    if (first == '=' || first == '+' || first == '/') {
      lexer->advance(lexer, false);
      consume_value_chars(lexer);
      lexer->mark_end(lexer);
      lexer->result_symbol = BARE_VALUE;
      return true;
    }

    // Pattern 3: starts with ident char, must contain = + / somewhere
    if (is_ident_char(first)) {
      lexer->advance(lexer, false);
      return scan_bare_ident_tail(lexer);
    }

    return false;
  }

  if (valid_symbols[TRIPLE_CONTENT]) {
    // Content of a triple-quoted TEMPLATE: brackets are ordinary
    // content (the splice is the marked thing, \[expr]); stop only at
    // backslash, and at a quote that begins the closing """.
    bool has = false;
    while (!lexer->eof(lexer)) {
      int32_t c = lexer->lookahead;
      if (c == '\\') break;
      if (c == '"') {
        // Peek: two more quotes end the string. Anything shorter is
        // content — mark_end only advances past CONFIRMED content, so
        // returning here leaves the terminator for the parser.
        lexer->mark_end(lexer);
        lexer->advance(lexer, false);
        if (lexer->lookahead != '"') {
          has = true;
          lexer->mark_end(lexer);
          continue;
        }
        lexer->advance(lexer, false);
        if (lexer->lookahead != '"') {
          has = true;
          lexer->mark_end(lexer);
          continue;
        }
        // A full """ — stop before it (mark_end already set).
        break;
      }
      has = true;
      lexer->advance(lexer, false);
      lexer->mark_end(lexer);
    }
    if (has) {
      lexer->result_symbol = TRIPLE_CONTENT;
      return true;
    }
    return false;
  }

  if (!valid_symbols[STRING_CONTENT]) return false;

  // Consume string content, stopping at characters that have special
  // meaning: '"' (close string), '\\' (escape), '[' (interpolation in
  // regular strings, or literal token in value_string).
  bool has_content = false;
  while (!lexer->eof(lexer)) {
    if (lexer->lookahead == '"' || lexer->lookahead == '\\' ||
        lexer->lookahead == '[' || lexer->lookahead == ']') {
      break;
    }
    has_content = true;
    lexer->advance(lexer, false);
  }

  if (has_content) {
    lexer->result_symbol = STRING_CONTENT;
    return true;
  }
  return false;
}
