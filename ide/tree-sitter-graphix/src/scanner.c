#include "tree_sitter/parser.h"

enum TokenType {
  STRING_CONTENT,
  VALUE_EXTENSION,
  BARE_VALUE,
  ERROR_SENTINEL,
};

static bool is_value_char(int32_t c) {
  return (c >= 'A' && c <= 'Z') ||
         (c >= 'a' && c <= 'z') ||
         (c >= '0' && c <= '9') ||
         c == '_' || c == '=' || c == '+' || c == '/';
}

// Consume a run of value characters [A-Za-z0-9_=+/].
// Returns the number of characters consumed.
static int consume_value_chars(TSLexer *lexer) {
  int count = 0;
  while (is_value_char(lexer->lookahead)) {
    lexer->advance(lexer, false);
    count++;
  }
  return count;
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

  if (valid_symbols[VALUE_EXTENSION]) {
    // Greedily consume adjacent value characters after a literal in type
    // ascription context. This runs BEFORE whitespace is consumed, so if
    // the first character is not a valid value char we return false, letting
    // the parser skip the optional and continue with other branches (e.g.,
    // struct_field in GLR mode).
    //
    // We must check the first character before committing — returning true
    // with zero length would kill GLR branches that don't expect this token.
    int32_t first = lexer->lookahead;
    if (!is_value_char(first) && first != '.' && first != '-' && first != ':') {
      return false;
    }

    lexer->result_symbol = VALUE_EXTENSION;
    lexer->mark_end(lexer);

    while (true) {
      int32_t c = lexer->lookahead;
      if (is_value_char(c)) {
        lexer->advance(lexer, false);
        lexer->mark_end(lexer);
      } else if (c == '.') {
        // Consume '.' only if not followed by another '.' (preserve '..' range)
        lexer->advance(lexer, false);
        if (lexer->lookahead == '.') {
          break;
        }
        lexer->mark_end(lexer);
      } else if (c == '-') {
        // Consume '-' only if followed by a digit (datetime like 2024-01-01)
        lexer->advance(lexer, false);
        if (lexer->lookahead >= '0' && lexer->lookahead <= '9') {
          lexer->mark_end(lexer);
        } else {
          break;
        }
      } else if (c == ':') {
        // Consume ':' only if followed by a digit (datetime like 00:00:00)
        lexer->advance(lexer, false);
        if (lexer->lookahead >= '0' && lexer->lookahead <= '9') {
          lexer->mark_end(lexer);
        } else {
          break;
        }
      } else {
        break;
      }
    }

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
    if ((first >= 'a' && first <= 'z') ||
        (first >= '0' && first <= '9') ||
        first == '_') {
      // Advance through leading ident chars looking for a special char.
      // If the scanner returns false, tree-sitter restores the lexer state.
      lexer->advance(lexer, false);
      while (true) {
        int32_t c = lexer->lookahead;
        if (c == '=' || c == '+' || c == '/') {
          // Found special char — commit everything so far and continue
          lexer->advance(lexer, false);
          consume_value_chars(lexer);
          lexer->mark_end(lexer);
          lexer->result_symbol = BARE_VALUE;
          return true;
        }
        if ((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') ||
            (c >= '0' && c <= '9') || c == '_') {
          lexer->advance(lexer, false);
        } else {
          // Reached non-value char without finding = + /
          // Not a bare_value — return false (lexer restores state)
          return false;
        }
      }
    }

    return false;
  }

  if (!valid_symbols[STRING_CONTENT]) return false;

  // Consume string content, stopping at characters that have special
  // meaning: '"' (close string), '\\' (escape), '[' (interpolation in
  // regular strings, or literal token in value_string).
  bool has_content = false;
  while (lexer->lookahead != 0) {
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
