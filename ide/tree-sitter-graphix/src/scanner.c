#include "tree_sitter/parser.h"

enum TokenType {
  STRING_CONTENT,
  ERROR_SENTINEL,
};

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
