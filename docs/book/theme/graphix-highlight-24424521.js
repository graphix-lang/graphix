/**
 * Syntax highlighting definition for the Graphix programming language
 * For use with highlight.js in mdbook
 */

(function () {
  "use strict";

  function graphix(hljs) {
    // The parser's reserved and construct words
    // (graphix-types/src/expr/parser/mod.rs KEYWORDS) plus the contextual
    // ones of module, seq, trait and signature syntax.
    const KEYWORDS =
      "mod let select type fn cast never if use rec catch try pub trait impl " +
      "seq seqq until mut val sig with as throws for dynamic sandbox whitelist " +
      "blacklist unrestricted source abort flush any self super package";

    // Compiler-known type names, the primitives, core's aliases and the
    // type-variable bounds
    const TYPES =
      "Array Map List Error Any Abstract " +
      "i8 u8 i16 u16 i32 u32 v32 z32 i64 u64 v64 z64 f32 f64 " +
      "decimal datetime duration bool string bytes " +
      "Result Option Number Int Sint Uint Float Real Primitive Ordering " +
      "Concrete Function Singleton OneNumber Discernible Ordered";

    // Type parameter pattern: 'a, 'b, 'r, 'e, etc.
    const TYPE_PARAM = {
      className: "type",
      begin: "'[a-z][a-z0-9_]*\\b",
    };

    // Variant pattern: `Foo, `Bar, `MoreArg, etc.
    const VARIANT = {
      className: "symbol",
      begin: "`[A-Z][a-zA-Z0-9_]*",
    };

    // Labeled argument pattern: #label:
    const LABEL = {
      className: "attr",
      begin: "#[a-z_][a-zA-Z0-9_]*:",
      relevance: 0,
    };

    // Attributes: #[native], #[parallel(g)]
    const ATTRIBUTE = {
      className: "meta",
      begin: "#\\[",
      end: "\\]",
    };

    // Reference operator: &
    const REFERENCE = {
      className: "operator",
      begin: "&(mut\\b)?",
      relevance: 0,
    };

    // Operators
    const OPERATORS = {
      className: "operator",
      begin:
        "(<-|->|=>|~!|~|\\?|\\$|\\.\\.|::|@|\\*(?=[a-z_])|\\||\\+\\??|\\-\\??|/\\??|%\\??|==|!=|<=|>=|<|>|&&|\\|\\|)",
    };

    // Numbers: decimal, float, exponent, hex, binary, octal
    const NUMBER = {
      className: "number",
      variants: [
        { begin: "\\b0x[0-9a-fA-F]+\\b" },
        { begin: "\\b0b[01]+\\b" },
        { begin: "\\b0o[0-7]+\\b" },
        { begin: "\\b\\d+\\.\\d+([eE][+-]?\\d+)?" },
        { begin: "\\b\\d+[eE][+-]?\\d+\\b" },
        { begin: "\\b\\d+\\b" },
      ],
      relevance: 0,
    };

    // Duration literals: duration:1.5s, duration:500.ms
    const DURATION = {
      className: "number",
      begin: "\\b\\d+(\\.\\d*)?(ns|us|ms|s|m|h|d|M|y)\\b",
    };

    const ESCAPE = {
      className: "char.escape",
      begin: "\\\\.",
      relevance: 0,
    };

    // Strings: interpolating "..[x]..", template """..\\[x]..""" (brackets
    // are text) and raw r"..", r#".."#
    const STRING = {
      className: "string",
      variants: [
        {
          begin: '"""',
          end: '"""',
          contains: [
            {
              className: "subst",
              begin: "\\\\\\[",
              end: "\\]",
              contains: ["self"],
            },
            ESCAPE,
          ],
        },
        { begin: 'r"', end: '"' },
        { begin: 'r#"', end: '"#' },
        { begin: 'r##"', end: '"##' },
        {
          begin: '"',
          end: '"',
          contains: [
            ESCAPE,
            {
              className: "subst",
              begin: "\\[",
              end: "\\]",
              contains: ["self"],
            },
          ],
        },
      ],
    };

    // Module path pattern: array::map, net::subscribe, etc.
    const MODULE_PATH = {
      className: "title.function",
      begin: "\\b[a-z_][a-z0-9_]*::[a-z_][a-z0-9_]*\\b",
    };

    // Function call pattern
    const FUNCTION_CALL = {
      className: "title.function",
      begin: "\\b[a-z_][a-z0-9_]*(?=\\()",
      relevance: 0,
    };

    // Type names: user types and typedefs
    const TYPE_NAME = {
      className: "type",
      begin: "\\b[A-Z][a-zA-Z0-9_]*\\b",
      relevance: 0,
    };

    return {
      name: "Graphix",
      aliases: ["gx"],
      keywords: {
        keyword: KEYWORDS,
        literal: "true false null",
        built_in: TYPES,
      },
      contains: [
        // Documentation comments (must come before regular comments)
        {
          className: "comment",
          begin: "///",
          end: "$",
          relevance: 10,
        },
        hljs.COMMENT("//", "$"),
        ATTRIBUTE,
        STRING,
        VARIANT,
        TYPE_PARAM,
        LABEL,
        MODULE_PATH,
        FUNCTION_CALL,
        DURATION,
        NUMBER,
        TYPE_NAME,
        REFERENCE,
        OPERATORS,
      ],
    };
  }

  // Register the language with highlight.js
  // This script is loaded via additional-js, so hljs is already available
  if (typeof hljs !== "undefined") {
    hljs.registerLanguage("graphix", graphix);
    hljs.registerLanguage("gx", graphix); // Also register the 'gx' alias

    // Wait a bit for book.js to finish its initial highlighting pass
    // then re-highlight all graphix code blocks with our newly registered language
    setTimeout(function () {
      var blocks = document.querySelectorAll(
        "code.language-graphix, code.language-gx",
      );

      // Escape HTML entities to prevent <i64> etc. from being interpreted as tags
      function escapeHtml(text) {
        return text
          .replace(/&/g, "&amp;")
          .replace(/</g, "&lt;")
          .replace(/>/g, "&gt;");
      }

      blocks.forEach(function (block) {
        // Clear any existing highlighting
        block.removeAttribute("data-highlighted");
        block.classList.remove("hljs");
        block.innerHTML = escapeHtml(block.textContent); // Reset to escaped plain text

        // Apply our highlighting (use highlightBlock for older hljs versions)
        if (typeof hljs.highlightElement === "function") {
          hljs.highlightElement(block);
        } else if (typeof hljs.highlightBlock === "function") {
          hljs.highlightBlock(block);
        }
      });
    }, 100);
  }

  // Export for use in Node.js/CommonJS environments
  if (typeof module !== "undefined" && module.exports) {
    module.exports = graphix;
  }
})();
