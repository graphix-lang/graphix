/**
 * Syntax highlighting definition for the Graphix programming language
 * For use with highlight.js in mdbook
 */

(function () {
  "use strict";

  function graphix(hljs) {
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

    // Reference operator: &
    const REFERENCE = {
      className: "operator",
      begin: "&[a-z_]",
      relevance: 0,
    };

    // Operators
    const OPERATORS = {
      className: "operator",
      begin:
        "(<-|->|=>|~|\\?|\\$|\\.\\.|::|@|\\*(?=[a-z_])|\\||\\+|\\-|/|%|==|!=|<=|>=|<|>|&&|\\|\\|)",
    };

    // Numbers with optional type suffixes
    const NUMBER = {
      className: "number",
      variants: [
        // Hexadecimal
        {
          begin:
            "\\b0x[0-9a-fA-F]+(_?[0-9a-fA-F]+)*(i32|z32|i64|z64|u32|v32|u64|v64)?\\b",
        },
        // Decimal/Float with type suffix
        {
          begin:
            "\\b\\d+(_?\\d+)*\\.\\d+(_?\\d+)*([eE][+-]?\\d+(_?\\d+)*)?(f32|f64)?\\b",
        },
        // Float with exponent
        { begin: "\\b\\d+(_?\\d+)*[eE][+-]?\\d+(_?\\d+)*(f32|f64)?\\b" },
        // Integer with type suffix
        {
          begin:
            "\\b\\d+(_?\\d+)*(i32|z32|i64|z64|u32|v32|u64|v64|f32|f64)?\\b",
        },
      ],
      relevance: 0,
    };

    // Duration literals: 1.5s, 500ms, etc.
    const DURATION = {
      className: "number",
      begin: "\\b\\d+(\\.\\d+)?(ms|s|m|h|d)\\b",
    };

    // String with interpolation support
    const STRING = {
      className: "string",
      variants: [
        {
          begin: '"',
          end: '"',
          contains: [
            {
              className: "subst",
              begin: "\\[",
              end: "\\]",
              contains: ["self"],
            },
            {
              className: "char.escape",
              begin: "\\\\.",
              relevance: 0,
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

    // Type names: Array, Map, String, Result, Option, Error, etc.
    const TYPE_NAME = {
      className: "type",
      begin: "\\b[A-Z][a-zA-Z0-9_]*\\b",
      relevance: 0,
    };

    return {
      name: "Graphix",
      aliases: ["gx"],
      // CR claude for eric: [doc-drift] The keyword list lacks seq, seqq, until, abort,
      // flush, try, catch, cast, never, any, rec, pub, trait and impl. The book's code
      // uses most of them, and they render as plain text. built_in lists String, Bool,
      // DateTime and Duration, which are not Graphix types, and lacks List. STRING
      // knows only plain double quotes, so a triple-quoted template scans as an empty
      // string followed by a new one, with its brackets shown as a splice, and `r#` raw
      // strings are not recognized. The block-comment rule (140) and the type-suffixed
      // numbers such as `42i64` (43-66) describe syntax Graphix does not have. Take the
      // keywords from the parser's KEYWORDS (graphix-types/src/expr/parser/mod.rs:100).
      // (ide-tooling.r2-16)
      keywords: {
        keyword:
          "let fn mod type val sig use select if throws dynamic sandbox whitelist as with",
        literal: "true false null",
        built_in:
          // Only include truly built-in types, not function names
          "Array Map Result Option Error String Number Int Float Bool DateTime Duration Any",
      },
      contains: [
        // Documentation comments (must come before regular comments)
        {
          className: "comment",
          begin: "///",
          end: "$",
          relevance: 10,
        },
        // Regular comments
        hljs.COMMENT("//", "$"),
        // Block comments
        hljs.COMMENT("/\\*", "\\*/"),

        STRING,
        VARIANT,
        TYPE_PARAM,
        LABEL,
        MODULE_PATH,
        FUNCTION_CALL,
        TYPE_NAME,
        DURATION,
        NUMBER,
        OPERATORS,
        REFERENCE,

        // Lambda syntax
        {
          className: "function",
          begin: "\\|",
          end: "\\|",
          keywords: {
            keyword: "as",
          },
          contains: [
            TYPE_PARAM,
            TYPE_NAME,
            {
              className: "params",
              begin: "[a-z_][a-z0-9_]*",
              relevance: 0,
            },
          ],
        },

        // Type annotations in patterns
        {
          // CR claude for eric: [bug] This mode opens at every `:` and ends at the next
          // `,`, `)`, `}`, `]`, `=` or `>`, coloring only type names inside, so
          // struct-literal values and typed literals lose their colors: `{name:
          // "hello", age: 3}` and `i64:3` render with no spans. The keywords (123) lack
          // seq, seqq, until, try, catch, cast, never, rec, trait, impl, pub, abort and
          // flush, so the seq chapter's examples show no keyword colored; the lambda
          // mode (155-171) never matches because OPERATORS takes every `|` first; the
          // `/*` comment mode (140), which Graphix does not have, turns `total /*count
          // + 1` into a comment; and triple-quoted and `r#` strings split into
          // fragments. probe: run docs/book/highlight-abc7f01d.js (hljs 10.1.1) and
          // this file in node and call hljs.highlight('graphix', src). (ide-tooling-13)
          begin: ":\\s*",
          end: "(?=[,\\)\\}\\]=>]|$)",
          keywords: {
            built_in:
              "Array Map Result Option Error String Number Int Float Bool DateTime Duration Any",
          },
          contains: [
            TYPE_PARAM,
            TYPE_NAME,
            {
              className: "type",
              begin:
                "\\b(i32|z32|i64|z64|u32|v32|u64|v64|f32|f64|bool|string|null|error|array|datetime|duration|decimal)\\b",
            },
          ],
          relevance: 0,
        },
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
