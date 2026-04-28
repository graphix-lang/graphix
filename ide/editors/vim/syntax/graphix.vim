" Syntax highlighting for Graphix
" Language: Graphix
" Maintainer: Graphix Authors

if exists('b:current_syntax')
  finish
endif

" Keywords
syntax keyword graphixKeyword let rec mod use type fn select if try catch cast any with where throws as

" Built-in constants
syntax keyword graphixConstant true false null ok

" Built-in types
syntax keyword graphixBuiltinType bool string bytes i8 u8 i16 u16 i32 u32 v32 z32 i64 u64 v64 z64 f32 f64 decimal datetime duration Any Array Map

" Type identifiers (capitalized)
syntax match graphixType /\<[A-Z][a-zA-Z0-9_]*\>/

" Type variables ('a, 'b, etc.)
syntax match graphixTypeVar /'[a-z][a-zA-Z0-9_]*/

" Labeled parameters and arguments (#name)
syntax match graphixLabel /#[a-z][a-zA-Z0-9_]*/

" Variant constructors (`Name)
syntax match graphixVariant /`[A-Z][a-zA-Z0-9_]*/

" Operators
syntax match graphixOperator /\(<-\|=>\|::\|->\|==\|!=\|<=\|>=\|&&\|||\|[+\-*/%=<>!~?$&@.,:;]\)/

" Numbers
syntax match graphixNumber /\<\d[0-9_]*\%(\.\d[0-9_]*\)\?\%([eE][+-]\?\d[0-9_]*\)\?\>/
syntax match graphixNumber /\<0[xX][0-9a-fA-F_]\+\>/
syntax match graphixNumber /\<0[oO][0-7_]\+\>/
syntax match graphixNumber /\<0[bB][01_]\+\>/

" Strings
syntax region graphixString start=/"/ skip=/\\\\\|\\"/ end=/"/ contains=graphixEscape,graphixInterpolation
syntax region graphixRawString start=/r"/ end=/"/

" Escape sequences
syntax match graphixEscape /\\[nrt\\'"0]/ contained
syntax match graphixEscape /\\x[0-9a-fA-F]\{2}/ contained
syntax match graphixEscape /\\u{[0-9a-fA-F]\+}/ contained

" String interpolation [expr]
syntax region graphixInterpolation start=/\[/ end=/\]/ contained contains=TOP

" Comments
syntax match graphixComment /\/\/.*$/ contains=graphixTodo
syntax match graphixDocComment /\/\/\/.*$/ contains=graphixTodo

" TODO/FIXME/XXX in comments
syntax keyword graphixTodo TODO FIXME XXX NOTE HACK contained

" Highlight links
highlight default link graphixKeyword Keyword
highlight default link graphixConstant Constant
highlight default link graphixBuiltinType Type
highlight default link graphixType Type
highlight default link graphixTypeVar Type
highlight default link graphixLabel Identifier
highlight default link graphixVariant Constant
highlight default link graphixOperator Operator
highlight default link graphixNumber Number
highlight default link graphixString String
highlight default link graphixRawString String
highlight default link graphixEscape SpecialChar
highlight default link graphixInterpolation Special
highlight default link graphixComment Comment
highlight default link graphixDocComment SpecialComment
highlight default link graphixTodo Todo

let b:current_syntax = 'graphix'
