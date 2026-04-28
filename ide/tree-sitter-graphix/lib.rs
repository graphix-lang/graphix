use tree_sitter_language::LanguageFn;

unsafe extern "C" {
    fn tree_sitter_graphix() -> *const ();
}

pub const LANGUAGE: LanguageFn = unsafe { LanguageFn::from_raw(tree_sitter_graphix) };
