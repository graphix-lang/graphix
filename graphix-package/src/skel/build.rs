// Pack this package's graphix source into a pre-parsed AST blob at build time.
// See graphix-ast-pack.
fn main() {
    graphix_ast_pack::emit().expect("packing graphix AST blob");
}
