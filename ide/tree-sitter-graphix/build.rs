fn main() {
    let src_dir = std::path::Path::new("src");
    let mut c_config = cc::Build::new();
    c_config.std("c11").include(src_dir);
    let parser_path = src_dir.join("parser.c");
    let scanner_path = src_dir.join("scanner.c");
    c_config.file(&parser_path);
    c_config.file(&scanner_path);
    println!("cargo:rerun-if-changed={}", parser_path.display());
    println!("cargo:rerun-if-changed={}", scanner_path.display());
    c_config.compile("tree-sitter-graphix");
}
