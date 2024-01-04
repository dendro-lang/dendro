fn main() {
    println!("cargo:rerun-if-changed=src/ast.lalrpop");
    lalrpop::Configuration::new()
        .use_cargo_dir_conventions()
        .process_file("src/ast.lalrpop")
        .unwrap();
}
