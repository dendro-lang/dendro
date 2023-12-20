fn main() {
    println!("cargo:rerun-if-changed=src/ast.lalrpop");
    lalrpop::Configuration::new().process().unwrap();
}
