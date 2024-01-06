fn main() {
    println!("cargo:rerun-if-changed=src/lexer.pest");
}
