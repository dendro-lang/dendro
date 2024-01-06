use std::{env, fs, process};

use dendro_error::DiagCx;

fn run_compiler(args: &[String]) {
    let mut paths = args.iter().filter(|&arg| !arg.starts_with('-'));
    let path = paths.next().expect("please specify the root input file");
    
    let input = fs::read_to_string(path).expect("failed to read input file");

    let diag = DiagCx::default();
    let tts = dendro_lexer::parse(&input, &diag);

    let leaf = dendro_parse::parse(&diag, &tts).unwrap();
    println!("{leaf:?}");
}

pub fn main() -> ! {
    println!("Hello dendro!");
    let args = env::args().collect::<Vec<_>>();
    run_compiler(&args[1..]);
    process::exit(0)
}
