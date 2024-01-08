use std::{env, fs, panic, process};

use dendro_error::{DiagCx, Error};
use dendro_span::{source::SourceMap, FatalError};

fn run_compiler(args: &[String]) -> Result<(), Error> {
    let mut paths = args.iter().filter(|&arg| !arg.starts_with('-'));
    let path = paths.next().expect("please specify the root input file");

    let source_map = SourceMap::default();
    let diag = DiagCx::default();

    let input = fs::read_to_string(path).expect("failed to read input file");
    let src = source_map.new_source_file(path.into(), input);

    let tts = dendro_lexer::parse(&src, &diag);

    let leaf = dendro_parse::parse(&diag, &tts).unwrap();

    // let leaf = dendro_expand::expand(&source_map, &diag, resolver, leaf)?;
    println!("{leaf:?}");

    // - expand:
    //   - load unloaded blocks;
    //   - resolve imports.

    // - resolve:
    //   - insert prelude;
    //   - #[alias]: aliased bindings must be of struct type and fully evaluated;
    //   - #[separate]: separate the inner namespace from the current block/leaf;
    //   - implicit arguments:
    //     - being imported alongside `a.b.mod`;
    //     - share the same namespace with other bindings;

    // - lower:
    //   - desugar `for` and `while let` to `loop match`;
    //   - desugar `if let` to `match`;
    //   - remove parentheses;

    // - reduce (partially evaluate):
    //   - implicit arguments:
    //     - if not specified, selected by the compiler at call site, with possible
    //       ambiguity error being reported.
    //   - numeral literals:
    //     - `forall a where a: type => let {{num}} ?a = {{num: a}}`;
    //     - `let {{num##prefix}} = {{num: prefix type}}`.
    Ok(())
}

pub fn main() -> ! {
    println!("Hello dendro!");
    let args = env::args().collect::<Vec<_>>();
    let code = match panic::catch_unwind(|| run_compiler(&args[1..])) {
        Ok(Ok(())) => 0,
        Ok(Err(_)) => 1,
        Err(err) if err.is::<FatalError>() => 1,
        Err(err) => panic::resume_unwind(err),
    };
    process::exit(code)
}
