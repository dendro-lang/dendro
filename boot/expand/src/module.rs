use std::{
    fs,
    path::{Path, PathBuf, MAIN_SEPARATOR},
};

use dendro_ast::ast::Leaf;
use dendro_error::{DiagCx, Error};
use dendro_span::{ident::Ident, source::SourceMap, span::Span};

enum ModError {
    NotFound(Ident, PathBuf, PathBuf),
    Ambiguous(Ident, PathBuf, PathBuf),
    CyclingDep(usize),
    Io(std::io::Error, PathBuf),
    Diag(Error),
}

#[derive(Debug, Default)]
struct SubmodPath {
    path: PathBuf,
    relative: Option<Ident>,
}

#[derive(Debug, Default)]
pub struct Module {
    pub path: PathBuf,
    pub prefix: PathBuf,

    pub file_stack: Vec<PathBuf>,
}

pub fn parse(diag: &DiagCx, sources: &SourceMap, cur: &Module, ident: Ident) -> (Leaf, Module) {
    let result: Result<_, ModError> = try {
        let submod = submod_path(&cur.prefix, ident)?;
        if let Some(pos) = cur.file_stack.iter().position(|p| p == &submod.path) {
            Err(ModError::CyclingDep(pos))?;
        }
        let src =
            fs::read_to_string(&submod.path).map_err(|e| ModError::Io(e, submod.path.clone()))?;
        let sfile = sources.new_source_file(submod.path.clone(), src);
        let tts = dendro_lexer::parse(&sfile, diag);
        let leaf = dendro_parse::parse(diag, &tts).unwrap();
        (leaf, submod)
    };
    let (leaf, submod) = result
        .map_err(|err| report_err(diag, cur, ident.span, err))
        .unwrap_or_default();

    let prefix = match submod.relative {
        Some(rel) => cur.prefix.join(rel),
        None => cur.prefix.to_path_buf(),
    };

    let module = Module {
        path: submod.path.clone(),
        prefix,
        file_stack: cur
            .file_stack
            .iter()
            .cloned()
            .chain([submod.path])
            .collect(),
    };
    (leaf, module)
}

fn submod_path(prefix: &Path, ident: Ident) -> Result<SubmodPath, ModError> {
    default_submod_path(prefix, ident)
}

fn default_submod_path(prefix: &Path, ident: Ident) -> Result<SubmodPath, ModError> {
    let same_dir = prefix.join(format!("{ident}.dd"));
    let subdir = prefix.join(format!("{ident}{MAIN_SEPARATOR}mod.dd"));

    match (same_dir.exists(), subdir.exists()) {
        (true, false) => Ok(SubmodPath {
            path: same_dir,
            relative: Some(ident),
        }),
        (false, true) => Ok(SubmodPath { path: subdir, relative: None }),
        (false, false) => Err(ModError::NotFound(ident, same_dir, subdir)),
        (true, true) => Err(ModError::Ambiguous(ident, same_dir, subdir)),
    }
}

fn report_err(diag: &DiagCx, cur: &Module, span: Span, err: ModError) -> Error {
    match err {
        ModError::NotFound(ident, same_dir, subdir) => diag.span_err(
            span,
            format_args!(
                "file not found for external block `{ident}` in {} or {};\n\
                please create either of them\
                or specify the full path to the file via `#[path = \"..\"].`",
                same_dir.display(),
                subdir.display(),
            ),
        ),
        ModError::Ambiguous(ident, same_dir, subdir) => diag.span_err(
            span,
            format_args!(
                "file for external block `{ident}` both found in {} and {};\n\
                please rename or delete either of them to resolve the ambiguity.",
                same_dir.display(),
                subdir.display(),
            ),
        ),
        ModError::CyclingDep(pos) => {
            let dep = cur.file_stack[pos..]
                .iter()
                .chain([&cur.file_stack[pos]])
                .map(|path| path.display().to_string())
                .reduce(|a, b| a + " -> " + &b)
                .unwrap_or_default();
            diag.span_err(span, format_args!("circular file dependency:\n{dep}"))
        }
        ModError::Io(err, path) => diag.span_err(
            span,
            format_args!("failed to read {}: {err}", path.display()),
        ),
        ModError::Diag(err) => err,
    }
}
