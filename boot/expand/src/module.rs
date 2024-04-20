use std::{
    fs,
    path::{Path, PathBuf, MAIN_SEPARATOR},
};

use dendro_ast::{
    ast::{Attribute, Expr, ExprKind, Leaf, P},
    token::{Lit, LitKind},
};
use dendro_error::{DiagCx, Error};
use dendro_span::{
    ident::{kw, Ident},
    source::SourceMap,
    span::Span,
};

enum ModError {
    PathAttrNotFound(Ident, Span, PathBuf),
    NotFound(Ident, PathBuf, PathBuf),
    Ambiguous(Ident, PathBuf, PathBuf),
    CyclingDep(usize),
    Io(std::io::Error, PathBuf),
    Diag(Error),
}

#[derive(Debug, Default)]
struct SubmodPath {
    path: PathBuf,
    prefix: PathBuf,
}

#[derive(Debug, Default)]
pub struct Module {
    pub path: PathBuf,
    pub prefix: PathBuf,

    pub file_stack: Vec<PathBuf>,
}

pub fn parse(
    diag: &DiagCx,
    sources: &SourceMap,
    cur: &Module,
    ident: Ident,
    attrs: [&mut Vec<Attribute>; 2],
) -> (Leaf, Module) {
    let result: Result<_, ModError> = try {
        let submod = submod_path(diag, &cur.prefix, ident, attrs)?;
        if let Some(pos) = cur.file_stack.iter().position(|p| p == &submod.path) {
            Err(ModError::CyclingDep(pos))?;
        }
        let src =
            fs::read_to_string(&submod.path).map_err(|e| ModError::Io(e, submod.path.clone()))?;
        let sfile = sources.new_source_file(submod.path.clone(), src);
        let tts = dendro_lexer::parse(&sfile, diag);
        let leaf = dendro_parse::parse_or_default(diag, &tts);
        (leaf, submod)
    };
    let (leaf, submod) = result
        .map_err(|err| report_err(diag, cur, ident.span, err))
        .unwrap_or_default();

    let module = Module {
        path: submod.path.clone(),
        prefix: submod.prefix,
        file_stack: cur
            .file_stack
            .iter()
            .cloned()
            .chain([submod.path])
            .collect(),
    };
    (leaf, module)
}

fn submod_path(
    diag: &DiagCx,
    prefix: &Path,
    ident: Ident,
    [a0, a1]: [&mut Vec<Attribute>; 2],
) -> Result<SubmodPath, ModError> {
    let parse = |span, expr: P<Expr>| match expr.kind {
        ExprKind::Literal(Lit { kind: LitKind::Str, symbol, .. }) => {
            let mut path = String::new();
            dendro_lexer::unescape::unescape_string(symbol.as_str(), expr.span, |res| match res {
                Ok(ch) => path.push(ch),
                Err((err, span)) => {
                    diag.span_err(
                        span,
                        format_args!("failed to unescape string literal: {err}"),
                    );
                }
            });
            Ok((span, PathBuf::from(path)))
        }
        _ => Err(diag.span_err(span, "expected string literal")),
    };
    let mut paths = Attribute::parse_builtin_eq(a0, kw::PATH, diag, parse);
    paths.append(&mut Attribute::parse_builtin_eq(a1, kw::PATH, diag, parse));

    if paths.is_empty() {
        default_submod_path(prefix, ident)
    } else {
        let res = paths.swap_remove(0);
        (paths.into_iter().filter_map(|p| p.ok())).for_each(|(span, _)| {
            diag.span_err(span, "duplicate `#[path = ..]` attribute");
        });
        let (span, path) = res.map_err(ModError::Diag)?;
        let path = (prefix.join(&path).canonicalize()).map_err(|err| ModError::Io(err, path))?;

        let parent = path.parent().unwrap();
        let prefix = if path.ends_with("mod.dd") {
            parent.to_path_buf()
        } else {
            parent.join(path.file_prefix().unwrap())
        };

        if path.exists() {
            Ok(SubmodPath { path, prefix })
        } else {
            Err(ModError::PathAttrNotFound(ident, span, path))
        }
    }
}

fn default_submod_path(prefix: &Path, ident: Ident) -> Result<SubmodPath, ModError> {
    let same_dir = prefix.join(format!("{ident}.dd"));
    let subdir = prefix.join(format!("{ident}{MAIN_SEPARATOR}mod.dd"));
    let dir = prefix.join(ident);

    match (same_dir.exists(), subdir.exists()) {
        (true, false) => Ok(SubmodPath { path: same_dir, prefix: dir }),
        (false, true) => Ok(SubmodPath { path: subdir, prefix: dir }),
        (false, false) => Err(ModError::NotFound(ident, same_dir, subdir)),
        (true, true) => Err(ModError::Ambiguous(ident, same_dir, subdir)),
    }
}

fn report_err(diag: &DiagCx, cur: &Module, span: Span, err: ModError) -> Error {
    match err {
        ModError::PathAttrNotFound(ident, span, path) => diag.span_err(
            span,
            format_args!(
                "file not found for external block `{ident}` in {};\n\
                please create either of them\
                or specify the full path to the file via `#[path = \"..\"].`",
                path.display(),
            ),
        ),
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
