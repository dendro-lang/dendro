use std::{
    collections::HashMap,
    path::{Path, PathBuf},
    sync::{Arc, RwLock},
};

use crate::{
    fatal_error,
    span::{Pos, RelPos, Span},
    Loc, LocSpan,
};

#[derive(Debug)]
pub struct OffsetOverflow;

#[derive(Debug, PartialEq, Eq)]
pub struct SourceFile {
    pub path: PathBuf,
    pub src: String,

    pub start_pos: Pos,
    pub source_len: RelPos,
    pub lines: Vec<RelPos>,
}

fn lines_from(src: &str) -> Vec<RelPos> {
    let split = src.split_inclusive('\n');
    let mut pos = 0;
    let iter = split.map(|line| {
        let stripped = line.strip_prefix('\r').unwrap_or(line);
        pos += line.len();
        RelPos(pos - stripped.len())
    });
    iter.collect()
}

impl SourceFile {
    pub fn new(path: PathBuf, src: String) -> Self {
        let source_len = RelPos(src.len());
        let lines = lines_from(&src);
        SourceFile {
            path,
            src,
            start_pos: Pos(0),
            source_len,
            lines,
        }
    }

    pub fn test(s: impl ToString) -> Self {
        SourceFile::new("test".into(), s.to_string())
    }

    pub fn relative_pos(&self, pos: Pos) -> RelPos {
        pos - self.start_pos
    }

    pub fn absolute_pos(&self, pos: RelPos) -> Pos {
        self.start_pos + pos
    }

    pub fn end_pos(&self) -> Pos {
        self.absolute_pos(self.source_len)
    }

    pub fn line_index(&self, pos: RelPos) -> Option<usize> {
        self.lines.partition_point(|x| x <= &pos).checked_sub(1)
    }

    pub fn line_col(&self, pos: Pos) -> (usize, usize) {
        let pos = self.relative_pos(pos);
        match self.line_index(pos) {
            Some(a) => (a + 1, pos.0 - self.lines[a].0),
            None => (0, pos.0),
        }
    }
}

#[derive(Default, Debug)]
struct SourceFiles {
    files: Vec<Arc<SourceFile>>,
    hashed: HashMap<PathBuf, Arc<SourceFile>>,
}

#[derive(Default)]
pub struct SourceMap {
    source_files: RwLock<SourceFiles>,
}

impl SourceMap {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn try_new_source_file(
        &self,
        path: PathBuf,
        src: String,
    ) -> Result<Arc<SourceFile>, OffsetOverflow> {
        if let Some(file) = self.get(&path) {
            return Ok(file);
        }

        let mut source_files = self.source_files.write().unwrap();
        let start_pos = Pos(match source_files.files.last() {
            Some(file) => file.end_pos().0.checked_add(1).ok_or(OffsetOverflow)?,
            None => 0,
        });

        let mut file = SourceFile::new(path.clone(), src);
        file.start_pos = start_pos;
        let file = Arc::new(file);

        source_files.files.push(file.clone());
        let _old = source_files.hashed.insert(path, file.clone());
        debug_assert!(_old.is_none());

        Ok(file)
    }

    pub fn new_source_file(&self, path: PathBuf, src: String) -> Arc<SourceFile> {
        self.try_new_source_file(path, src).unwrap_or_else(|_| {
            fatal_error!("the whole sum of file lengths exceeded 4GB");
        })
    }

    pub fn get(&self, path: impl AsRef<Path>) -> Option<Arc<SourceFile>> {
        let source_files = self.source_files.read().unwrap();
        source_files.hashed.get(path.as_ref()).cloned()
    }

    pub fn contains(&self, path: impl AsRef<Path>) -> bool {
        let source_files = self.source_files.read().unwrap();
        source_files.hashed.contains_key(path.as_ref())
    }

    pub fn source_file(&self, pos: Pos) -> Arc<SourceFile> {
        let source_files = self.source_files.read().unwrap();
        let index = source_files
            .files
            .partition_point(|file| file.start_pos <= pos);
        source_files.files[index - 1].clone()
    }

    pub fn location(&self, pos: Pos) -> Loc {
        let file = self.source_file(pos);
        let (line, col) = file.line_col(pos);
        Loc { file, line, col }
    }

    pub fn spanned_location(&self, span: Span) -> LocSpan {
        let file = self.source_file(span.start);
        debug_assert_eq!(self.source_file(span.end), file);

        let (start_line, start_col) = file.line_col(span.start);
        let (end_line, end_col) = file.line_col(span.end);

        LocSpan {
            file,
            lines: start_line..end_line,
            start_col,
            end_col,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::lines_from;
    use crate::span::RelPos;

    #[test]
    fn test_lines() {
        let s = "1\n\r2\r\n3\n4\n\n5";
        let lines = lines_from(s);
        assert_eq!(lines, [
            RelPos(0),
            RelPos(3),
            RelPos(6),
            RelPos(8),
            RelPos(10),
            RelPos(11)
        ]);
    }
}
