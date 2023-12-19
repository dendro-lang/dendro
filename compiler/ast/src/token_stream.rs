use std::sync::Arc;

use dendro_span::span::DelimSpan;
use smallvec::SmallVec;

use crate::token::{Delimiter, Token};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TokenTree {
    Token(Token),
    Delimited(DelimSpan, Delimiter, TokenStream),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Spacing {
    Alone,
    Joint,
}

impl From<TokenTree> for (TokenTree, Spacing) {
    fn from(value: TokenTree) -> Self {
        (value, Spacing::Alone)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub struct TokenStream(pub(crate) Arc<Vec<(TokenTree, Spacing)>>);

impl TokenStream {
    pub fn new(tokens: Vec<(TokenTree, Spacing)>) -> Self {
        TokenStream(Arc::new(tokens))
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn from_streams(mut streams: SmallVec<[Self; 2]>) -> Self {
        match streams.len() {
            0 => TokenStream::default(),
            1 => streams.pop().unwrap(),
            _ => {
                // We are going to extend the first stream in `streams` with
                // the elements from the subsequent streams. This requires
                // using `make_mut()` on the first stream, and in practice this
                // doesn't cause cloning 99.9% of the time.
                //
                // One very common use case is when `streams` has two elements,
                // where the first stream has any number of elements within
                // (often 1, but sometimes many more) and the second stream has
                // a single element within.

                // Determine how much the first stream will be extended.
                // Needed to avoid quadratic blow up from on-the-fly
                // reallocations (#57735).
                let num_appends = streams.iter().skip(1).map(|ts| ts.len()).sum();

                // Get the first stream. If it's `None`, create an empty
                // stream.
                let mut iter = streams.drain(..);
                let mut first_stream = iter.next().unwrap().0;

                // Append the elements to the first stream, after reserving
                // space for them.
                let first_vec_mut = Arc::make_mut(&mut first_stream);
                first_vec_mut.reserve(num_appends);
                for stream in iter {
                    first_vec_mut.extend(stream.0.iter().cloned());
                }

                // Create the final `TokenStream`.
                TokenStream(first_stream)
            }
        }
    }
}

// 99.5%+ of the time we have 1 or 2 elements in this vector.
#[derive(Clone, Default)]
pub struct TokenStreamBuilder(SmallVec<[TokenStream; 2]>);

impl TokenStreamBuilder {
    pub fn new() -> TokenStreamBuilder {
        TokenStreamBuilder(SmallVec::new())
    }

    pub fn push<T: Into<TokenStream>>(&mut self, stream: T) {
        let mut stream = stream.into();

        // If `self` is not empty and the last tree within the last stream is a
        // token tree marked with `Joint`...
        if let Some(TokenStream(ref mut last_stream_lrc)) = self.0.last_mut()
            && let Some((TokenTree::Token(last_token), Spacing::Joint)) = last_stream_lrc.last()
            // ...and `stream` is not empty and the first tree within it is
            // a token tree...
            && let TokenStream(ref mut stream_lrc) = stream
            && let Some((TokenTree::Token(token), spacing)) = stream_lrc.first()
            // ...and the two tokens can be glued together...
            && let Some(glued_tok) = last_token.glue(token)
        {
            // ...then do so, by overwriting the last token
            // tree in `self` and removing the first token tree
            // from `stream`. This requires using `make_mut()`
            // on the last stream in `self` and on `stream`,
            // and in practice this doesn't cause cloning 99.9%
            // of the time.

            // Overwrite the last token tree with the merged
            // token.
            let last_vec_mut = Arc::make_mut(last_stream_lrc);
            *last_vec_mut.last_mut().unwrap() = (TokenTree::Token(glued_tok), *spacing);

            // Remove the first token tree from `stream`. (This
            // is almost always the only tree in `stream`.)
            let stream_vec_mut = Arc::make_mut(stream_lrc);
            stream_vec_mut.remove(0);

            // Don't push `stream` if it's empty -- that could
            // block subsequent token gluing, by getting
            // between two token trees that should be glued
            // together.
            if !stream.is_empty() {
                self.0.push(stream);
            }
            return;
        }
        self.0.push(stream);
    }

    pub fn build(self) -> TokenStream {
        TokenStream::from_streams(self.0)
    }
}

impl TokenStream {
    pub fn trees(&self) -> CursorRef<'_> {
        CursorRef::new(self)
    }

    pub fn into_trees(self) -> Cursor {
        Cursor::new(self)
    }
}

impl<'a> IntoIterator for &'a TokenStream {
    type IntoIter = CursorRef<'a>;

    type Item = &'a TokenTree;

    fn into_iter(self) -> Self::IntoIter {
        CursorRef::new(self)
    }
}

impl IntoIterator for TokenStream {
    type IntoIter = Cursor;

    type Item = TokenTree;

    fn into_iter(self) -> Self::IntoIter {
        Cursor::new(self)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CursorRef<'a>(&'a [(TokenTree, Spacing)]);

impl<'a> CursorRef<'a> {
    fn new(stream: &'a TokenStream) -> Self {
        CursorRef(&stream.0)
    }

    pub fn next_with_spacing(&mut self) -> Option<&'a (TokenTree, Spacing)> {
        match self {
            CursorRef([first, next @ ..]) => {
                *self = CursorRef(next);
                Some(first)
            }
            _ => None,
        }
    }
}

impl<'a> Iterator for CursorRef<'a> {
    type Item = &'a TokenTree;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_with_spacing().map(|(tree, _)| tree)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Cursor {
    stream: TokenStream,
    index: usize,
}

impl Cursor {
    fn new(stream: TokenStream) -> Self {
        Cursor { stream, index: 0 }
    }

    pub fn next_with_spacing_ref(&mut self) -> Option<&(TokenTree, Spacing)> {
        self.stream.0.get(self.index).map(|tree| {
            self.index += 1;
            tree
        })
    }

    pub fn next_with_spacing(&mut self) -> Option<(TokenTree, Spacing)> {
        self.next_with_spacing_ref().cloned()
    }

    pub fn look_ahead(&self, n: usize) -> Option<&TokenTree> {
        self.stream.0[self.index..].get(n).map(|(tree, _)| tree)
    }

    pub fn into_stream(self) -> TokenStream {
        assert_eq!(self.index, 0);
        self.stream
    }
}

impl Iterator for Cursor {
    type Item = TokenTree;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_with_spacing().map(|(tree, _)| tree)
    }
}
