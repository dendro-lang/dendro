use std::sync::Arc;

use pest::Span;
use smallvec::SmallVec;

use crate::token::{Delimiter, Token};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TokenTree<'i> {
    Token(Token<'i>),
    Delimited {
        open: Span<'i>,
        close: Span<'i>,
        delim: Delimiter,
        inner: TokenStream<'i>,
    },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Spacing {
    Alone,
    Joint,
}

impl<'i> From<TokenTree<'i>> for (TokenTree<'i>, Spacing) {
    fn from(value: TokenTree<'i>) -> Self {
        (value, Spacing::Alone)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub struct TokenStream<'i>(pub(crate) Arc<Vec<(TokenTree<'i>, Spacing)>>);

impl<'i> TokenStream<'i> {
    pub fn new(tokens: Vec<(TokenTree<'i>, Spacing)>) -> Self {
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
pub struct TokenStreamBuilder<'i>(SmallVec<[TokenStream<'i>; 2]>);

impl<'i> TokenStreamBuilder<'i> {
    pub fn new() -> TokenStreamBuilder<'i> {
        TokenStreamBuilder(SmallVec::new())
    }

    pub fn push<T: Into<TokenStream<'i>>>(&mut self, stream: T) {
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

    pub fn build(self) -> TokenStream<'i> {
        TokenStream::from_streams(self.0)
    }
}

impl<'i> TokenStream<'i> {
    pub fn trees(&self) -> CursorRef<'_, 'i> {
        CursorRef::new(self)
    }

    pub fn into_trees(self) -> Cursor<'i> {
        Cursor::new(self)
    }
}

impl<'a, 'i> IntoIterator for &'a TokenStream<'i> {
    type IntoIter = CursorRef<'a, 'i>;

    type Item = &'a TokenTree<'i>;

    fn into_iter(self) -> Self::IntoIter {
        CursorRef::new(self)
    }
}

impl<'i> IntoIterator for TokenStream<'i> {
    type IntoIter = Cursor<'i>;

    type Item = TokenTree<'i>;

    fn into_iter(self) -> Self::IntoIter {
        Cursor::new(self)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CursorRef<'a, 'i> {
    stream: &'a TokenStream<'i>,
    index: usize,
}

impl<'a, 'i> CursorRef<'a, 'i> {
    fn new(stream: &'a TokenStream<'i>) -> Self {
        CursorRef { stream, index: 0 }
    }

    pub fn next_with_spacing(&mut self) -> Option<&'a (TokenTree<'i>, Spacing)> {
        self.stream.0.get(self.index).map(|tree| {
            self.index += 1;
            tree
        })
    }
}

impl<'a, 'i> Iterator for CursorRef<'a, 'i> {
    type Item = &'a TokenTree<'i>;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_with_spacing().map(|(tree, _)| tree)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Cursor<'i> {
    stream: TokenStream<'i>,
    index: usize,
}

impl<'i> Cursor<'i> {
    fn new(stream: TokenStream<'i>) -> Self {
        Cursor { stream, index: 0 }
    }

    pub fn next_with_spacing_ref(&mut self) -> Option<&(TokenTree<'i>, Spacing)> {
        self.stream.0.get(self.index).map(|tree| {
            self.index += 1;
            tree
        })
    }

    pub fn next_with_spacing(&mut self) -> Option<(TokenTree<'i>, Spacing)> {
        self.next_with_spacing_ref().cloned()
    }

    pub fn look_ahead(&self, n: usize) -> Option<&TokenTree> {
        self.stream.0[self.index..].get(n).map(|(tree, _)| tree)
    }
}

impl<'i> Iterator for Cursor<'i> {
    type Item = TokenTree<'i>;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_with_spacing().map(|(tree, _)| tree)
    }
}
