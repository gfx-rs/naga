use crate::{Arena, Handle, UniqueArena};
use std::error::Error;
use std::fmt;
use std::ops::Range;

/// A source code span, used for error reporting.
#[derive(Clone, Copy, Debug, PartialEq, Default)]
pub struct Span {
    start: u32,
    end: u32,
}

impl Span {
    pub const UNDEFINED: Self = Self { start: 0, end: 0 };
    /// Creates a new `Span` from a range of byte indices
    ///
    /// Note: end is exclusive, it doesn't belong to the `Span`
    pub fn new(start: u32, end: u32) -> Self {
        Span { start, end }
    }

    /// Modifies `self` to contain the smallest `Span` possible that
    /// contains both `self` and `other`
    pub fn subsume(&mut self, other: Self) {
        *self = if !self.is_defined() {
            // self isn't defined so use other
            other
        } else if !other.is_defined() {
            // other isn't defined so don't try to subsume
            *self
        } else {
            // Both self and other are defined so calculate the span that contains them both
            Span {
                start: self.start.min(other.start),
                end: self.end.max(other.end),
            }
        }
    }

    /// Returns the smallest `Span` possible that contains all the `Span`s
    /// defined in the `from` iterator
    pub fn total_span<T: Iterator<Item = Self>>(from: T) -> Self {
        let mut span: Self = Default::default();
        for other in from {
            span.subsume(other);
        }
        span
    }

    /// Converts `self` to a range if the span is not unknown
    pub fn to_range(self) -> Option<Range<usize>> {
        if self.is_defined() {
            Some(self.start as usize..self.end as usize)
        } else {
            None
        }
    }

    /// Check wether `self` was defined or is a default/unknown span
    pub fn is_defined(&self) -> bool {
        *self != Self::default()
    }
}

impl From<Range<usize>> for Span {
    fn from(range: Range<usize>) -> Self {
        Span {
            start: range.start as u32,
            end: range.end as u32,
        }
    }
}

pub type SpanContext = (Span, String);

#[derive(Debug)]
pub struct WithSpan<E> {
    inner: E,
    #[cfg(feature = "span")]
    spans: Vec<SpanContext>,
}

impl<E> fmt::Display for WithSpan<E>
where
    E: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::fmt::Result {
        self.inner.fmt(f)
    }
}

#[cfg(test)]
impl<E> PartialEq for WithSpan<E>
where
    E: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.inner.eq(&other.inner)
    }
}

impl<E> Error for WithSpan<E>
where
    E: Error,
{
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        self.inner.source()
    }
}

impl<E> WithSpan<E> {
    pub fn new(inner: E) -> Self {
        Self {
            inner,
            #[cfg(feature = "span")]
            spans: Vec::new(),
        }
    }

    pub fn into_inner(self) -> E {
        self.inner
    }

    pub fn spans(&self) -> impl Iterator<Item = &SpanContext> {
        #[cfg(feature = "span")]
        return self.spans.iter();
        #[cfg(not(feature = "span"))]
        return std::iter::empty();
    }

    #[cfg_attr(not(feature = "span"), allow(unused_variables, unused_mut))]
    pub fn with_span<S>(mut self, span: Span, description: S) -> Self
    where
        S: ToString,
    {
        #[cfg(feature = "span")]
        if span.is_defined() {
            self.spans.push((span, description.to_string()));
        }
        self
    }

    pub fn with_context(self, span_context: SpanContext) -> Self {
        let (span, description) = span_context;
        self.with_span(span, description)
    }

    #[cfg_attr(not(feature = "span"), allow(unused_variables, unused_mut))]
    pub fn with_contexts<T>(mut self, span_contexts: T) -> Self
    where
        T: IntoIterator<Item = SpanContext>,
    {
        #[cfg(feature = "span")]
        self.spans
            .extend(span_contexts.into_iter().filter(|&(s, _)| s.is_defined()));
        self
    }

    pub(crate) fn with_handle<T, A: SpanProvider<T>>(self, handle: Handle<T>, arena: &A) -> Self {
        self.with_context(arena.get_span_context(handle))
    }

    pub fn into_other<E2>(self) -> WithSpan<E2>
    where
        E2: From<E>,
    {
        WithSpan {
            inner: self.inner.into(),
            #[cfg(feature = "span")]
            spans: self.spans,
        }
    }

    pub fn and_then<F, E2>(self, func: F) -> WithSpan<E2>
    where
        F: FnOnce(E) -> WithSpan<E2>,
    {
        #[cfg_attr(not(feature = "span"), allow(unused_mut))]
        let mut res = func(self.inner);
        #[cfg(feature = "span")]
        res.spans.extend(self.spans);
        res
    }
}

/// Convenience trait for [`Error`] to be able to apply spans to anything
pub(crate) trait AddSpan: Sized {
    type Output;
    fn with_span(self) -> Self::Output;
    fn with_span_static(self, span: Span, description: &'static str) -> Self::Output;
    fn with_span_context(self, span_context: SpanContext) -> Self::Output;
    fn with_span_handle<T, A: SpanProvider<T>>(self, handle: Handle<T>, arena: &A) -> Self::Output;
}

pub(crate) trait AddSpanResult: Sized {
    type Output;
    fn with_span(self) -> Self::Output;
}

/// Convenience trait for Arena and UniqueArena
pub(crate) trait SpanProvider<T> {
    fn get_span(&self, handle: Handle<T>) -> Span;
    fn get_span_context(&self, handle: Handle<T>) -> SpanContext {
        match self.get_span(handle) {
            x if !x.is_defined() => (Default::default(), "".to_string()),
            known => (
                known,
                format!("{} {:?}", std::any::type_name::<T>(), handle),
            ),
        }
    }
}

impl<T> SpanProvider<T> for Arena<T> {
    fn get_span(&self, handle: Handle<T>) -> Span {
        self.get_span(handle)
    }
}

impl<T> SpanProvider<T> for UniqueArena<T> {
    fn get_span(&self, handle: Handle<T>) -> Span {
        self.get_span(handle)
    }
}

impl<E> AddSpan for E
where
    E: Error,
{
    type Output = WithSpan<Self>;
    fn with_span(self) -> WithSpan<Self> {
        WithSpan::new(self)
    }

    fn with_span_static(self, span: Span, description: &'static str) -> WithSpan<Self> {
        WithSpan::new(self).with_span(span, description)
    }

    fn with_span_context(self, span_context: SpanContext) -> WithSpan<Self> {
        WithSpan::new(self).with_context(span_context)
    }

    fn with_span_handle<T, A: SpanProvider<T>>(
        self,
        handle: Handle<T>,
        arena: &A,
    ) -> WithSpan<Self> {
        WithSpan::new(self).with_handle(handle, arena)
    }
}

impl<T, E> AddSpanResult for Result<T, E>
where
    E: Error,
{
    type Output = Result<T, WithSpan<E>>;

    fn with_span(self) -> Self::Output {
        self.map_err(|e| e.with_span())
    }
}

/// Convenience trait for Result
pub trait MapErrWithSpan<E, E2>: Sized {
    type Output: Sized;
    fn map_err_inner<F, E3>(self, func: F) -> Self::Output
    where
        F: FnOnce(E) -> WithSpan<E3>,
        E2: From<E3>;
}

impl<T, E, E2> MapErrWithSpan<E, E2> for Result<T, WithSpan<E>> {
    type Output = Result<T, WithSpan<E2>>;
    fn map_err_inner<F, E3>(self, func: F) -> Result<T, WithSpan<E2>>
    where
        F: FnOnce(E) -> WithSpan<E3>,
        E2: From<E3>,
    {
        self.map_err(|e| e.and_then(func).into_other::<E2>())
    }
}
