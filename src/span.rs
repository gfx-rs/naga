use std::ops::Range;

/// A source code span, used for error reporting.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Span {
    /// Span is unknown - no source information.
    Unknown,
    /// Byte range.
    ByteRange { start: usize, end: usize },
}

impl Default for Span {
    fn default() -> Self {
        Self::Unknown
    }
}

impl Span {
    pub fn subsume(&mut self, other: Self) {
        match *self {
            Self::Unknown => *self = other,
            Self::ByteRange {
                ref mut start,
                ref mut end,
            } => {
                if let Self::ByteRange {
                    start: other_start,
                    end: other_end,
                } = other
                {
                    *start = (*start).min(other_start);
                    *end = (*end).max(other_end);
                }
            }
        }
    }

    pub fn total_span<T: Iterator<Item = Self>>(from: T) -> Self {
        let mut span: Self = Default::default();
        for other in from {
            span.subsume(other);
        }
        span
    }
}

impl From<Range<usize>> for Span {
    fn from(range: Range<usize>) -> Self {
        Span::ByteRange {
            start: range.start,
            end: range.end,
        }
    }
}
