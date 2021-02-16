use crate::arena::{Arena, Handle};
use std::fmt;

impl<T: fmt::Debug> fmt::Debug for Arena<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_map().entries(self.iter()).finish()
    }
}

struct DebugExpression<'a> {
    handle: Handle<super::Expression>,
    expressions: &'a Arena<super::Expression>,
}

impl DebugExpression<'_> {
    fn with_handle(&self, handle: Handle<super::Expression>) -> Self {
        DebugExpression {
            handle,
            expressions: self.expressions,
        }
    }
}

impl fmt::Debug for DebugExpression<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use super::Expression as E;
        match self.expressions[self.handle] {
            E::Access { base, index } => f
                .debug_struct("Access")
                .field("base", &self.with_handle(base))
                .field("index", &self.with_handle(index))
                .finish(),
            _ => f.debug_list().finish(),
        }
    }
}

struct DebugStatement<'a> {
    statement: &'a super::Statement,
    expressions: &'a Arena<super::Expression>,
}

impl<'a> DebugStatement<'a> {
    fn with_block(&self, statements: &'a [super::Statement]) -> DebugBlock<'a> {
        DebugBlock {
            statements,
            expressions: self.expressions,
        }
    }
}

impl fmt::Debug for DebugStatement<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use super::Statement as S;
        match *self.statement {
            S::Block(ref block) => f
                .debug_tuple("Block")
                .field(&self.with_block(block))
                .finish(),
            S::If { condition, ref accept, ref reject } => f
                .debug_struct("If")
                .field("condition", &DebugExpression {
                    handle: condition,
                    expressions: self.expressions,
                })
                .field("accept", &self.with_block(accept))
                .field("reject", &self.with_block(reject))
                .finish(),
            _ => f.debug_list().finish(),
        }
    }
}

struct DebugBlock<'a> {
    statements: &'a [super::Statement],
    expressions: &'a Arena<super::Expression>,
}

impl fmt::Debug for DebugBlock<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_list()
            .entries(self.statements.iter().map(|statement| DebugStatement {
                statement,
                expressions: self.expressions,
            }))
            .finish()
    }
}

impl fmt::Debug for super::Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let body = DebugBlock {
            statements: &self.body,
            expressions: &self.expressions,
        };
        f.debug_struct("Function").field("body", &body).finish()
    }
}
