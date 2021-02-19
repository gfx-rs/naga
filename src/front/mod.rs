//! Parsers which load shaders into memory.

#[cfg(feature = "glsl-in")]
pub mod glsl;
#[cfg(feature = "spv-in")]
pub mod spv;
#[cfg(feature = "wgsl-in")]
pub mod wgsl;

use crate::arena::{Arena, Handle};

impl super::Block {
    pub fn extend(&mut self, mut other: super::Block) {
        //TODO: this isn't very nice, really. I wish we didn't need to do this.
        other.expressions.retain(|e| !self.expressions.contains(e));
        self.expressions.extend(other.expressions);
        self.statements.extend(other.statements);
    }
}

impl super::Statement {
    pub fn into_block(self) -> super::Block {
        super::Block {
            expressions: Vec::new(),
            statements: vec![self],
        }
    }
}

#[allow(dead_code)]
struct ExpressionArena<'a> {
    arena: &'a mut Arena<crate::Expression>,
    handles: &'a mut Vec<Handle<crate::Expression>>,
}

#[allow(dead_code)]
impl ExpressionArena<'_> {
    fn reborrow(&mut self) -> ExpressionArena<'_> {
        ExpressionArena {
            arena: self.arena,
            handles: self.handles,
        }
    }

    fn add(&mut self, expr: crate::Expression) -> Handle<crate::Expression> {
        let handle = self.arena.append(expr);
        self.handles.push(handle);
        handle
    }
}
