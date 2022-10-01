use std::collections::HashMap;

use crate::front::wgsl::parse::ast::*;
use crate::front::wgsl::{resolve::ir::DeclId, text::Text, WgslError};
use crate::Span;

pub struct Index {
    decls: HashMap<Text, DeclId>,
    spans: Vec<Span>,
}

impl Index {
    fn insert(&mut self, ident: Ident) -> Option<Span> {
        let id = self.spans.len() as u32;
        let old = self.decls.insert(ident.name, DeclId(id));
        self.spans.push(ident.span);
        old.map(|id| self.spans[id.0 as usize])
    }

    pub fn get(&self, ident: Text) -> Option<DeclId> {
        self.decls.get(&ident).copied()
    }
}

pub fn generate_index(tu: &TranslationUnit, diagnostics: &mut Vec<WgslError>) -> Index {
    let mut index = Index {
        decls: HashMap::new(),
        spans: Vec::new(),
    };

    for decl in tu.decls.iter() {
        let prev = match decl.kind {
            GlobalDeclKind::Fn(ref f) => index.insert(f.name),
            GlobalDeclKind::Override(ref o) => index.insert(o.name),
            GlobalDeclKind::Var(ref v) => index.insert(v.inner.name),
            GlobalDeclKind::Const(ref c) => index.insert(c.name),
            GlobalDeclKind::Struct(ref s) => index.insert(s.name),
            GlobalDeclKind::Type(ref ty) => index.insert(ty.name),
            GlobalDeclKind::StaticAssert(_) => None,
            GlobalDeclKind::Let(ref l) => {
                diagnostics.push(
                    WgslError::new("global `let`s are deprecated")
                        .marker(decl.span)
                        .note("consider making it a `const`"),
                );
                index.insert(l.name)
            }
        };

        if let Some(prev) = prev {
            diagnostics.push(
                WgslError::new("duplicate declaration")
                    .label(prev, "previously declared here")
                    .label(decl.span, "redeclared here"),
            );
        }
    }

    index
}
