use std::collections::HashMap;

use crate::front::wgsl::parse::ast::*;
use crate::front::wgsl::{resolve::ir::DeclId, text::Text, WgslError};
use crate::Span;

pub struct Index {
    decls: HashMap<Text, DeclId>,
    spans: Vec<Span>,
}

impl Index {
    pub fn new() -> Self {
        Self {
            decls: HashMap::new(),
            spans: Vec::new(),
        }
    }

    fn insert(&mut self, ident: Ident) -> Option<Span> {
        let id = self.spans.len() as u32;
        let old = self.decls.insert(ident.name, DeclId(id));
        self.spans.push(ident.span);
        old.map(|id| self.spans[id.0 as usize])
    }

    pub fn get(&self, ident: Text) -> Option<DeclId> {
        self.decls.get(&ident).copied()
    }

    pub fn reset(&mut self) {
        self.decls.clear();
        self.spans.clear();
    }

    pub fn generate(&mut self, tu: &TranslationUnit, diagnostics: &mut Vec<WgslError>) {
        self.reset();

        for decl in tu.decls.iter() {
            let prev = match decl.kind {
                GlobalDeclKind::Fn(ref f) => self.insert(f.name),
                GlobalDeclKind::Override(ref o) => self.insert(o.name),
                GlobalDeclKind::Var(ref v) => self.insert(v.inner.name),
                GlobalDeclKind::Const(ref c) => self.insert(c.name),
                GlobalDeclKind::Struct(ref s) => self.insert(s.name),
                GlobalDeclKind::Type(ref ty) => self.insert(ty.name),
                GlobalDeclKind::StaticAssert(_) => None,
                GlobalDeclKind::Let(ref l) => {
                    diagnostics.push(
                        WgslError::new("global `let`s are deprecated")
                            .marker(decl.span)
                            .note("consider making it a `const`"),
                    );
                    self.insert(l.name)
                }
            };

            if let Some(prev) = prev {
                diagnostics.push(
                    WgslError::new("duplicate declaration")
                        .label(prev, "previously declared here")
                        .label(decl_ident_span(decl), "redeclared here"),
                );
            }
        }
    }
}

fn decl_ident_span(decl: &GlobalDecl) -> Span {
    match decl.kind {
        GlobalDeclKind::Fn(ref f) => f.name.span,
        GlobalDeclKind::Struct(ref s) => s.name.span,
        GlobalDeclKind::Type(ref t) => t.name.span,
        GlobalDeclKind::Const(ref c) => c.name.span,
        GlobalDeclKind::Override(ref o) => o.name.span,
        GlobalDeclKind::Var(ref v) => v.inner.name.span,
        GlobalDeclKind::Let(ref l) => l.name.span,
        GlobalDeclKind::StaticAssert(_) => unreachable!(),
    }
}
