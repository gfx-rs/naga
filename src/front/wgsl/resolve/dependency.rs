use crate::front::wgsl::resolve::ir::{Decl, DeclDependency, DeclId, DeclKind, TranslationUnit};
use crate::front::wgsl::WgslError;
use crate::Span;

pub fn resolve_all_dependencies(module: &mut TranslationUnit, diagnostics: &mut Vec<WgslError>) {
    let order = DependencySolver::new(module, diagnostics).solve();
    module.dependency_order = order;
}

struct DependencySolver<'a> {
    module: &'a TranslationUnit,
    diagnostics: &'a mut Vec<WgslError>,
    visited: Vec<bool>,
    temp_visited: Vec<bool>,
    path: Vec<DeclDependency>,
    out: Vec<DeclId>,
}

impl<'a> DependencySolver<'a> {
    fn new(module: &'a TranslationUnit, diagnostics: &'a mut Vec<WgslError>) -> Self {
        let len = module.decls.len();
        Self {
            module,
            diagnostics,
            visited: vec![false; len],
            temp_visited: vec![false; len],
            path: Vec::with_capacity(len),
            out: Vec::with_capacity(len),
        }
    }

    fn solve(mut self) -> Vec<DeclId> {
        for id in 0..self.module.decls.len() {
            if self.visited[id] {
                continue;
            }
            self.dfs(id);
        }

        self.out
    }

    fn dfs(&mut self, id: usize) {
        let decl = &self.module.decls[id];
        if self.visited[id] {
            return;
        }

        self.temp_visited[id] = true;
        for dep in decl.dependencies.iter() {
            let dep_id = dep.id.0 as usize;
            self.path.push(*dep);

            if self.temp_visited[dep_id] {
                // found a cycle.
                if dep_id == id {
                    self.diagnostics.push(
                        WgslError::new("recursive declarations are not allowed")
                            .marker(decl_ident_span(&decl))
                            .label(dep.usage, "uses itself here"),
                    )
                } else {
                    let mut error = WgslError::new("cyclic declarations are not allowed").label(
                        decl_ident_span(&self.module.decls[dep_id]),
                        "this declaration",
                    );

                    let start_at = self
                        .path
                        .iter()
                        .rev()
                        .enumerate()
                        .find(|(_, dep)| dep.id.0 as usize == dep_id)
                        .map(|x| x.0)
                        .unwrap_or(0);

                    let last = self.path.len() - start_at - 1;
                    for (i, curr_dep) in self.path[start_at..].iter().enumerate() {
                        let curr_id = curr_dep.id.0 as usize;
                        let curr_decl = &self.module.decls[curr_id];

                        error.labels.push((
                            curr_dep.usage,
                            if i == last {
                                "ending the cycle".to_string()
                            } else {
                                "uses".to_string()
                            },
                        ));
                        error
                            .labels
                            .push((decl_ident_span(curr_decl), "".to_string()));
                    }

                    self.diagnostics.push(error);
                }
            } else if !self.visited[dep_id] {
                self.dfs(dep_id);
            }

            self.path.pop();
        }

        self.temp_visited[id] = false;
        self.visited[id] = true;
        self.out.push(DeclId(id as _));
    }
}

fn decl_ident_span(decl: &Decl) -> Span {
    match &decl.kind {
        DeclKind::Fn(f) => f.name.span,
        DeclKind::Struct(s) => s.name.span,
        DeclKind::Type(t) => t.name.span,
        DeclKind::Const(c) => c.name.span,
        DeclKind::Override(o) => o.name.span,
        DeclKind::Var(v) => v.inner.name.span,
        DeclKind::StaticAssert(_) => unreachable!(),
    }
}
