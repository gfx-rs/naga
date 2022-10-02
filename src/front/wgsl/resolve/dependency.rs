use crate::front::wgsl::resolve::ir::{Decl, DeclDependency, DeclId, DeclKind, TranslationUnit};
use crate::front::wgsl::WgslError;
use crate::Span;

pub struct DependencyContext {
    visited: Vec<bool>,
    temp_visited: Vec<bool>,
    path: Vec<DeclDependency>,
    out: Vec<DeclId>,
}

impl DependencyContext {
    pub fn new() -> Self {
        Self {
            visited: Vec::new(),
            temp_visited: Vec::new(),
            path: Vec::new(),
            out: Vec::new(),
        }
    }

    pub fn resolve(&mut self, module: &mut TranslationUnit, diagnostics: &mut Vec<WgslError>) {
        self.reset_for(module);
        let solver = DependencySolver {
            module,
            diagnostics,
            ctx: self,
        };
        solver.solve();
        module.dependency_order = std::mem::take(&mut self.out);
    }

    pub fn reset_for(&mut self, module: &TranslationUnit) {
        self.visited.clear();
        self.visited.resize(module.decls.len(), false);
        self.temp_visited.clear();
        self.temp_visited.resize(module.decls.len(), false);
        self.path.clear();
        self.path.reserve(module.decls.len());
        self.out.reserve(module.decls.len());
    }
}

struct DependencySolver<'a> {
    module: &'a TranslationUnit,
    diagnostics: &'a mut Vec<WgslError>,
    ctx: &'a mut DependencyContext,
}

impl<'a> DependencySolver<'a> {
    fn solve(mut self) {
        for id in 0..self.module.decls.len() {
            if self.ctx.visited[id] {
                continue;
            }
            self.dfs(id);
        }
    }

    fn dfs(&mut self, id: usize) {
        let decl = &self.module.decls[id];
        if self.ctx.visited[id] {
            return;
        }

        self.ctx.temp_visited[id] = true;
        for dep in decl.dependencies.iter() {
            let dep_id = dep.id.0 as usize;
            self.ctx.path.push(*dep);

            if self.ctx.temp_visited[dep_id] {
                // found a cycle.
                if dep_id == id {
                    self.diagnostics.push(
                        WgslError::new("recursive declarations are not allowed")
                            .marker(decl_ident_span(decl))
                            .label(dep.usage, "uses itself here"),
                    )
                } else {
                    let mut error = WgslError::new("cyclic declarations are not allowed").label(
                        decl_ident_span(&self.module.decls[dep_id]),
                        "this declaration",
                    );

                    let start_at = self
                        .ctx
                        .path
                        .iter()
                        .rev()
                        .enumerate()
                        .find(|&(_, dep)| dep.id.0 as usize == dep_id)
                        .map(|x| x.0)
                        .unwrap_or(0);

                    let last = self.ctx.path.len() - start_at - 1;
                    for (i, curr_dep) in self.ctx.path[start_at..].iter().enumerate() {
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
            } else if !self.ctx.visited[dep_id] {
                self.dfs(dep_id);
            }

            self.ctx.path.pop();
        }

        self.ctx.temp_visited[id] = false;
        self.ctx.visited[id] = true;
        self.ctx.out.push(DeclId(id as _));
    }
}

fn decl_ident_span(decl: &Decl) -> Span {
    match decl.kind {
        DeclKind::Fn(ref f) => f.name.span,
        DeclKind::Struct(ref s) => s.name.span,
        DeclKind::Type(ref t) => t.name.span,
        DeclKind::Const(ref c) => c.name.span,
        DeclKind::Override(ref o) => o.name.span,
        DeclKind::Var(ref v) => v.inner.name.span,
        DeclKind::StaticAssert(_) => unreachable!(),
    }
}
