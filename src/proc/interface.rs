use crate::arena::{Arena, Handle};

pub struct Interface<'a, T> {
    expressions: &'a Arena<crate::Expression>,
    visitor: &'a mut T,
}

pub trait Visitor {
    fn visit_expr(&mut self, _: &crate::Expression) {}
    fn visit_lhs_expr(&mut self, _: &crate::Expression) {}
}

impl<'a, T> Interface<'a, T>
where
    T: Visitor,
{
    fn traverse_expr(&mut self, handle: Handle<crate::Expression>) {
        use crate::Expression as E;

        let expr = &self.expressions[handle];

        self.visitor.visit_expr(expr);

        match *expr {
            E::Access { base, index } => {
                self.traverse_expr(base);
                self.traverse_expr(index);
            }
            E::AccessIndex { base, .. } => {
                self.traverse_expr(base);
            }
            E::Constant(_) => {}
            E::Compose { ref components, .. } => {
                for &comp in components {
                    self.traverse_expr(comp);
                }
            }
            E::FunctionParameter(_) | E::GlobalVariable(_) | E::LocalVariable(_) => {}
            E::Load { pointer } => {
                self.traverse_expr(pointer);
            }
            E::ImageSample {
                image,
                sampler,
                coordinate,
                level,
                depth_ref,
            } => {
                self.traverse_expr(image);
                self.traverse_expr(sampler);
                self.traverse_expr(coordinate);
                match level {
                    crate::SampleLevel::Auto => (),
                    crate::SampleLevel::Exact(h) | crate::SampleLevel::Bias(h) => {
                        self.traverse_expr(h)
                    }
                }
                if let Some(dref) = depth_ref {
                    self.traverse_expr(dref);
                }
            }
            E::ImageLoad {
                image,
                coordinate,
                index,
            } => {
                self.traverse_expr(image);
                self.traverse_expr(coordinate);
                self.traverse_expr(index);
            }
            E::Unary { expr, .. } => {
                self.traverse_expr(expr);
            }
            E::Binary { left, right, .. } => {
                self.traverse_expr(left);
                self.traverse_expr(right);
            }
            E::Intrinsic { argument, .. } => {
                self.traverse_expr(argument);
            }
            E::Transpose(matrix) => {
                self.traverse_expr(matrix);
            }
            E::DotProduct(left, right) => {
                self.traverse_expr(left);
                self.traverse_expr(right);
            }
            E::CrossProduct(left, right) => {
                self.traverse_expr(left);
                self.traverse_expr(right);
            }
            E::As { expr, .. } => {
                self.traverse_expr(expr);
            }
            E::Derivative { expr, .. } => {
                self.traverse_expr(expr);
            }
            E::Call { ref arguments, .. } => {
                for &argument in arguments {
                    self.traverse_expr(argument);
                }
            }
        }
    }

    pub fn traverse(&mut self, block: &[crate::Statement]) {
        for statement in block {
            use crate::Statement as S;
            match *statement {
                S::Empty | S::Break | S::Continue | S::Kill => (),
                S::Block(ref b) => {
                    self.traverse(b);
                }
                S::If {
                    condition,
                    ref accept,
                    ref reject,
                } => {
                    self.traverse_expr(condition);
                    self.traverse(accept);
                    self.traverse(reject);
                }
                S::Switch {
                    selector,
                    ref cases,
                    ref default,
                } => {
                    self.traverse_expr(selector);
                    for &(ref case, _) in cases.values() {
                        self.traverse(case);
                    }
                    self.traverse(default);
                }
                S::Loop {
                    ref body,
                    ref continuing,
                } => {
                    self.traverse(body);
                    self.traverse(continuing);
                }
                S::Return { value } => {
                    if let Some(expr) = value {
                        self.traverse_expr(expr);
                    }
                }
                S::Store { pointer, value } => {
                    let mut left = pointer;
                    loop {
                        match self.expressions[left] {
                            crate::Expression::Access { base, index } => {
                                self.traverse_expr(index);
                                left = base;
                            }
                            crate::Expression::AccessIndex { base, .. } => {
                                left = base;
                            }
                            _ => break,
                        }
                    }
                    self.visitor.visit_lhs_expr(&self.expressions[left]);
                    self.traverse_expr(value);
                }
            }
        }
    }
}

struct GlobalUseVisitor(Vec<crate::GlobalUse>);

impl Visitor for GlobalUseVisitor {
    fn visit_expr(&mut self, expr: &crate::Expression) {
        if let crate::Expression::GlobalVariable(handle) = expr {
            self.0[handle.index()] |= crate::GlobalUse::LOAD;
        }
    }

    fn visit_lhs_expr(&mut self, expr: &crate::Expression) {
        if let crate::Expression::GlobalVariable(handle) = expr {
            self.0[handle.index()] |= crate::GlobalUse::STORE;
        }
    }
}

impl crate::GlobalUse {
    pub fn scan(
        expressions: &Arena<crate::Expression>,
        body: &[crate::Statement],
        globals: &Arena<crate::GlobalVariable>,
    ) -> Vec<Self> {
        let mut visitor = GlobalUseVisitor(vec![crate::GlobalUse::empty(); globals.len()]);

        let mut io = Interface {
            expressions,
            visitor: &mut visitor,
        };
        io.traverse(body);
        visitor.0
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        Arena, Expression, GlobalUse, GlobalVariable, Handle, Statement, StorageAccess,
        StorageClass,
    };

    #[test]
    fn global_use_scan() {
        let test_global = GlobalVariable {
            name: None,
            class: StorageClass::Constant,
            binding: None,
            ty: Handle::new(std::num::NonZeroU32::new(1).unwrap()),
            interpolation: None,
            storage_access: StorageAccess::empty(),
        };
        let mut test_globals = Arena::new();

        let global_1 = test_globals.append(test_global.clone());
        let global_2 = test_globals.append(test_global.clone());
        let global_3 = test_globals.append(test_global.clone());
        let global_4 = test_globals.append(test_global);

        let mut expressions = Arena::new();
        let global_1_expr = expressions.append(Expression::GlobalVariable(global_1));
        let global_2_expr = expressions.append(Expression::GlobalVariable(global_2));
        let global_3_expr = expressions.append(Expression::GlobalVariable(global_3));
        let global_4_expr = expressions.append(Expression::GlobalVariable(global_4));

        let test_body = vec![
            Statement::Return {
                value: Some(global_1_expr),
            },
            Statement::Store {
                pointer: global_2_expr,
                value: global_1_expr,
            },
            Statement::Store {
                pointer: expressions.append(Expression::Access {
                    base: global_3_expr,
                    index: global_4_expr,
                }),
                value: global_1_expr,
            },
        ];

        assert_eq!(
            GlobalUse::scan(&expressions, &test_body, &test_globals),
            vec![
                GlobalUse::LOAD,
                GlobalUse::STORE,
                GlobalUse::STORE,
                GlobalUse::LOAD,
            ]
        )
    }
}
