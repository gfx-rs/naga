use crate::arena::{Arena, Handle};

pub struct Interface<'a, T> {
    expressions: &'a Arena<crate::Expression>,
    visitor: &'a mut T,
}

pub trait Visitor {
    fn expr_visitor(&mut self, _: &crate::Expression) {}
    fn sta_visitor(&mut self, _: &crate::Statement, _: &Arena<crate::Expression>) {}
}

impl<'a, T> Interface<'a, T>
where
    T: Visitor,
{
    fn add_inputs(&mut self, handle: Handle<crate::Expression>) {
        use crate::Expression as E;
        match self.expressions[handle] {
            E::Access { base, index } => {
                self.add_inputs(base);
                self.add_inputs(index);
            }
            E::AccessIndex { base, .. } => {
                self.add_inputs(base);
            }
            E::Constant(_) => {}
            E::Compose { ref components, .. } => {
                for &comp in components {
                    self.add_inputs(comp);
                }
            }
            E::FunctionParameter(_) | E::GlobalVariable(_) | E::LocalVariable(_) => {}
            E::Load { pointer } => {
                self.add_inputs(pointer);
            }
            E::ImageSample {
                image,
                sampler,
                coordinate,
                level,
                depth_ref,
            } => {
                self.add_inputs(image);
                self.add_inputs(sampler);
                self.add_inputs(coordinate);
                match level {
                    crate::SampleLevel::Auto => (),
                    crate::SampleLevel::Exact(h) | crate::SampleLevel::Bias(h) => {
                        self.add_inputs(h)
                    }
                }
                if let Some(dref) = depth_ref {
                    self.add_inputs(dref);
                }
            }
            E::ImageLoad {
                image,
                coordinate,
                index,
            } => {
                self.add_inputs(image);
                self.add_inputs(coordinate);
                self.add_inputs(index);
            }
            E::Unary { expr, .. } => {
                self.add_inputs(expr);
            }
            E::Binary { left, right, .. } => {
                self.add_inputs(left);
                self.add_inputs(right);
            }
            E::Intrinsic { argument, .. } => {
                self.add_inputs(argument);
            }
            E::Transpose(matrix) => {
                self.add_inputs(matrix);
            }
            E::DotProduct(left, right) => {
                self.add_inputs(left);
                self.add_inputs(right);
            }
            E::CrossProduct(left, right) => {
                self.add_inputs(left);
                self.add_inputs(right);
            }
            E::As(expr, _) => {
                self.add_inputs(expr);
            }
            E::Derivative { expr, .. } => {
                self.add_inputs(expr);
            }
            E::Call { ref arguments, .. } => {
                for &argument in arguments {
                    self.add_inputs(argument);
                }
            }
        }
        self.visitor.expr_visitor(&self.expressions[handle])
    }

    pub fn collect(&mut self, block: &[crate::Statement]) {
        for statement in block {
            use crate::Statement as S;
            match *statement {
                S::Empty | S::Break | S::Continue | S::Kill => (),
                S::Block(ref b) => {
                    self.collect(b);
                }
                S::If {
                    condition,
                    ref accept,
                    ref reject,
                } => {
                    self.add_inputs(condition);
                    self.collect(accept);
                    self.collect(reject);
                }
                S::Switch {
                    selector,
                    ref cases,
                    ref default,
                } => {
                    self.add_inputs(selector);
                    for &(ref case, _) in cases.values() {
                        self.collect(case);
                    }
                    self.collect(default);
                }
                S::Loop {
                    ref body,
                    ref continuing,
                } => {
                    self.collect(body);
                    self.collect(continuing);
                }
                S::Return { value } => {
                    if let Some(expr) = value {
                        self.add_inputs(expr);
                    }
                }
                S::Store { pointer, value } => {
                    let mut left = pointer;
                    loop {
                        match self.expressions[left] {
                            crate::Expression::Access { base, index } => {
                                self.add_inputs(index);
                                left = base;
                            }
                            crate::Expression::AccessIndex { base, .. } => {
                                left = base;
                            }
                            _ => break,
                        }
                    }
                    self.add_inputs(value);
                }
            }
            self.visitor.sta_visitor(statement, &self.expressions)
        }
    }
}

struct GlobalUseVisitor(Vec<crate::GlobalUse>);

impl Visitor for GlobalUseVisitor {
    fn expr_visitor(&mut self, expr: &crate::Expression) {
        if let crate::Expression::GlobalVariable(handle) = expr {
            self.0[handle.index()] |= crate::GlobalUse::LOAD;
        }
    }

    fn sta_visitor(&mut self, sta: &crate::Statement, expressions: &Arena<crate::Expression>) {
        if let crate::Statement::Store { pointer, .. } = sta {
            let mut left = *pointer;
            loop {
                match expressions[left] {
                    crate::Expression::Access { base, .. } => {
                        left = base;
                    }
                    crate::Expression::AccessIndex { base, .. } => {
                        left = base;
                    }
                    crate::Expression::GlobalVariable(handle) => {
                        self.0[handle.index()] |= crate::GlobalUse::STORE;
                        break;
                    }
                    _ => break,
                }
            }
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
        io.collect(body);
        visitor.0
    }
}
