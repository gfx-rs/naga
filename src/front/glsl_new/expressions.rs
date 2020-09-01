use super::ast::*;
use crate::{BinaryOperator, Expression};

pub fn binary_expr(
    program: &mut Program,
    op: BinaryOperator,
    left: ExpressionRule,
    right: ExpressionRule,
) -> ExpressionRule {
    ExpressionRule::from_expression(program.context.expressions.append(Expression::Binary {
        op,
        left: left.expression,
        right: right.expression,
    }))
}
