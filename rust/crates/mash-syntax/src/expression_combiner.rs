//! Combine a Mash expression with a suffix (e.g. `.reverse`, ` | f`, ` * 3`), adding parentheses around the
//! expression only where needed for the combination to mean what it looks like. Port of
//! `ExpressionCombiner`; used by the object browser to build expressions for drilled-into values.

use crate::ast::{Expr, InvocationExpr};
use crate::{Mode, SyntaxKind, SyntaxKind::*, parse};

/// For example `combine_safely("1 + 2", " * 3")` is `"(1 + 2) * 3"`, but `combine_safely("1 * 2", " + 3")`
/// is `"1 * 2 + 3"`.
pub fn combine_safely(prefix: &str, suffix: &str) -> String {
    if needs_parens(prefix, suffix) {
        format!("({prefix}){suffix}")
    } else {
        format!("{prefix}{suffix}")
    }
}

fn needs_parens(prefix: &str, suffix: &str) -> bool {
    let (Some(prefix_expr), Some(suffix_expr)) = (body(prefix), body(&format!("it{suffix}"))) else {
        return true;
    };
    match (&prefix_expr, &suffix_expr) {
        (Expr::BinOp(prefix_op), Expr::BinOp(suffix_op)) => {
            let kind = |op: Option<crate::SyntaxToken>| op.map(|t| t.kind());
            needs_parens_bin_op(kind(prefix_op.op()), kind(suffix_op.op()))
        }
        (p, Expr::Invocation(invocation)) if is_self_contained(p) && invokes_member(invocation) => false,
        (_, Expr::Invocation(_)) => true,
        (p, _) if is_self_contained(p) => false,
        (Expr::Lambda(_), Expr::Pipe(_)) => true,
        (_, Expr::Pipe(_)) => false,
        _ => true,
    }
}

fn body(s: &str) -> Option<Expr> {
    parse(s, Mode::Mash).source_file().exprs().into_iter().next()
}

/// Expressions that bind tighter than anything that could follow them.
fn is_self_contained(expr: &Expr) -> bool {
    matches!(
        expr,
        Expr::Paren(_)
            | Expr::Member(_)
            | Expr::ParenInvocation(_)
            | Expr::Lookup(_)
            | Expr::Block(_)
            | Expr::List(_)
            | Expr::Object(_)
            | Expr::Identifier(_)
            | Expr::Literal(_)
    )
}

fn invokes_member(invocation: &InvocationExpr) -> bool {
    matches!(invocation.function(), Some(Expr::Member(_)))
}

fn needs_parens_bin_op(prefix_op: Option<SyntaxKind>, suffix_op: Option<SyntaxKind>) -> bool {
    let additive = |k: Option<SyntaxKind>| matches!(k, Some(PLUS | MINUS));
    let multiplicative = |k: Option<SyntaxKind>| matches!(k, Some(TIMES | DIVIDE));
    !(multiplicative(prefix_op) && additive(suffix_op)
        || multiplicative(prefix_op) && multiplicative(suffix_op)
        || additive(prefix_op) && additive(suffix_op))
}

#[cfg(test)]
mod tests {
    use super::combine_safely;

    // From ExpressionCombinerTest.scala

    #[test]
    fn adds_parens_when_required() {
        assert_eq!(combine_safely("f", " a"), "(f) a");
        assert_eq!(combine_safely("f a", " b"), "(f a) b");
        assert_eq!(combine_safely("1 + 2", ".negate"), "(1 + 2).negate");
        assert_eq!(combine_safely("a => b", " | f"), "(a => b) | f");
        assert_eq!(combine_safely("1 + 2", " * 3"), "(1 + 2) * 3");
        assert_eq!(
            combine_safely("help List", ".helpForMethod 'any'"),
            "(help List).helpForMethod 'any'"
        );
    }

    #[test]
    fn does_not_add_parens_if_not_required() {
        assert_eq!(combine_safely("1", " + 2"), "1 + 2");
        assert_eq!(combine_safely("items", ".reverse"), "items.reverse");
        assert_eq!(
            combine_safely("items", ".sortBy (.lastModified)"),
            "items.sortBy (.lastModified)"
        );
        assert_eq!(combine_safely("items", "[0]"), "items[0]");
        assert_eq!(combine_safely("a | f", " | g"), "a | f | g");
        assert_eq!(combine_safely("a | b => f b", " | g"), "a | b => f b | g");
        assert_eq!(combine_safely("42", " | f"), "42 | f");
        assert_eq!(combine_safely("1 + 2", " | f"), "1 + 2 | f");
        assert_eq!(combine_safely("1 * 2", " + 3"), "1 * 2 + 3");
        assert_eq!(combine_safely("1 - 2", " - 3"), "1 - 2 - 3");
        assert_eq!(combine_safely("r4.sortBy(.x)", ".reverse"), "r4.sortBy(.x).reverse");
    }
}
