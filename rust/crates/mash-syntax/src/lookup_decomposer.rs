//! Split simple lookup and member expressions (`xs[10]`, `obj['foo']`, `foo.bar`) into their target
//! expression text and the index or name. Port of `LookupDecomposer`; used by the object browser.

use crate::ast::{AstNode, Expr, LiteralValue};
use crate::{Mode, parse};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LookupWithIntegerIndex {
    pub target: String,
    pub index: i64,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LookupWithStringIndex {
    pub target: String,
    pub index: String,
}

/// `xs[10]` → (`xs`, 10)
pub fn decompose_lookup_with_integer_index(expr: &str) -> Option<LookupWithIntegerIndex> {
    let Expr::Lookup(lookup) = parse_valid(expr)? else {
        return None;
    };
    let Expr::Literal(literal) = lookup.index()? else {
        return None;
    };
    let token = literal.token()?;
    if token.kind() != crate::SyntaxKind::NUMBER_LITERAL {
        return None;
    }
    let index = token.text().parse().ok()?;
    Some(LookupWithIntegerIndex {
        target: text_of(expr, &lookup.target()?),
        index,
    })
}

/// `obj['foo']` → (`obj`, `foo`)
pub fn decompose_lookup_with_string_index(expr: &str) -> Option<LookupWithStringIndex> {
    let Expr::Lookup(lookup) = parse_valid(expr)? else {
        return None;
    };
    let Expr::Literal(literal) = lookup.index()? else {
        return None;
    };
    let LiteralValue::String { value, .. } = literal.value()? else {
        return None;
    };
    Some(LookupWithStringIndex {
        target: text_of(expr, &lookup.target()?),
        index: value,
    })
}

/// `foo.bar` → (`foo`, `bar`)
pub fn decompose_member(expr: &str) -> Option<LookupWithStringIndex> {
    let Expr::Member(member) = parse_valid(expr)? else {
        return None;
    };
    let name = member.name_token()?.text().to_string();
    Some(LookupWithStringIndex {
        target: text_of(expr, &member.target()?),
        index: name,
    })
}

fn parse_valid(expr: &str) -> Option<Expr> {
    let p = parse(expr, Mode::Mash);
    if !p.ok() {
        return None;
    }
    let mut exprs = p.source_file().exprs().into_iter();
    let first = exprs.next()?;
    exprs.next().is_none().then_some(first)
}

fn text_of(source: &str, expr: &Expr) -> String {
    source[expr.syntax().text_range()].to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

    // From LookupDecomposerTest.scala

    fn int(target: &str, index: i64) -> Option<LookupWithIntegerIndex> {
        Some(LookupWithIntegerIndex {
            target: target.to_string(),
            index,
        })
    }

    fn string(target: &str, index: &str) -> Option<LookupWithStringIndex> {
        Some(LookupWithStringIndex {
            target: target.to_string(),
            index: index.to_string(),
        })
    }

    #[test]
    fn lookups_with_integer_indices() {
        assert_eq!(decompose_lookup_with_integer_index("xs[10]"), int("xs", 10));
        assert_eq!(decompose_lookup_with_integer_index("xs[0]"), int("xs", 0));
        assert_eq!(decompose_lookup_with_integer_index("xs[1]"), int("xs", 1));
        assert_eq!(decompose_lookup_with_integer_index("foo.bar[10]"), int("foo.bar", 10));
        assert_eq!(decompose_lookup_with_integer_index("xs.foo"), None);
        assert_eq!(decompose_lookup_with_integer_index("xs[1.2]"), None);
    }

    #[test]
    fn lookups_with_string_indices() {
        assert_eq!(decompose_lookup_with_string_index("obj['foo']"), string("obj", "foo"));
        assert_eq!(
            decompose_lookup_with_string_index("obj['foo`nbar']"),
            string("obj", "foo\nbar")
        );
        assert_eq!(decompose_lookup_with_string_index("obj[\"foo\"]"), string("obj", "foo"));
        assert_eq!(decompose_lookup_with_string_index("obj[42]"), None);
        assert_eq!(decompose_lookup_with_string_index("obj.foo"), None);
    }

    #[test]
    fn members() {
        assert_eq!(decompose_member("foo.bar"), string("foo", "bar"));
        assert_eq!(decompose_member("(a | b).c"), string("(a | b)", "c"));
        assert_eq!(decompose_member("foo[0]"), None);
        assert_eq!(decompose_member("obj.class"), None);
        assert_eq!(decompose_member("obj['foo']"), None);
        assert_eq!(decompose_member("obj[42]"), None);
    }
}
