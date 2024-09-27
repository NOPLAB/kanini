use super::ast::*;
use nom::branch::alt;
use nom::bytes::complete::is_a;
use nom::bytes::complete::tag;
use nom::character::complete::digit1;
use nom::character::streaming::char;
use nom::combinator::map;
use nom::multi::many0;
use nom::multi::many1;
use nom::IResult;

use nom::combinator::opt;
use nom::sequence::tuple;

/// 翻訳ユニットのパーサ
pub fn translation_unit_parser(s: &str) -> IResult<&str, Expr> {
    let parser = many1(external_declaration_parser);

    map(parser, |stmt| {
        // 単数の式ステートメント
        Expr::ExprStatement(stmt)
    })(s)
}

/// 外部宣言のパーサ
pub fn external_declaration_parser(s: &str) -> IResult<&str, Expr> {
    return function_definition_parser(s);
}

/// 関数宣言のパーサ
pub fn function_definition_parser(s: &str) -> IResult<&str, Expr> {
    let parser = tuple((declaration_specifier_parser, compound_statement_parser));
    map(parser, |(declaration_specifier, compound_statement)| {
        // 単数の式ステートメント
        Expr::FunctionDefinitionParser(
            Box::new(declaration_specifier),
            Box::new(compound_statement),
        )
    })(s)
}

///
pub fn declaration_specifier_parser(s: &str) -> IResult<&str, Expr> {
    return type_specifier_parser(s);
}

/// 型名のパーサ
pub fn type_specifier_parser(s: &str) -> IResult<&str, Expr> {
    let parser = map(alt((tag("void"), tag("int"))), |type_str| match type_str {
        "int" => TypeKind::Int,
        _ => panic!("error!"),
    });

    map(parser, |head_expr| {
        Expr::TypeSpecifier(TypeSpecifier::new(head_expr))
    })(s)
}

/// compound-statementのパーサ
pub fn compound_statement_parser(s: &str) -> IResult<&str, Expr> {
    let (no_used, _) = opt(is_a(" "))(s)?;
    let (no_used, _) = char('{')(no_used)?;
    let (no_used, expr) = expr_statement_parser(no_used)?;
    let (no_used, _) = char('}')(no_used)?;
    let (no_used, _) = opt(is_a(" "))(no_used)?;

    Ok((no_used, expr))
}

/// 複合式のパーサ
pub fn expr_statement_parser(s: &str) -> IResult<&str, Expr> {
    let parser = many1(statement_parser);

    map(parser, |stmt| {
        // 単数の式ステートメント
        Expr::ExprStatement(stmt)
    })(s)
}

pub fn statement_parser(s: &str) -> IResult<&str, Expr> {
    let (x) = tuple((expr_parser, char(';'), opt(is_a(" "))));

    map(x, |(head_expr, _, _)| {
        // 単数の式
        Expr::Statement(Box::new(head_expr))
    })(s)
}

/// 式のパーサ
pub fn expr_parser(s: &str) -> IResult<&str, Expr> {
    let op_kind_parser = map(alt((char('+'), char('-'))), |op_char| match op_char {
        '+' => OpKind::Add,
        '-' => OpKind::Sub,
        _ => panic!("error!"),
    });

    let binary_parser = tuple((term_parser, opt(tuple((op_kind_parser, expr_parser)))));

    map(binary_parser, |(head_expr, tail_expr_opt)| {
        if let Option::Some((op_kind, tail_expr)) = tail_expr_opt {
            Expr::BinaryOp(Box::new(BinaryOp::new(op_kind, head_expr, tail_expr)))
        } else {
            head_expr
        }
    })(s)
}

/// 項のパーサ
pub fn term_parser(s: &str) -> IResult<&str, Expr> {
    //*/ 記号をOpKind型にパースするパーサ
    let op_kind_parser = map(alt((char('*'), char('/'))), |op_char| match op_char {
        '*' => OpKind::Mul,
        '/' => OpKind::Div,
        _ => panic!("error"),
    });

    // 掛け算、割り算のパーサ
    let binary_parser = tuple((factor_parser, opt(tuple((op_kind_parser, term_parser)))));

    // 掛け算、割り算のパーサのパースされた値をMapで調整
    map(binary_parser, |(head_expr, tail_expr_opt)| {
        if let Option::Some((op_kind, tail_expr)) = tail_expr_opt {
            Expr::BinaryOp(Box::new(BinaryOp::new(op_kind, head_expr, tail_expr)))
        } else {
            head_expr
        }
    })(s)
}
/// 因子のパーサ
pub fn factor_parser(s: &str) -> IResult<&str, Expr> {
    alt((
        map(constant_val_parser, |constant_val| {
            Expr::ConstantVal(constant_val)
        }),
        paren_expr_parser,
    ))(s)
}

#[test]
fn factor_parser_test() {
    let (_, actual) = factor_parser("4").unwrap();
    let expect = Expr::ConstantVal(ConstantVal::new(4));
    assert_eq!(actual, expect);

    let (_, actual) = factor_parser("(3)").unwrap();
    let expect = Expr::ConstantVal(ConstantVal::new(3));
    assert_eq!(actual, expect);
}

/// 丸括弧で囲まれた式のパーサ
pub fn paren_expr_parser(s: &str) -> IResult<&str, Expr> {
    let (no_used, _) = char('(')(s)?;
    let (no_used, expr) = expr_parser(no_used)?;
    let (no_used, _) = char(')')(no_used)?;
    Ok((no_used, expr))
}

/// 定数のパーサ
pub fn constant_val_parser(s: &str) -> IResult<&str, ConstantVal> {
    use std::str::FromStr;

    let (no_used, _) = opt(is_a(" "))(s)?;
    let (no_used, used) = digit1(no_used)?;
    let (no_used, _) = opt(is_a(" "))(no_used)?;
    let val = FromStr::from_str(used).unwrap();
    Ok((no_used, ConstantVal::new(val)))
}

#[test]
fn constant_val_parser_test() {
    let (_, actual) = constant_val_parser("889").unwrap();
    let expect = ConstantVal::new(889);
    assert_eq!(actual, expect);
}
