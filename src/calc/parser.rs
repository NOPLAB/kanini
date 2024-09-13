use nom::character::complete::digit1;
use nom::IResult;

use super::ast::*;
use nom::branch::alt;
use nom::combinator::map;
use nom::combinator::opt;
use nom::sequence::tuple;
use nom::character::complete::char;

/// 複合式のパーサ
pub fn expr_statement_parser(s: &str) -> IResult<&str, Expr> {
    let x = tuple((
        expr_parser,
        char(';'),
        opt(char(' ')),
        opt(expr_statement_parser),
    ));

    map(x, |(head_expr, _, _, tail_expr_opt)| {
        if let Option::Some(tail_expr) = tail_expr_opt {
            Expr::ExprStatement(Box::new(ExprStatement::new(head_expr, tail_expr)))
        } else {
            head_expr
        }
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
    let x = alt((
        map(constant_val_parser, |constant_val| {
            Expr::ConstantVal(constant_val)
        }),
        paren_expr_parser,
    ))(s);

    println!("因子: {:?}", x);

    x
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
    let (no_used, used) = digit1(s)?;
    let val = FromStr::from_str(used).unwrap();
    Ok((no_used, ConstantVal::new(val)))
}

#[test]
fn constant_val_parser_test() {
    let (_, actual) = constant_val_parser("889").unwrap();
    let expect = ConstantVal::new(889);
    assert_eq!(actual, expect);
}
