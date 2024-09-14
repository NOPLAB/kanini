pub mod ast;
pub mod codegen;
pub mod parser;

use crate::calc::ast::Expr;
use nom::error::ErrorKind;
use nom::Err;

/// 受け取った文字列をパースして評価する
pub fn expr_eval(s: &str) -> Result<&str, Err<(&str, ErrorKind)>> {
    codegen::generator(parser::expr_statement_parser(s).map(|(_, expr)| expr))
}

/*
#[test]
fn expr_eval_test() {
    assert_eq!(expr_eval("1+2*3-7").unwrap(), 1 + 2 * 3 - 7);
    assert_eq!(expr_eval("1+2+3+4+5").unwrap(), 1 + 2 + 3 + 4 + 5);
    assert_eq!(
        expr_eval("(2+55)/(5-4*42)").unwrap(),
        (2 + 55) / (5 - 4 * 42)
    );
}*/
