use super::ast::*;
use nom::branch::alt;
use nom::branch::permutation;
use nom::bytes::complete::is_a;
use nom::bytes::complete::tag;
use nom::character::complete::alpha1;
use nom::character::complete::digit1;
use nom::character::complete::multispace0;
use nom::character::streaming::char;
use nom::combinator::map;
use nom::multi::many1;
use nom::permutation;
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
    let parser = tuple((
        many1(declaration_specifier_parser),
        many1(declarator_parser),
        compound_statement_parser,
    ));
    map(
        parser,
        |(declaration_specifier, declarator, compound_statement)| {
            // 単数の式ステートメント
            Expr::FunctionDefinitionParser(
                Box::new(declaration_specifier),
                Box::new(declarator),
                Box::new(compound_statement),
            )
        },
    )(s)
}

/// 宣言指定子のパーサ
/// <declaration-specifier> ::= <storage-class-specifier>
///                          | <type-specifier>
///                          | <type-qualifier>
pub fn declaration_specifier_parser(s: &str) -> IResult<&str, Expr> {
    return type_specifier_parser(s);
}

/// タイプ指定子のパーサ
/// <type-specifier> ::= void
///                    | char
///                    | short
///                    | int
///                    | long
///                    | float
///                    | double
///                    | signed
///                    | unsigned
///                    | <struct-or-union-specifier>
///                    | <enum-specifier>
///                    | <typedef-name>
pub fn type_specifier_parser(s: &str) -> IResult<&str, Expr> {
    let parser = map(
        permutation((
            multispace0,
            alt((
                tag("void"),
                tag("int"),
                tag("char"),
                tag("short"),
                tag("long"),
                tag("float"),
                tag("double"),
                tag("signed"),
                tag("unsigned"),
            )),
            multispace0,
        )),
        |(_, type_str, _)| -> Expr {
            let type_specifier = match type_str {
                "void" => TypeKind::Void,
                "char" => TypeKind::Char,
                "short" => TypeKind::Short,
                "int" => TypeKind::Int,
                "long" => TypeKind::Long,
                "float" => TypeKind::Float,
                "double" => TypeKind::Double,
                "signed" => TypeKind::Signed,
                "unsigned" => TypeKind::Unsigned,
                _ => panic!("error!"),
            };
            return Expr::TypeSpecifier(TypeSpecifier::new(type_specifier));
        },
    )(s);

    parser
}

/// declaratorのパーサ
/// <declarator> ::= {<pointer>}? <direct-declarator>
pub fn declarator_parser(s: &str) -> IResult<&str, Expr> {
    return direct_declarator(s);
}

/// 直接宣言者のパーサ
/// <direct-declarator> ::= <identifier>
///                       | ( <declarator> )
///                       | <direct-declarator> [ {<constant-expression>}? ]
///                       | <direct-declarator> ( <parameter-type-list> )
///                       | <direct-declarator> ( {<identifier>}* )
pub fn direct_declarator(s: &str) -> IResult<&str, Expr> {
    let result = tuple((identifier_parser, opt(char('(')), opt(char(')'))));

    map(result, |(head_expr, _, _)| {
        // 単数の式
        Expr::Identifier(head_expr)
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
    let x = tuple((expr_parser, char(';'), opt(is_a(" "))));

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

/// 識別子のパーサ
pub fn identifier_parser(s: &str) -> IResult<&str, Identifier> {
    let (no_used, _) = opt(is_a(" "))(s)?;
    let (no_used, used) = alpha1(no_used)?;
    let (no_used, _) = opt(is_a(" "))(no_used)?;
    Ok((no_used, Identifier::new(used.to_string())))
}

#[test]
fn constant_val_parser_test() {
    let (_, actual) = constant_val_parser("889").unwrap();
    let expect = ConstantVal::new(889);
    assert_eq!(actual, expect);
}
