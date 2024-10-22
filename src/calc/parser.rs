use std::ops::Mul;

use super::ast::*;
use nom::branch::alt;
use nom::branch::permutation;
use nom::bytes::complete::is_a;
use nom::bytes::complete::tag;
use nom::character::complete::alpha1;
use nom::character::complete::char;
use nom::character::complete::digit1;
use nom::character::complete::multispace0;
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
    let result = function_definition_parser(s);
    return result;
}

/// 関数宣言のパーサ
/// <function-definition> ::= {<declaration-specifier>}* <declarator> {<declaration>}* <compound-statement>
pub fn function_definition_parser(s: &str) -> IResult<&str, Expr> {
    let parser = tuple((
        //opt(
        many0(declaration_specifier_parser), //)
        declarator_parser,
        compound_statement_parser,
    ));
    map(
        parser,
        |(declaration_specifier, declarator, compound_statement)| {
            // 単数の式ステートメント
            //    if let Option(ds) = declaration_specifier {

            Expr::FunctionDefinitionParser(
                declaration_specifier,
                Box::new(declarator),
                Box::new(compound_statement),
            )
            /*
            } else {
                Expr::FunctionDefinitionParser(Box::new(declarator), Box::new(compound_statement))
            }*/
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
///                       | <direct-declarator> ( <parameter-type-list> )
///                       | <direct-declarator> [ {<constant-expression>}? ]
///                       | <direct-declarator> ( {<identifier>}* )
pub fn direct_declarator(s: &str) -> IResult<&str, Expr> {
    let directdeclarator_declarator = tuple((tag("("), declarator_parser, tag(")")));

    let directdeclarator_parametertypelist =
        tuple((identifier_parser, char('('), parameter_type_list, char(')')));

    let identifer = identifier_parser;

    alt((
        map(directdeclarator_parametertypelist, |dp| {
            Expr::DirectDeclarator(DirectDeclarator::directdeclarator_parametertypelist(
                dp.0, dp.2,
            ))
        }),
        map(directdeclarator_declarator, |dd| dd.1),
        map(identifer, |i| i),
    ))(s)
}

/// <parameter-type-list> ::= <parameter-list>
///                         | <parameter-list> , ...

pub fn parameter_type_list(s: &str) -> IResult<&str, Expr> {
    let result = permutation((multispace0, many0(parameter_list), multispace0));

    map(result, |(_, param_list, _)| {
        Expr::ParameterTypeList(ParameterTypeList::new(param_list))
    })(s)
}

/// <parameter-list> ::= <parameter-declaration>
///                    | <parameter-list> , <parameter-declaration>
pub fn parameter_list(s: &str) -> IResult<&str, Expr> {
    let result = tuple((parameter_declaration, opt(char(','))));
    map(result, |(head_expr, _)| head_expr)(s)
}

/// <parameter-declaration> ::= {<declaration-specifier>}+ <declarator>
///                           | {<declaration-specifier>}+ <abstract-declarator>
///                           | {<declaration-specifier>}+
pub fn parameter_declaration(s: &str) -> IResult<&str, Expr> {
    let parser = tuple((many1(declaration_specifier_parser), opt(declarator_parser)));

    map(parser, |(dsp, dp_apt)| {
        if let Option::Some(dp) = dp_apt {
            Expr::ParameterDeclaration(dsp, Declarator::new(dp))
        } else {
            Expr::ParameterDeclaration(dsp, Declarator::dummy())
        }
    })(s)
    /*
        let parser = tuple((many1(declaration_specifier_parser), declarator_parser));

    */
}

/// compound-statementのパーサ
/// <statement> ::= <labeled-statement>
///               | <expression-statement>
///               | <compound-statement>
///               | <selection-statement>
///               | <iteration-statement>
///               | <jump-statement>
pub fn compound_statement_parser(s: &str) -> IResult<&str, Expr> {
    let (no_used, _) = permutation((multispace0, char('{'), multispace0))(s)?;

    let expr_statement = expr_statement_parser;
    let selection_statement = selection_statement_parser;
    let jump_statement = jump_statement_parser;

    let (no_used, expr) = alt((
        map(expr_statement, |es| {
            println!("{:?}", es);
            return es;
        }),
        map(jump_statement, |js| {
            println!("{:?}", js);
            return js;
        }),
        map(selection_statement, |ss| {
            println!("{:?}", ss);
            return ss;
        }),
    ))(no_used)?;

    let (no_used, _) = permutation((multispace0, char('}'), multispace0))(no_used)?;

    Ok((no_used, expr))
}

pub fn selection_statement_parser(s: &str) -> IResult<&str, Expr> {
    let parser = many1(statement_parser);

    map(parser, |stmt| {
        // 単数の式ステートメント
        Expr::ExprStatement(stmt)
    })(s)
}

/// <jump-statement> ::= goto <identifier> ;
///                    | continue ;
///                    | break ;
///                    | return {<expression>}? ;

pub fn jump_statement_parser(s: &str) -> IResult<&str, Expr> {
    let return_parser = tuple((
        tag("return"),
        multispace0,
        many0(expression_parser),
        char(';'),
        multispace0,
    ));

    map(return_parser, |stmt| {
        // 単数の式ステートメント
        Expr::Return(stmt.2)
    })(s)
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
    let x = tuple((expression_parser, char(';'), multispace0));

    map(x, |(head_expr, _, _)| {
        // 単数の式
        Expr::Statement(Box::new(head_expr))
    })(s)
}

pub fn expression_parser(s: &str) -> IResult<&str, Expr> {
    let a = tuple((additive_expression_parser, char(',')));
    let parser_with_comma = many1(a);
    let parser = additive_expression_parser;

    alt((
        map(parser_with_comma, |head_expr| {
            let mut exprs: Vec<Expr> = Vec::new();
            for (expr, _) in head_expr {
                // charの部分は無視
                exprs.push(expr);
            }
            Expr::ExprStatement(exprs)
        }),
        map(parser, |expr_s| Expr::Statement(Box::new(expr_s))),
    ))(s)
}

/// 式のパーサ
pub fn additive_expression_parser(s: &str) -> IResult<&str, Expr> {
    let op_kind_parser = map(alt((char('+'), char('-'))), |op_char| match op_char {
        '+' => OpKind::Add,
        '-' => OpKind::Sub,
        _ => panic!("error!"),
    });

    let binary_parser = tuple((
        term_parser,
        opt(tuple((op_kind_parser, additive_expression_parser))),
    ));

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
    let binary_parser = tuple((
        postfix_expression_parser,
        opt(tuple((op_kind_parser, term_parser))),
    ));

    // 掛け算、割り算のパーサのパースされた値をMapで調整
    map(binary_parser, |(head_expr, tail_expr_opt)| {
        if let Option::Some((op_kind, tail_expr)) = tail_expr_opt {
            Expr::BinaryOp(Box::new(BinaryOp::new(op_kind, head_expr, tail_expr)))
        } else {
            head_expr
        }
    })(s)
}

///
/// <postfix-expression> ::= <primary-expression>
///                        | <postfix-expression> [ <expression> ]
///                        | <postfix-expression> ( {<assignment-expression>}* )
///                        | <postfix-expression> . <identifier>
///                        | <postfix-expression> -> <identifier>
///                        | <postfix-expression> ++
///                        | <postfix-expression> --
pub fn postfix_expression_parser(s: &str) -> IResult<&str, Expr> {
    //((factor_parser, postfix_expression_expression_parser))(s)?;
    let i_a = tuple((
        factor_parser,
        many1(postfix_expression_assignment_expression_parser),
    ));
    alt((
        map(i_a, |(i, assignment)| {
            Expr::PostfixExpression(PostfixExpression::new(i, assignment))
        }),
        map(factor_parser, |i| i), //      postfix_expression_expression_parser,
    ))(s)
}

pub fn postfix_expression_assignment_expression_parser(s: &str) -> IResult<&str, Expr> {
    let (no_used, _) = char('(')(s)?;
    let (no_used, expr) = many0(expression_parser)(no_used)?;
    let (no_used, _) = char(')')(no_used)?;
    Ok((
        no_used,
        Expr::PostfixExpression(PostfixExpression::assignment_expression(expr)),
    ))
}

/*
pub fn postfix_expression_expression_parser(s: &str) -> IResult<&str, Expr> {
    //let (un_used, used) = postfix_expression_parser(s)?;
    let (un_used, used) = char('[')(s)?;
    let (un_used, _) = char(']')(un_used)?;

    Ok((un_used, used))
}*/

/// 因子のパーサ
/// primary-expression
pub fn factor_parser(s: &str) -> IResult<&str, Expr> {
    alt((
        identifier_parser,
        map(constant_val_parser, |constant_val| {
            Expr::ConstantVal(constant_val)
        }),
        paren_additive_parser,
    ))(s)
}

/// 丸括弧で囲まれた式のパーサ
pub fn paren_additive_parser(s: &str) -> IResult<&str, Expr> {
    let (no_used, _) = char('(')(s)?;
    let (no_used, expr) = expression_parser(no_used)?;
    let (no_used, _) = char(')')(no_used)?;
    Ok((no_used, expr))
}

/// 定数のパーサ
pub fn constant_val_parser(s: &str) -> IResult<&str, ConstantVal> {
    use std::str::FromStr;
    let (no_used, used) = permutation((multispace0, digit1, multispace0))(s)?;
    let val = FromStr::from_str(used.1).unwrap();
    Ok((no_used, ConstantVal::new(val)))
}

/// 識別子のパーサ
pub fn identifier_parser(s: &str) -> IResult<&str, Expr> {
    // アンダーバーかアルファベットのいずれか

    let (no_used, used) = permutation((multispace0, alpha1, multispace0))(s)?;

    Ok((
        no_used,
        Expr::Identifier(Identifier::new(used.1.to_string())),
    ))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_additive_expression_parser() {
        let input = "13-5+1";
        let (rest, result) = additive_expression_parser(input).unwrap();
        let r = Expr::BinaryOp(Box::new(BinaryOp::new(
            OpKind::Sub,
            Expr::ConstantVal(ConstantVal::new(13)),
            Expr::BinaryOp(Box::new(BinaryOp::new(
                OpKind::Add,
                Expr::ConstantVal(ConstantVal::new(5)),
                Expr::ConstantVal(ConstantVal::new(1)),
            ))),
        )));
        assert_eq!(rest, "");
        assert_eq!(result, r);
    }
    #[test]
    fn test_postfix_expression_parser_success() {
        // 正常系: 正しい後置式が解析できる
        let input = "a(j)";
        let (rest, result) = postfix_expression_parser(input).unwrap();
        assert_eq!(rest, "");
        assert_eq!(
            result,
            Expr::PostfixExpression(PostfixExpression::new(
                Expr::Identifier(Identifier::new(String::from("a"))),
                vec![Expr::PostfixExpression(PostfixExpression::new(
                    Expr::Eof(Eof::new()),
                    vec![Expr::Statement(Box::new(Expr::Identifier(
                        Identifier::new(String::from("j"))
                    )))],
                ))]
            ))
        );

        let input = "a(j, k, l)";
        let (rest, result) = postfix_expression_parser(input).unwrap();
        assert_eq!(rest, "");
        assert_eq!(
            result,
            Expr::PostfixExpression(PostfixExpression::new(
                Expr::Identifier(Identifier::new(String::from("a"))),
                vec![Expr::PostfixExpression(PostfixExpression::new(
                    Expr::Eof(Eof::new()),
                    vec![
                        Expr::ExprStatement(vec![
                            Expr::Identifier(Identifier::new(String::from("j"))),
                            Expr::Identifier(Identifier::new(String::from("k"))),
                        ]),
                        Expr::Statement(Box::new(Expr::Identifier(Identifier::new(String::from(
                            "l"
                        ))))),
                    ],
                ))]
            ))
        );
    }

    #[test]
    fn test_factor_parser() {
        let (_, actual) = factor_parser("4").unwrap();
        let expect = Expr::ConstantVal(ConstantVal::new(4));
        assert_eq!(actual, expect);

        let (_, actual) = factor_parser("(3)").unwrap();
        let expect = Expr::Statement(Box::new(Expr::ConstantVal(ConstantVal::new(3))));
        assert_eq!(actual, expect);
    }

    #[test]
    fn test_constant_val_parser() {
        let (_, actual) = constant_val_parser("889").unwrap();
        let expect = ConstantVal::new(889);
        assert_eq!(actual, expect);
    }
}
