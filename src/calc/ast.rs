/// Declarattor
#[derive(Debug, PartialEq, Clone)]
pub struct Declarator {
    //pointer: Pointer,
    direct_declarator: Box<Expr>,
}

impl Declarator {
    pub fn new(val: Expr) -> Declarator {
        Declarator {
            direct_declarator: Box::new(val),
        }
    }
    pub fn dummy() -> Declarator {
        Declarator {
            direct_declarator: Box::new(Expr::Eof(Eof::new())),
        }
    }
    /// Declaratorを評価する
    pub fn eval(&self) -> Declarator {
        self.clone()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct DirectDeclarator {
    identifer: Box<Expr>,
    declarator: Box<Expr>,
    direct_declarator: Box<Expr>,
    parameter_type_list: Option<Box<Expr>>,
}

impl DirectDeclarator {
    /// <direct-declarator> ::= <identifier>
    ///                       | ( <declarator> )
    ///                       | <direct-declarator> [ {<constant-expression>}? ]
    ///                       | <direct-declarator> ( <parameter-type-list> )
    ///                       | <direct-declarator> ( {<identifier>}* )
    pub fn identifier(identifier: Expr) -> DirectDeclarator {
        DirectDeclarator {
            identifer: Box::new(identifier),
            declarator: Box::new(Expr::Eof(Eof::new())),
            direct_declarator: Box::new(Expr::Eof(Eof::new())),
            parameter_type_list: None,
        }
    }

    pub fn declarator(declarator: Expr) -> DirectDeclarator {
        DirectDeclarator {
            declarator: Box::new(declarator),
            identifer: Box::new(Expr::Eof(Eof::new())),
            direct_declarator: Box::new(Expr::Eof(Eof::new())),
            parameter_type_list: None,
        }
    }

    pub fn directdeclarator_identifier(
        directdeclarator: Expr,
        identifier: Expr,
    ) -> DirectDeclarator {
        DirectDeclarator {
            direct_declarator: Box::new(directdeclarator),
            identifer: Box::new(identifier),
            declarator: Box::new(Expr::Eof(Eof::new())),
            parameter_type_list: None,
        }
    }

    pub fn directdeclarator_parametertypelist(
        identifier: Expr,
        parameter_type_list: Expr,
    ) -> DirectDeclarator {
        DirectDeclarator {
            direct_declarator: Box::new(Expr::Eof(Eof::new())),
            identifer: Box::new(identifier),
            declarator: Box::new(Expr::Eof(Eof::new())),
            parameter_type_list: Some(Box::new(parameter_type_list)),
        }
    }
    /// Declaratorを評価する
    pub fn eval(&self) -> DirectDeclarator {
        self.clone()
    }
}

/*
/// DirectDeclarattor
#[derive(Debug, PartialEq, Clone)]
pub struct DirectDeclarattor {
    identifer: Identifier,
}
impl DirectDeclarator {
    pub fn new(identifer: Box<Identifier>, declarator: Box<DirectDeclarattor>) -> DirectDeclarator {
    }
}
*/

/// 複合ステートメントを表す
/// statement := <labeled-statement>
///           | <expression-statement> | <compound-statement>
///           | <selection-statement>
///           | <iteration-statement>
///           | <jump-statement>
#[derive(Debug, PartialEq, Clone)]
pub struct ExprStatement {
    // 式の集合
    expr_stmt: Vec<Expr>,
}

impl ExprStatement {
    /// 生成する
    pub fn new(val: Vec<Expr>) -> ExprStatement {
        ExprStatement { expr_stmt: val }
    }

    pub fn add(mut self, expr: Expr) -> ExprStatement {
        self.expr_stmt.push(expr);
        self
    }

    pub fn eval(&self) -> ExprStatement {
        todo!("stmt eval")
    }
}

/// 引数リストを表す
/// expression-statement ::= {<expression>}? ;
#[derive(Debug, PartialEq, Clone)]
pub struct ParameterTypeList {
    parameter: Vec<Expr>,
}

impl ParameterTypeList {
    pub fn new(val: Vec<Expr>) -> ParameterTypeList {
        ParameterTypeList { parameter: val }
    }
}

/// 型+変数を表す
#[derive(Debug, PartialEq, Clone)]
pub struct ParameterDeclaration {
    declaration_specifier: Vec<Expr>,
    declarator: Declarator,
}

impl ParameterDeclaration {
    pub fn new(ds: Vec<Expr>, d: Declarator) -> ParameterDeclaration {
        ParameterDeclaration {
            declaration_specifier: ds,
            declarator: d,
        }
    }
}

/// 式ステートメントを表す
/// expression-statement ::= {<expression>}? ;
#[derive(Debug, PartialEq, Clone)]
pub struct Statement {
    hoge: String,
    stmt: Expr,
}

impl Statement {
    /// ConstantVal init
    pub fn new(val: Expr) -> Statement {
        Statement {
            hoge: String::from("hoge"),
            stmt: val,
        }
    }
}

/// 関数定義を表す
/// <function-definition> ::= {<declaration-specifier>}* <declarator> {<declaration>}* <compound-statement>
#[derive(Debug, PartialEq, Clone)]
pub struct FunctionDefinitionParser {
    type_specifier: TypeSpecifier,
    declarator: Identifier,
    expr_stmt: ExprStatement,
}

impl FunctionDefinitionParser {
    /// 生成する
    pub fn new(
        type_specifier: TypeSpecifier,
        declarator: Identifier,
        expr_stmt: ExprStatement,
    ) -> FunctionDefinitionParser {
        FunctionDefinitionParser {
            type_specifier,
            declarator,
            expr_stmt,
        }
    }

    /// 関数を評価する
    pub fn eval(&self) -> FunctionDefinitionParser {
        self.clone()
    }
}

/// 任意の式を表す
#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    FunctionDefinitionParser(Vec<Expr>, Box<Expr>, Box<Expr>),
    ParameterTypeList(ParameterTypeList),
    ParameterDeclaration(Vec<Expr>, Declarator),
    DirectDeclarator(DirectDeclarator),
    Declarator(Box<Expr>),
    TypeSpecifier(TypeSpecifier),
    Identifier(Identifier),
    ConstantVal(ConstantVal),
    StringVal(String),
    BinaryOp(Box<BinaryOp>),
    ExprStatement(Vec<Expr>),
    Statement(Box<Expr>),
    PostfixExpression(PostfixExpression),
    Return(Vec<Expr>),
    Eof(Eof),
}

impl Expr {
    /// 式を評価する
    pub fn eval(&self) -> i32 {
        match self {
            Expr::ExprStatement(_) => 0,
            Expr::ConstantVal(e) => e.eval(),
            Expr::BinaryOp(e) => e.eval(),
            _ => 0,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct PostfixExpression {
    assignment_expression: Vec<Expr>,
    factor: Box<Expr>,
}

impl PostfixExpression {
    pub fn new(factor: Expr, ae: Vec<Expr>) -> PostfixExpression {
        PostfixExpression {
            factor: Box::new(factor),
            assignment_expression: ae,
        }
    }

    pub fn assignment_expression(ae: Vec<Expr>) -> PostfixExpression {
        PostfixExpression {
            factor: Box::new(Expr::Eof(Eof::new())),
            assignment_expression: ae,
        }
    }
}

/// return
#[derive(Debug, PartialEq, Clone)]

pub struct Return {
    statement: Statement,
    foo1: i32,
    foo2: i64,
}

impl Return {
    pub fn new(val: Statement, hoge1: i32, hoge2: i64) -> Return {
        Return {
            statement: val,
            foo1: hoge1,
            foo2: hoge2,
        }
    }
}

/// 型名種別
#[derive(Debug, PartialEq, Clone)]
pub enum TypeKind {
    Void,
    Char,
    Short,
    Int,
    Long,
    Float,
    Double,
    Signed,
    Unsigned,
}

/// 型を表す
#[derive(Debug, PartialEq, Clone)]
pub struct TypeSpecifier(TypeKind);

impl TypeSpecifier {
    /// TypeSpecifier init
    pub fn new(val: TypeKind) -> TypeSpecifier {
        TypeSpecifier(val)
    }

    /// TypeSpecifierの値を取得
    pub fn eval(&self) -> TypeKind {
        self.0.clone()
    }
}

#[test]
fn constant_val_test() {
    let expect = 55;
    let constant_val = ConstantVal::new(expect);
    assert_eq!(constant_val.eval(), expect);
}

/// 終端を表す
#[derive(Debug, PartialEq, Clone)]
pub struct Eof;

impl Eof {
    /// ConstantVal init
    pub fn new() -> Eof {
        Eof
    }

    /// ConstantValの値を取得
    pub fn eval(&self) {}
}

/// 演算子種別
#[derive(Debug, PartialEq, Clone)]
pub enum OpKind {
    Add,
    Sub,
    Mul,
    Div,
}

/// 二項演算子を表す
#[derive(Debug, PartialEq, Clone)]
pub struct BinaryOp {
    // 適応する演算子種別
    op_kind: OpKind,
    // 演算子の左にある式
    left_expr: Expr,
    // 演算子の右にある式
    right_expr: Expr,
}

impl BinaryOp {
    /// BinaryOpを生成する
    pub fn new(op_kind: OpKind, left_expr: Expr, right_expr: Expr) -> BinaryOp {
        BinaryOp {
            op_kind,
            left_expr,
            right_expr,
        }
    }

    pub fn eval(&self) -> i32 {
        let right = self.right_expr.eval();
        let left = self.left_expr.eval();
        match self.op_kind {
            OpKind::Add => left + right,
            OpKind::Sub => left - right,
            OpKind::Mul => left * right,
            OpKind::Div => left / right,
        }
    }
}

#[test]
fn plus_op_test() {
    // 13*(5+1)

    let binary_op = BinaryOp::new(
        OpKind::Mul,
        Expr::ConstantVal(ConstantVal::new(13)),
        Expr::BinaryOp(Box::new(BinaryOp::new(
            OpKind::Add,
            Expr::ConstantVal(ConstantVal(5)),
            Expr::ConstantVal(ConstantVal(1)),
        ))),
    );

    let expect = 13 * (5 + 1);

    assert_eq!(binary_op.eval(), expect);
}

/// 定数を表す
#[derive(Debug, PartialEq, Clone)]
pub struct ConstantVal(i32);

impl ConstantVal {
    /// ConstantVal init
    pub fn new(val: i32) -> ConstantVal {
        ConstantVal(val)
    }

    /// ConstantValの値を取得
    pub fn eval(&self) -> i32 {
        self.0
    }
}

/// 文字列を表す
#[derive(Debug, PartialEq, Clone)]
pub struct StringVal(String);

impl StringVal {
    /// StringVal init
    pub fn new(val: String) -> StringVal {
        StringVal(val)
    }

    /// StringValの値を取得
    pub fn eval(&self) -> String {
        self.0.clone()
    }
}

/// 任意トークンを表す
#[derive(Debug, PartialEq, Clone)]
pub struct Identifier(String);

impl Identifier {
    /// ConstantVal init
    pub fn new(val: String) -> Identifier {
        Identifier(val)
    }

    /// Identifierの値を取得
    pub fn eval(&self) -> String {
        self.0.clone()
    }
}
