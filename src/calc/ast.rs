use std::ptr::null;
/// 複合ステートメントを表す
/// BNF =>
/// statement := <labeled-statement>
///           | <expression-statement>
///           | <compound-statement>
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

/// 式ステートメントを表す
/// BNF => expression-statement ::= {<expression>}? ;
#[derive(Debug, PartialEq, Clone)]
pub struct Statement {
    stmt: Expr,
}

impl Statement {
    /// ConstantVal init
    pub fn new(val: Expr) -> Statement {
        Statement { stmt: val }
    }
}

/// 任意の式を表す
#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    ConstantVal(ConstantVal),
    BinaryOp(Box<BinaryOp>),
    ExprStatement(Vec<Expr>),
    Statement(Box<Expr>),
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
