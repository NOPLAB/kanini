use std::ptr::null;

/// 式ステートメントを表す
#[derive(Debug, PartialEq, Clone)]
pub struct ExprStatement {
    // 左にある式
    left_expr: Expr,
    // 右にある式
    right_expr: Expr,
}

impl ExprStatement {
    /// 生成する
    pub fn new(left_expr: Expr, right_expr: Expr) -> ExprStatement {
        ExprStatement {
            left_expr,
            right_expr,
        }
    }

    pub fn eval(&self) -> ExprStatement {
        ExprStatement {
            left_expr: self.left_expr.clone(),
            right_expr: self.right_expr.clone(),
        }
    }

    // left_exprを取得
    pub fn get_left_expr(&self) -> Expr {
        self.left_expr.clone()
    }

    // right_exprを取得
    pub fn get_right_expr(&self) -> Expr {
        self.right_expr.clone()
    }
}

/// 任意の式を表す
#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    ConstantVal(ConstantVal),
    BinaryOp(Box<BinaryOp>),
    ExprStatement(Box<ExprStatement>),
    Eof(Eof),
}

impl Expr {
    /// 式を評価する
    pub fn eval(&self) -> i32 {
        match self {
            Expr::ExprStatement(e) => 0,
            Expr::ConstantVal(e) => e.eval(),
            Expr::BinaryOp(e) => e.eval(),
            Expr::Eof(e) => 0,
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
