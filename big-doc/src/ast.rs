
#[derive(Debug)]
pub struct Expr {
    pub location: isize,
    pub expr: EExpr,
}

#[derive(Debug)]
pub enum EExpr {
    BoolExpr(bool),
    NumberExpr(i64),
    StringExpr(String),
    TupleExpr(Vec<Box<Expr>>),
    FunctionExpr { arg: String, body: Box<Expr> },
    IfExpr { cond: Box<Expr>, yes: Box<Expr>, no: Box<Expr> },
    BinOpExpr { lhs: Box<Expr>, op: BinOp, rhs: Box<Expr> },
    UnaryOpExpr { op: UnaryOp, expr: Box<Expr> },
    LetExpr { name: String, value: Box<Expr>, body: Box<Expr> },
}

#[derive(Debug)]
pub enum BinOp {
    Plus, Minus, Times, Divide, And, Or, Xor,
}

#[derive(Debug)]
pub enum UnaryOp {
    Negative, Not,
}
