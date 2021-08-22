mod ast;
mod eval;

fn get_expr(e: ast::EExpr) -> ast::Expr {
    ast::Expr{ expr: e, location: 10 }
}

fn main() {
    let lhs = ast::EExpr::NumberExpr(10);
    let rhs = ast::EExpr::NumberExpr(20);
    let e = ast::EExpr::BinOpExpr {
        lhs: Box::new(get_expr(lhs)),
        rhs: Box::new(get_expr(rhs)),
        op: ast::BinOp::Plus,
    };

    let eval = eval::get_eval();
    println!("{:?}", eval(get_expr(e)));
}
