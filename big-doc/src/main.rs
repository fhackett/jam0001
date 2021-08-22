mod ast;
mod eval;
use im_rc::hashmap;


fn get_expr(e: ast::EExpr) -> ast::Expr {
    ast::Expr{ expr: e, location: 10 }
}

fn main() {
    let lhs = ast::EExpr::IdentifierExpr("x".to_string());
    let rhs = ast::EExpr::NumberExpr(1);
    let e1 = ast::EExpr::BinOpExpr {
        lhs: Box::new(get_expr(lhs)),
        rhs: Box::new(get_expr(rhs)),
        op: ast::BinOp::Plus,
    };

    let e2 = ast::EExpr::LetExpr {
        name: "x".to_string(),
        value: Box::new(get_expr(ast::EExpr::NumberExpr(2))),
        body: Box::new(get_expr(e1))
    };

    println!("{:?}", eval::eval(get_expr(e2), &hashmap!{}));
}
