mod ast;

fn main() {
    let expr = ast::EExpr::StringExpr(String::from("foo"));

    println!("Hello, world!");
}
