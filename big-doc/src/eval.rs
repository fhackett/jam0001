use crate::ast::Expr;
use crate::ast::EExpr;
use crate::ast::BinOp;
use crate::ast::UnaryOp;
use im_rc::hashmap::HashMap;

#[derive(Debug, Clone)]
pub enum Value {
    BoolValue(bool),
    NumberValue(i64),
    StringValue(String),
    TupleValue(Vec<Box<Value>>),
    FunctionValue { arg: String, body: Box<Expr> },
}

pub fn eval(expr: Expr, bindings: &HashMap<String, Value>) -> Value {
    match expr.expr {
        EExpr::BoolExpr(val) => Value::BoolValue(val),
        EExpr::NumberExpr(val) => Value::NumberValue(val),
        EExpr::StringExpr(val) => Value::StringValue(val),
        EExpr::TupleExpr(val) => {
            Value::TupleValue(val.into_iter().map(|e| Box::new(eval(*e, bindings))).collect())
        },
        EExpr::IdentifierExpr(name) => {
            match bindings.get(&name) {
                Some(val) => val.clone(),
                None => panic!("identifier not declared")
            }       
        }
        EExpr::FunctionExpr { arg, body } => Value::FunctionValue{ arg: arg, body: body  },
        EExpr::IfExpr { cond, yes, no } => {
            let cond_val = eval(*cond, bindings);
            match cond_val {
                Value::BoolValue(true) => eval(*yes, bindings),
                Value::BoolValue(false) => eval(*no, bindings),
                _ => panic!("wrong cond type"),
            }
        }
        EExpr::BinOpExpr { lhs, op, rhs } => {
            match op {
                BinOp::Plus => match (eval(*lhs, bindings), eval(*rhs, bindings)) {
                    (Value::NumberValue(lhs_val), Value::NumberValue(rhs_val)) => Value::NumberValue(lhs_val + rhs_val),
                    _ => panic!("wrong operand type")
                },
                BinOp::Minus => match (eval(*lhs, bindings), eval(*rhs, bindings)) {
                    (Value::NumberValue(lhs_val), Value::NumberValue(rhs_val)) => Value::NumberValue(lhs_val - rhs_val),
                    _ => panic!("wrong operand type")
                },
                BinOp::Times => match (eval(*lhs, bindings), eval(*rhs, bindings)) {
                    (Value::NumberValue(lhs_val), Value::NumberValue(rhs_val)) => Value::NumberValue(lhs_val * rhs_val),
                    _ => panic!("wrong operand type")
                },
                BinOp::Divide => match (eval(*lhs, bindings), eval(*rhs, bindings)) {
                    (Value::NumberValue(lhs_val), Value::NumberValue(rhs_val)) if rhs_val != 0 => Value::NumberValue(lhs_val / rhs_val),
                    (Value::NumberValue(_), Value::NumberValue(rhs_val)) if rhs_val == 0 => panic!("division by zero"),
                    _ => panic!("wrong operand type")
                },
                BinOp::And => match (eval(*lhs, bindings), eval(*rhs, bindings)) {
                    (Value::NumberValue(lhs_val), Value::NumberValue(rhs_val)) => Value::NumberValue(lhs_val & rhs_val),
                    (Value::BoolValue(lhs_val), Value::BoolValue(rhs_val)) => Value::BoolValue(lhs_val & rhs_val),
                    _ => panic!("wrong operand type")
                },
                BinOp::Or => match (eval(*lhs, bindings), eval(*rhs, bindings)) {
                    (Value::NumberValue(lhs_val), Value::NumberValue(rhs_val)) => Value::NumberValue(lhs_val | rhs_val),
                    (Value::BoolValue(lhs_val), Value::BoolValue(rhs_val)) => Value::BoolValue(lhs_val | rhs_val),
                    _ => panic!("wrong operand type")
                },
                BinOp::Xor => match (eval(*lhs, bindings), eval(*rhs, bindings)) {
                    (Value::NumberValue(lhs_val), Value::NumberValue(rhs_val)) => Value::NumberValue(lhs_val ^ rhs_val),
                    (Value::BoolValue(lhs_val), Value::BoolValue(rhs_val)) => Value::BoolValue(lhs_val ^ rhs_val),
                    _ => panic!("wrong operand type")
                },
            }
        },
        EExpr::UnaryOpExpr { op, expr } => {
            match op {
                UnaryOp::Negative => match eval(*expr, bindings) {
                    Value::NumberValue(val) => Value::NumberValue(-val),
                    _ => panic!("wrong operand type")
                },
                UnaryOp::Not => match eval(*expr, bindings) {
                    Value::BoolValue(val) => Value::BoolValue(!val),
                    _ => panic!("wrong operand type")
                }
            }
        },
        EExpr::LetExpr { name, value, body } => {
            eval(*body, &bindings.update(name, eval(*value, bindings)))
        }
    }
}