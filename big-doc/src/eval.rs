use crate::ast::Expr;
use crate::ast::EExpr;
use crate::ast::BinOp;
use crate::ast::UnaryOp;
use std::collections::HashMap;

#[derive(Debug)]
pub enum Value {
    BoolValue(bool),
    NumberValue(i64),
    StringValue(String),
    TupleValue(Vec<Box<Value>>),
    BindValue { name: String, value: Box<Value> },
    FunctionValue { arg: String, body: Box<Expr> },
}

pub fn get_eval() -> Box<dyn Fn(Expr) -> Value> {
    let mut bindings: HashMap<String, Value> = HashMap::new();
    fn eval(expr: Expr) -> Value {
        match expr.expr {
            EExpr::BoolExpr(val) => Value::BoolValue(val),
            EExpr::NumberExpr(val) => Value::NumberValue(val),
            EExpr::StringExpr(val) => Value::StringValue(val),
            EExpr::TupleExpr(val) => {
                Value::TupleValue(val.into_iter().map(|e| Box::new(eval(*e))).collect())
            },
            EExpr::FunctionExpr { arg, body } => Value::FunctionValue{ arg: arg, body: body  },
            EExpr::IfExpr { cond, yes, no } => {
                let cond_val = eval(*cond);
                match cond_val {
                    Value::BoolValue(true) => eval(*yes),
                    Value::BoolValue(false) => eval(*no),
                    _ => panic!("wrong cond type"),
                }
            }
            EExpr::BinOpExpr { lhs, op, rhs } => {
                match op {
                    BinOp::Plus => match (eval(*lhs), eval(*rhs)) {
                        (Value::NumberValue(lhs_val), Value::NumberValue(rhs_val)) => Value::NumberValue(lhs_val + rhs_val),
                        _ => panic!("wrong operand type")
                    },
                    BinOp::Minus => match (eval(*lhs), eval(*rhs)) {
                        (Value::NumberValue(lhs_val), Value::NumberValue(rhs_val)) => Value::NumberValue(lhs_val - rhs_val),
                        _ => panic!("wrong operand type")
                    },
                    BinOp::Times => match (eval(*lhs), eval(*rhs)) {
                        (Value::NumberValue(lhs_val), Value::NumberValue(rhs_val)) => Value::NumberValue(lhs_val * rhs_val),
                        _ => panic!("wrong operand type")
                    },
                    BinOp::Divide => match (eval(*lhs), eval(*rhs)) {
                        (Value::NumberValue(lhs_val), Value::NumberValue(rhs_val)) if rhs_val != 0 => Value::NumberValue(lhs_val / rhs_val),
                        (Value::NumberValue(_), Value::NumberValue(rhs_val)) if rhs_val == 0 => panic!("division by zero"),
                        _ => panic!("wrong operand type")
                    },
                    BinOp::And => match (eval(*lhs), eval(*rhs)) {
                        (Value::NumberValue(lhs_val), Value::NumberValue(rhs_val)) => Value::NumberValue(lhs_val & rhs_val),
                        (Value::BoolValue(lhs_val), Value::BoolValue(rhs_val)) => Value::BoolValue(lhs_val & rhs_val),
                        _ => panic!("wrong operand type")
                    },
                    BinOp::Or => match (eval(*lhs), eval(*rhs)) {
                        (Value::NumberValue(lhs_val), Value::NumberValue(rhs_val)) => Value::NumberValue(lhs_val | rhs_val),
                        (Value::BoolValue(lhs_val), Value::BoolValue(rhs_val)) => Value::BoolValue(lhs_val | rhs_val),
                        _ => panic!("wrong operand type")
                    },
                    BinOp::Xor => match (eval(*lhs), eval(*rhs)) {
                        (Value::NumberValue(lhs_val), Value::NumberValue(rhs_val)) => Value::NumberValue(lhs_val ^ rhs_val),
                        (Value::BoolValue(lhs_val), Value::BoolValue(rhs_val)) => Value::BoolValue(lhs_val ^ rhs_val),
                        _ => panic!("wrong operand type")
                    },
                }
            },
            EExpr::UnaryOpExpr { op, expr } => {
                match op {
                    UnaryOp::Negative => match eval(*expr) {
                        Value::NumberValue(val) => Value::NumberValue(-val),
                        _ => panic!("wrong operand type")
                    },
                    UnaryOp::Not => match eval(*expr) {
                        Value::BoolValue(val) => Value::BoolValue(!val),
                        _ => panic!("wrong operand type")
                    }
                }
            },
            EExpr::LetExpr { name, value, body } => {
                panic!("not implemented")
            }
        }
    }
    Box::new(eval)
}