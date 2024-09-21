use std::{borrow::Cow, collections::HashMap};

use uuid::Uuid;

use crate::{expr::*, ExprError, ExprResult};

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    strum_macros::Display,
    strum_macros::IntoStaticStr,
    strum_macros::AsRefStr,
    strum_macros::EnumTryAs,
)]
pub enum ExprOp {
    Add,
    Sub,
    Mul,
    Div,
    Gt,
    Lt,
    Ge,
    Le,
    Eq,
    Ne,
    Rsh,
    Lsh,
    BitAnd,
    BitOr,
    BitXor,
    BitNot,
    Not,
    And,
    Or,
    Xor,
    Mod,
    In,
    Defined,
}

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    strum_macros::IntoStaticStr,
    strum_macros::AsRefStr,
    strum_macros::Display,
    strum_macros::EnumTryAs,
    derive_more::From,
)]
pub enum ExprVal {
    Int(u64),
    Bool(bool),
    Pcd(String, String),
    #[from(skip)]
    Macro(String),
    Guid(Uuid),
    List(Vec<String>),
    String(String),
}

impl ExprVal {
    pub fn type_str(&self) -> &'static str {
        self.into()
    }
}

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    strum_macros::IntoStaticStr,
    strum_macros::AsRefStr,
    strum_macros::Display,
    strum_macros::EnumTryAs,
)]
pub enum Expr {
    Value(ExprVal),
    BinaryOp(Box<Expr>, ExprOp, Box<Expr>),
    UnaryOp(ExprOp, Box<Expr>),
    TernaryCond(Box<Expr>, Box<Expr>, Box<Expr>),
}

impl<T> From<T> for Expr
where
    T: Into<ExprVal>,
{
    fn from(value: T) -> Self {
        Expr::Value(value.into())
    }
}

impl Expr {
    pub fn eval(&self, dict: &HashMap<String, String>) -> ExprResult<Cow<Expr>> {
        match self {
            Expr::Value(v) => {
                match v {
                    ExprVal::Macro(macro_name) => {
                        if let Some(expanded_content) = dict.get(macro_name) {
                            let expanded = ExpressionParser::new()
                                .parse(expanded_content)
                                .map_err(|e| {
                                    ExprError::MacroExpansionFailed(
                                        macro_name.clone(),
                                        Box::new(e.into()),
                                    )
                                })?;
                            Ok(Cow::Owned(expanded))
                        } else {
                            Err(ExprError::MacroNotFound(macro_name.to_string()))
                        }
                    }
                    ExprVal::Pcd(space, name) => {
                        let pcd_id = format!("{space}.{name}");

                        if let Some(expanded_content) = dict.get(&pcd_id) {
                            let expanded = ExpressionParser::new()
                                .parse(expanded_content)
                                .map_err(|e| {
                                    ExprError::MacroExpansionFailed(pcd_id, Box::new(e.into()))
                                })?;
                            Ok(Cow::Owned(expanded))
                        } else {
                            Err(ExprError::MacroNotFound(pcd_id))
                        }
                    }
                    _ => Ok(Cow::Borrowed(self)),
                }
            }
            Expr::BinaryOp(lhs, op, rhs) => {
                let mut expanded_lhd = lhs.eval(dict)?;
                while let Cow::Owned(new_expanded) = expanded_lhd.eval(dict)? {
                    expanded_lhd = Cow::Owned(new_expanded);
                }

                let mut expanded_rhd = rhs.eval(dict)?;
                while let Cow::Owned(new_expanded) = expanded_rhd.eval(dict)? {
                    expanded_rhd = Cow::Owned(new_expanded);
                }

                if let Some((lhs, rhs)) = expanded_lhd
                    .try_as_value_ref()
                    .zip(expanded_rhd.try_as_value_ref())
                {
                    match op {
                        ExprOp::Sub
                        | ExprOp::Mul
                        | ExprOp::Div
                        | ExprOp::Gt
                        | ExprOp::Lt
                        | ExprOp::Ge
                        | ExprOp::Le
                        | ExprOp::Rsh
                        | ExprOp::Lsh
                        | ExprOp::BitAnd
                        | ExprOp::BitOr
                        | ExprOp::BitXor
                        | ExprOp::Mod
                        | ExprOp::Add => {
                            let lhs = lhs
                                .try_as_int_ref()
                                .copied()
                                .or(lhs.try_as_bool_ref().map(|b| if *b { 1 } else { 0 }))
                                .ok_or(ExprError::InvalidOperandType(
                                    lhs.type_str(),
                                    vec!["Int", "Bool"],
                                ))?;
                            let rhs = rhs
                                .try_as_int_ref()
                                .copied()
                                .or(rhs.try_as_bool_ref().map(|b| if *b { 1 } else { 0 }))
                                .ok_or(ExprError::InvalidOperandType(
                                    rhs.type_str(),
                                    vec!["Int", "Bool"],
                                ))?;
                            let result = match op {
                                ExprOp::Add => ExprVal::Int(lhs + rhs),
                                ExprOp::Sub => ExprVal::Int(lhs - rhs),
                                ExprOp::Mul => ExprVal::Int(lhs * rhs),
                                ExprOp::Div => ExprVal::Int(lhs / rhs),
                                ExprOp::Gt => ExprVal::Bool(lhs > rhs),
                                ExprOp::Lt => ExprVal::Bool(lhs < rhs),
                                ExprOp::Ge => ExprVal::Bool(lhs >= rhs),
                                ExprOp::Le => ExprVal::Bool(lhs <= rhs),
                                ExprOp::Lsh => ExprVal::Int(lhs << rhs),
                                ExprOp::Rsh => ExprVal::Int(lhs >> rhs),
                                ExprOp::BitAnd => ExprVal::Int(lhs & rhs),
                                ExprOp::BitOr => ExprVal::Int(lhs | rhs),
                                ExprOp::BitXor => ExprVal::Int(lhs ^ rhs),
                                ExprOp::Mod => ExprVal::Int(lhs % rhs),
                                _ => unreachable!("Not possible"),
                            };
                            Ok(Cow::Owned(Expr::Value(result)))
                        }
                        ExprOp::And | ExprOp::Or | ExprOp::Xor => {
                            let lhs = lhs
                                .try_as_bool_ref()
                                .copied()
                                .or(lhs.try_as_int_ref().map(|i| *i != 0))
                                .ok_or(ExprError::InvalidOperandType(
                                    lhs.type_str(),
                                    vec!["Int", "Bool"],
                                ))?;
                            let rhs = rhs
                                .try_as_bool_ref()
                                .copied()
                                .or(rhs.try_as_int_ref().map(|i| *i != 0))
                                .ok_or(ExprError::InvalidOperandType(
                                    rhs.type_str(),
                                    vec!["Int", "Bool"],
                                ))?;
                            let result = match op {
                                ExprOp::And => ExprVal::Bool(lhs && rhs),
                                ExprOp::Or => ExprVal::Bool(lhs || rhs),
                                ExprOp::Xor => ExprVal::Bool(lhs ^ rhs),
                                _ => unreachable!("Not possible"),
                            };
                            Ok(Cow::Owned(Expr::Value(result)))
                        }
                        ExprOp::In => {
                            let lhs =
                                lhs.try_as_string_ref()
                                    .ok_or(ExprError::InvalidOperandType(
                                        lhs.type_str(),
                                        vec!["String"],
                                    ))?;
                            let rhs = rhs.try_as_list_ref().ok_or(
                                ExprError::InvalidOperandType(rhs.type_str(), vec!["List"]),
                            )?;
                            Ok(Cow::Owned(Expr::Value(ExprVal::Bool(rhs.contains(lhs)))))
                        }
                        ExprOp::Eq | ExprOp::Ne => {
                            if lhs.type_str() != rhs.type_str() {
                                return Err(ExprError::InvalidOperandType(
                                    rhs.type_str(),
                                    vec![lhs.type_str()],
                                ));
                            }
                            match op {
                                ExprOp::Eq => {
                                    Ok(Cow::Owned(Expr::Value(ExprVal::Bool(lhs == rhs))))
                                }
                                _ => unreachable!("Not possible"),
                            }
                        }
                        _ => Err(ExprError::InvalidBinaryOp(*op)),
                    }
                } else {
                    Err(ExprError::CantBeFullyExpanded(self.clone()))
                }
            }
            Expr::UnaryOp(op, operand) => {
                let mut expanded_operand = operand.eval(dict)?;
                while let Cow::Owned(new_expanded) = expanded_operand.eval(dict)? {
                    expanded_operand = Cow::Owned(new_expanded);
                }

                if let Some(operand) = expanded_operand.try_as_value_ref() {
                    match op {
                        ExprOp::Not => {
                            let operand = operand
                                .try_as_bool_ref()
                                .copied()
                                .or(operand.try_as_int_ref().map(|i| *i != 0))
                                .ok_or(ExprError::InvalidOperandType(
                                    operand.type_str(),
                                    vec!["Int", "Bool"],
                                ))?;
                            Ok(Cow::Owned(Expr::Value(ExprVal::Bool(!operand))))
                        }
                        ExprOp::BitNot => {
                            let operand = operand
                                .try_as_int_ref()
                                .copied()
                                .or(operand.try_as_bool_ref().map(|b| if *b { 1 } else { 0 }))
                                .ok_or(ExprError::InvalidOperandType(
                                    operand.type_str(),
                                    vec!["Int", "Bool"],
                                ))?;
                            Ok(Cow::Owned(Expr::Value(ExprVal::Int(!operand))))
                        }
                        ExprOp::Defined => {
                            let operand = operand.try_as_string_ref().ok_or(
                                ExprError::InvalidOperandType(operand.type_str(), vec!["String"]),
                            )?;
                            Ok(Cow::Owned(dict.contains_key(operand).into()))
                        }
                        _ => Err(ExprError::InvalidUnaryOp(*op)),
                    }
                } else {
                    Err(ExprError::CantBeFullyExpanded(self.clone()))
                }
            }
            Expr::TernaryCond(cond, when_true, when_false) => {
                let mut expanded_cond = cond.eval(dict)?;
                while let Cow::Owned(new_expanded) = expanded_cond.eval(dict)? {
                    expanded_cond = Cow::Owned(new_expanded);
                }
                let mut expanded_when_true = when_true.eval(dict)?;
                while let Cow::Owned(new_expanded) = expanded_when_true.eval(dict)? {
                    expanded_when_true = Cow::Owned(new_expanded);
                }
                let mut expanded_when_false = when_false.eval(dict)?;
                while let Cow::Owned(new_expanded) = expanded_when_false.eval(dict)? {
                    expanded_when_false = Cow::Owned(new_expanded);
                }

                let cond = expanded_cond
                    .try_as_value_ref()
                    .ok_or(ExprError::CantBeFullyExpanded(self.clone()))?;
                let cond = cond
                    .try_as_bool_ref()
                    .copied()
                    .or(cond.try_as_int_ref().map(|i| *i != 0))
                    .ok_or(ExprError::InvalidOperandType(
                        cond.type_str(),
                        vec!["Int", "Bool"],
                    ))?;
                let (when_true, when_false) = expanded_when_true
                    .try_as_value_ref()
                    .zip(expanded_when_false.try_as_value_ref())
                    .ok_or(ExprError::CantBeFullyExpanded(self.clone()))?;
                Ok(Cow::Owned(Expr::Value(if cond {
                    when_true.clone()
                } else {
                    when_false.clone()
                })))
            }
        }
    }
}
