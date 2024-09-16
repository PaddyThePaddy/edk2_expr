use std::{
    collections::HashMap,
    fmt::{Debug, Display},
    num::ParseIntError,
};

use expr::ExpressionParser;
use lalrpop_util::lalrpop_mod;
use uuid::Uuid;

lalrpop_mod!(expr);
mod token;

pub use token::Expr;
pub use token::ExprOp;
pub use token::ExprVal;

pub type ExprResult<T> = Result<T, ExprError>;
#[derive(Debug, thiserror::Error, strum_macros::EnumIs)]
pub enum ExprError {
    #[error("Parser error: {0}")]
    ParseError(String),
    #[error("Parser int error: {0}")]
    ParseIntError(#[from] ParseIntError),
    #[error("Unescape error: {0}")]
    UnescapeError(#[from] unescaper::Error),
    #[error("UUID error: {0}")]
    UuidError(#[from] uuid::Error),
    #[error("Macro {0} not found in the dictionary")]
    MacroNotFound(String),
    #[error("Macro {0} expansion failed: {1}")]
    MacroExpansionFailed(String, Box<Self>),
    #[error("Invalid binary operator: {0}")]
    InvalidBinaryOp(ExprOp),
    #[error("Invalid unary operator: {0}")]
    InvalidUnaryOp(ExprOp),
    #[error("Invalid operand type: {0}, expect: {1:?}")]
    InvalidOperandType(&'static str, Vec<&'static str>),
    #[error("Expression {0} can not be fully expanded")]
    CantBeFullyExpanded(Expr),
}

impl<L, T, E> From<lalrpop_util::ParseError<L, T, E>> for ExprError
where
    L: Debug + Display,
    T: Debug + Display,
    E: Debug + Display,
{
    fn from(value: lalrpop_util::ParseError<L, T, E>) -> Self {
        Self::ParseError(value.to_string())
    }
}

fn convert_c_format_guid(parts: (u32, u16, u16, u8, u8, u8, u8, u8, u8, u8, u8)) -> Uuid {
    Uuid::from_fields(
        parts.0,
        parts.1,
        parts.2,
        &[
            parts.3, parts.4, parts.5, parts.6, parts.7, parts.8, parts.9, parts.10,
        ],
    )
}

pub fn parse_expr(expr: &str) -> ExprResult<Expr> {
    Ok(ExpressionParser::new().parse(expr)?)
}

pub fn eval(expr: &str, dict: &HashMap<String, String>) -> ExprResult<ExprVal> {
    let expr = parse_expr(expr)?;
    let result = expr
        .eval(dict)?
        .into_owned()
        .try_as_value()
        .ok_or(ExprError::CantBeFullyExpanded(expr))?;
    Ok(result)
}

#[cfg(test)]
mod test {
    use std::collections::HashMap;

    use uuid::uuid;

    use crate::{eval, expr::*, token::*};

    #[test]
    fn test_boolean() {
        assert_eq!(
            ExpressionParser::new().parse("true").unwrap(),
            Expr::Value(ExprVal::Bool(true))
        );
        assert_eq!(
            ExpressionParser::new().parse(r#""test\t str""#).unwrap(),
            "test\t str".to_string().into()
        );
        assert_eq!(
            ExpressionParser::new().parse(r#""test\nstr""#).unwrap(),
            Expr::Value(ExprVal::String("test\nstr".to_string()))
        );
        assert_eq!(
            ExpressionParser::new().parse(r#""test\x12 str""#).unwrap(),
            Expr::Value(ExprVal::String("test\x12 str".to_string()))
        );
        assert_eq!(
            ExpressionParser::new()
                .parse(r#""test\u{1234} str""#)
                .unwrap(),
            Expr::Value(ExprVal::String("test\u{1234} str".to_string()))
        );
        assert_eq!(
            ExpressionParser::new().parse(r#"'test\nstr'"#).unwrap(),
            Expr::Value(ExprVal::String("test\nstr".to_string()))
        );
        assert_eq!(
            ExpressionParser::new().parse(r#"L"test\nstr""#).unwrap(),
            Expr::Value(ExprVal::String("test\nstr".to_string()))
        );
        assert_eq!(
            ExpressionParser::new().parse(r#"L'test\nstr'"#).unwrap(),
            Expr::Value(ExprVal::String("test\nstr".to_string()))
        );
        assert_eq!(
            ExpressionParser::new().parse(r#"12345"#).unwrap(),
            Expr::Value(ExprVal::Int(12345))
        );
        assert_eq!(
            ExpressionParser::new().parse(r#"0x12345"#).unwrap(),
            Expr::Value(ExprVal::Int(0x12345))
        );
        assert_eq!(
            ExpressionParser::new()
                .parse(r#"3a4e8079-2a06-44ea-a8b4-b4c055f3086a"#)
                .unwrap(),
            Expr::Value(ExprVal::Guid(uuid!("3a4e8079-2a06-44ea-a8b4-b4c055f3086a")))
        );
        assert_eq!(ExpressionParser::new().parse(r#"{0x3a4e8079, 0x2a06, 0x44ea, {0xa8, 0xb4, 0xb4, 0xc0, 0x55, 0xf3, 0x08, 0x6a}}"#).unwrap(),
            Expr::Value(ExprVal::Guid(uuid!("3a4e8079-2a06-44ea-a8b4-b4c055f3086a"))));
        assert_eq!(
            ExpressionParser::new().parse(r#"$(A_MACRO)"#).unwrap(),
            Expr::Value(ExprVal::Macro("A_MACRO".to_string()))
        );
        assert_eq!(
            ExpressionParser::new()
                .parse(r#"gTestSpaceGuid.PcdTestPcdName"#)
                .unwrap(),
            Expr::Value(ExprVal::Pcd(
                "gTestSpaceGuid".to_string(),
                "PcdTestPcdName".to_string()
            ))
        );
        let dict = HashMap::from_iter([
            (
                "TEST_MACRO".to_string(),
                "5+gTestSpaceGuid.PcdTestPcdName".to_string(),
            ),
            ("gTestSpaceGuid.PcdTestPcdName".to_string(), "2".to_string()),
            ("A_STR".to_string(), r#""important message""#.to_string()),
            (
                "A_GUID".to_string(),
                "f3a3c2ce-b87b-47ef-8388-f8b3a97102fd".to_string(),
            ),
            ("ARCH_LIST".to_string(), r#"["X64", "IA32"]"#.to_string()),
        ]);
        assert_eq!(
            ExpressionParser::new()
                .parse(r#"gTestSpaceGuid.PcdTestPcdName*(5+4)/3+$(TEST_MACRO)"#)
                .unwrap()
                .eval(&dict)
                .unwrap()
                .into_owned(),
            (2 * (5 + 4) / 3 + (5 + 2)).into()
        );
        assert_eq!(
            ExpressionParser::new()
                .parse(r#"$(A_STR) == "important message""#)
                .unwrap()
                .eval(&dict)
                .unwrap()
                .into_owned(),
            true.into()
        );
        assert_eq!(
            ExpressionParser::new()
                .parse(r#"$(A_STR) == "unimportant message""#)
                .unwrap()
                .eval(&dict)
                .unwrap()
                .into_owned(),
            false.into()
        );
        assert!(ExpressionParser::new()
            .parse(r#"$(NOBODY) == "unimportant message""#)
            .unwrap()
            .eval(&dict)
            .is_err_and(|e| e.is_macro_not_found()));
        assert!(ExpressionParser::new()
            .parse(r#"f3a3c2ce-b87b-47ef-8388-f8b3a97102fd == "unimportant message""#)
            .unwrap()
            .eval(&dict)
            .is_err_and(|e| e.is_invalid_operand_type()));
        assert_eq!(
            ExpressionParser::new()
                .parse(r#"$(A_GUID) == f3a3c2ce-b87b-47ef-8388-f8b3a97102fd"#)
                .unwrap()
                .eval(&dict)
                .unwrap()
                .into_owned(),
            true.into()
        );
        assert_eq!(
            ExpressionParser::new().parse(r#"["X64","IA32",]"#).unwrap(),
            Expr::Value(ExprVal::List(vec!["X64".to_string(), "IA32".to_string()]))
        );
        assert_eq!(
            ExpressionParser::new()
                .parse(r#""X64" in $(ARCH_LIST)"#)
                .unwrap()
                .eval(&dict)
                .unwrap()
                .into_owned(),
            true.into()
        );
        assert_eq!(
            ExpressionParser::new()
                .parse(r#""AARCH" in $(ARCH_LIST)"#)
                .unwrap()
                .eval(&dict)
                .unwrap()
                .into_owned(),
            false.into()
        );
        assert_eq!(
            ExpressionParser::new()
                .parse(r#"1 << 5 % 3"#)
                .unwrap()
                .eval(&dict)
                .unwrap()
                .into_owned(),
            (1 << (5 % 3)).into()
        );
        assert_eq!(
            ExpressionParser::new()
                .parse(r#"1 ^ 5 >> 1"#)
                .unwrap()
                .eval(&dict)
                .unwrap()
                .into_owned(),
            (1 ^ 5 >> 1).into()
        );
        assert_eq!(
            ExpressionParser::new()
                .parse(r#"~(2 & 8)"#)
                .unwrap()
                .eval(&dict)
                .unwrap()
                .into_owned(),
            (!(2 & 8)).into()
        );
        assert_eq!(
            ExpressionParser::new()
                .parse(r#"not true"#)
                .unwrap()
                .eval(&dict)
                .unwrap()
                .into_owned(),
            false.into()
        );
        assert_eq!(
            eval(
                r#"gTestSpaceGuid.PcdTestPcdName*(5+4)/3+$(TEST_MACRO)"#,
                &dict
            )
            .unwrap(),
            (2 * (5 + 4) / 3 + (5 + 2)).into()
        )
    }
}
