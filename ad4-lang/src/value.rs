use std::ops::{Add, Div, Mul, Sub};

use macros::From;
use strum::Display;

#[derive(Debug, Display, Clone, From)]
pub enum Value {
    #[strum(to_string = "\"{0}\"")]
    String(#[from] String),
    #[strum(to_string = "'{0}'")]
    Char(#[from] char),
    #[strum(to_string = "{0}")]
    I64(#[from] i64),
    #[strum(to_string = "{0}")]
    U64(#[from] u64),
    #[strum(to_string = "{0}")]
    I32(#[from] i32),
    #[strum(to_string = "{0}")]
    U32(#[from] u32),
    #[strum(to_string = "{0}")]
    F64(#[from] f64),
    #[strum(to_string = "{0}")]
    F32(#[from] f32),
    #[strum(to_string = "{0}")]
    Bool(#[from] bool),

    #[strum(to_string = "{0:?}")]
    Vec(#[from] Vec<Value>),

    #[strum(to_string = "void")]
    Void,
}

impl From<()> for Value {
    fn from((): ()) -> Self {
        Self::Void
    }
}

macro_rules! Op {
    ($trait:ty: $fn_name:ident $op:tt ( $($field:ident),+ ) $( & { $($rest:tt)+ } )? ) => {
        impl $trait for Value {
            type Output = ValueOperationResult<Self>;

            fn $fn_name (self, rhs: &Self) -> Self::Output {
                Ok(match (self, rhs) {
                    $(
                        (Self::$field(lhs), Self::$field(rhs)) => (lhs $op rhs).into(),
                    )+
                    $($($rest)+ ,)?
                    (lhs, rhs) => return Err(format!(
                        "Invalid operation, trying to perform '{}' (`{}`) on {lhs:?} and {rhs:?} which is not supported",
                        stringify!($fn_name),
                        stringify!($op)
                    ))
                })
            }
        }
    };
}

pub type ValueOperationResult<T> = Result<T, String>;

Op!(Add<&Self>: add + (String, I64, U64, F64, F32) & {
    (Self::Char(a), Self::Char(b)) => (a.to_string() + &b.to_string()).into()
});
Op!(Sub<&Self>: sub - ( I64, U64, F64, F32 ));
Op!(Mul<&Self>: mul * ( I64, U64, F64, F32 ));
Op!(Div<&Self>: div / ( I64, U64, F64, F32 ));

macro_rules! impl_cmp {
    ($($field:ident),+ $(,)? & {
        $($rest:tt)+
    }) => {
        impl PartialOrd for Value {
            fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
                match (self, other) {
                    $((Self::$field(lhs), Self::$field(rhs)) => lhs.partial_cmp(rhs),)+
                    $($rest)+
                    (lhs, rhs) => panic!("Invalid Operation. Trying to compare {lhs:?} and {rhs:?}"),
                }
            }
        }
    };
}

impl_cmp!(
    String,
    Char,
    I64,
    U64,
    I32,
    U32,
    F64,
    F32 & {
    (Self::Void, Self::Void) => None,
});

macro_rules! impl_eq {
    ($($field:ident),+ $(,)? & {
        $($rest:tt)+
    }) => {
        impl PartialEq for Value {
            fn eq(&self, other: &Self) -> bool {
                match (self, other) {
                    $((Self::$field(lhs), Self::$field(rhs)) => lhs == rhs,)+
                    $($rest)+
                    (lhs, rhs) => panic!("Invalid Operation. Trying to compare {lhs:?} and {rhs:?}"),
                }
            }
        }
    };
}

impl_eq!(
    String,
    Char,
    I64,
    U64,
    I32,
    U32,
    F64,
    F32,
    Bool,
    Vec, & {
    (Self::Void, Self::Void) => true,
    (Self::I64(lhs), Self::U32(rhs)) => *lhs == i64::from(*rhs),
    (Self::U32(lhs), Self::I64(rhs)) => i64::from(*lhs) == *rhs,

    (Self::I64(lhs), Self::I32(rhs)) => *lhs == i64::from(*rhs),
    (Self::I32(lhs), Self::I64(rhs)) => i64::from(*lhs) == *rhs,

    (Self::U64(lhs), Self::U32(rhs)) => *lhs == u64::from(*rhs),
    (Self::U32(lhs), Self::U64(rhs)) => u64::from(*lhs) == *rhs,

    (Self::U64(lhs), Self::I32(rhs)) => Ok(*lhs) == u64::try_from(*rhs),
    (Self::I32(lhs), Self::U64(rhs)) => u64::try_from(*lhs) == Ok(*rhs),

    (Self::F64(lhs), Self::F32(rhs)) => *lhs == f64::from(*rhs),
    (Self::F32(lhs), Self::F64(rhs)) => f64::from(*lhs) == *rhs,
});
