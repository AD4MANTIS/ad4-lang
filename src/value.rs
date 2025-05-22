use std::{
    fmt::Display,
    ops::{Add, Div, Mul, Sub},
};

#[derive(Debug, Clone)]
pub enum Value {
    String(String),
    Char(char),
    I64(i64),
    U64(u64),
    I32(i32),
    U32(u32),
    F64(f64),
    F32(f32),
    Bool(bool),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::String(s) => write!(f, "{s}"),
            Self::Char(c) => write!(f, "{c}"),
            Self::I64(i) => write!(f, "{i}"),
            Self::U64(u) => write!(f, "{u}"),
            Self::I32(i) => write!(f, "{i}"),
            Self::U32(u) => write!(f, "{u}"),
            Self::F64(f64) => write!(f, "{f64}"),
            Self::F32(f32) => write!(f, "{f32}"),
            Self::Bool(bool) => write!(f, "{bool}"),
        }
    }
}

macro_rules! FromValue {
    ($type:ty => $field:ident) => {
        impl From<$type> for Value {
            fn from(value: $type) -> Self {
                Self::$field(value)
            }
        }
    };
}

FromValue!(String => String);
FromValue!(char => Char);
FromValue!(i64 => I64);
FromValue!(u64 => U64);
FromValue!(i32 => I32);
FromValue!(u32 => U32);
FromValue!(f64 => F64);
FromValue!(f32 => F32);
FromValue!(bool => Bool);

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
                    _ => return Err("invalid operation")
                })
            }
        }
    };
}

pub type ValueOperationResult<T> = Result<T, &'static str>;

Op!(Add<&Self>: add + (String, I64, U64, F64, F32) & {
    (Self::Char(a), Self::Char(b)) => (a.to_string() + &b.to_string()).into()
});
Op!(Sub<&Self>: sub - ( I64, U64, F64, F32 ));
Op!(Mul<&Self>: mul * ( I64, U64, F64, F32 ));
Op!(Div<&Self>: div / ( I64, U64, F64, F32 ));

macro_rules! impl_eq {
    ($($field:ident),+ $(,)? & {
        $($rest:tt)+
    }) => {
        impl PartialEq for Value {
            fn eq(&self, other: &Self) -> bool {
                match (self, other) {
                    $((Self::$field(l0), Self::$field(r0)) => l0 == r0,)+
                    $($rest)+
                    (lhs, rhs) => panic!("Invalid Operation. Trying to compare {lhs:?} and {rhs:?}"),
                }
            }
        }
    };
}

impl_eq!(String, Char, I64, U64, I32, U32, F64, F32, Bool & {
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
