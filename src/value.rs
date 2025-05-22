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
    F32(f32),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::String(s) => write!(f, "{s}"),
            Self::Char(c) => write!(f, "{c}"),
            Self::I64(i) => write!(f, "{i}"),
            Self::U64(u) => write!(f, "{u}"),
            Self::F32(f32) => write!(f, "{f32}"),
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
FromValue!(f32 => F32);

macro_rules! Op {
    ($trait:ty: $fn_name:ident $op:tt { $($field:ident),+ $(& $($rest:tt)+)? }) => {
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

Op!(Add<&Self>: add + {
    String,
    I64,
    U64,
    F32 &
    (Self::Char(a), Self::Char(b)) => (a.to_string() + &b.to_string()).into()
});
Op!(Sub<&Self>: sub - { I64, U64, F32 });
Op!(Mul<&Self>: mul * { I64, U64, F32 });
Op!(Div<&Self>: div / { I64, U64, F32 });
