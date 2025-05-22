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

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::String(l0), Self::String(r0)) => l0 == r0,
            (Self::Char(l0), Self::Char(r0)) => l0 == r0,
            (Self::I64(l0), Self::I64(r0)) => l0 == r0,
            (Self::U64(l0), Self::U64(r0)) => l0 == r0,
            (Self::F64(l0), Self::F64(r0)) => l0 == r0,
            (Self::F32(l0), Self::F32(r0)) => l0 == r0,
            (Self::Bool(l0), Self::Bool(r0)) => l0 == r0,
            _ => panic!("Invalid Operation"),
        }
    }
}
