use std::ops::Add;

#[derive(Debug, Clone)]
pub enum Value {
    String(String),
    Char(char),
    I64(i64),
    U64(u64),
    Float(f32),
}

impl From<String> for Value {
    fn from(value: String) -> Self {
        Self::String(value)
    }
}
impl From<char> for Value {
    fn from(value: char) -> Self {
        Self::Char(value)
    }
}
impl From<i64> for Value {
    fn from(value: i64) -> Self {
        Self::I64(value)
    }
}
impl From<u64> for Value {
    fn from(value: u64) -> Self {
        Self::U64(value)
    }
}
impl From<f32> for Value {
    fn from(value: f32) -> Self {
        Self::Float(value)
    }
}

impl Add<&Self> for Value {
    type Output = Option<Self>;

    fn add(self, rhs: &Self) -> Self::Output {
        Some(match (self, rhs) {
            (Self::String(a), Self::String(b)) => (a + b).into(),
            (Self::Char(a), Self::Char(b)) => (a.to_string() + &b.to_string()).into(),
            (Self::I64(a), Self::I64(b)) => (a + b).into(),
            (Self::U64(a), Self::U64(b)) => (a + b).into(),
            (Self::Float(a), Self::Float(b)) => (a + b).into(),
            _ => return None,
        })
    }
}
