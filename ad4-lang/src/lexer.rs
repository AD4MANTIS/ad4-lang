use strum::IntoEnumIterator;

use crate::{COMMA, Operator, SEMICOLON, Token, TokenError, literal::parse_float};

#[derive(Debug)]
pub struct Lexer {
    tokens: Vec<Token>,
}

#[derive(thiserror::Error, Debug)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub enum TokenizeError {
    #[error(transparent)]
    Token(#[from] TokenError),
}

impl Lexer {
    pub fn build(input: &str) -> Result<Self, TokenizeError> {
        let mut tokens = tokenize(input)
            .collect::<Result<Vec<_>, _>>()
            .map_err(TokenizeError::from)?;
        tokens.reverse();
        Ok(Self { tokens })
    }

    #[must_use]
    pub fn peek(&self) -> Option<&Token> {
        self.tokens.last()
    }

    pub fn is_next(&self, token: &Token) -> bool {
        self.peek() == Some(token)
    }
}

impl Iterator for Lexer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.tokens.pop()
    }
}

fn tokenize(input: &str) -> impl Iterator<Item = Result<Token, TokenError>> {
    let split_input = split_into_tokens(input);

    split_input.into_iter().map(str::parse)
}

fn split_into_tokens(input: &str) -> impl Iterator<Item = &str> {
    let mut current_literal_start: Option<char> = None;
    let mut bracket_depth = 0;

    #[allow(
        clippy::needless_collect,
        reason = "Because the closure captures `current_literal_start` and we can't return a reference to this local variable"
    )]
    let split_input = input
        .split_inclusive(|char| match char {
            '\'' | '"' => {
                match current_literal_start {
                    Some(current_start) => {
                        if current_start == char {
                            current_literal_start = None;
                            return true;
                        }
                    }
                    None => {
                        current_literal_start = Some(char);
                    }
                }

                false
            }
            '[' => {
                bracket_depth += 1;

                false
            }
            ']' => {
                bracket_depth -= 1;

                true
            }
            _ => char.is_whitespace() && current_literal_start.is_none(),
        })
        .map(str::trim)
        .filter(|tokens| !tokens.is_empty())
        .flat_map(|parsing_str| split_special_operators(parsing_str))
        .map(str::trim)
        .filter(|char| !char.is_empty())
        .collect::<Vec<_>>();

    assert!(
        current_literal_start.is_none(),
        "Expected closing delimiter for `{}`",
        current_literal_start.unwrap_or_default()
    );

    assert!(bracket_depth == 0, "Expected {bracket_depth} `]`");

    split_input.into_iter()
}

fn split_special_operators(parsing_str: &str) -> Box<dyn Iterator<Item = &str> + '_> {
    if parsing_str.starts_with(['"', '\\']) {
        return Box::new(std::iter::once(parsing_str));
    }

    let tokens = [SEMICOLON, COMMA]
        .into_iter()
        .chain(Operator::iter().map(Operator::as_str));

    for token_str in tokens {
        let Some(op_pos) = parsing_str.find(token_str) else {
            continue;
        };

        if token_str == Operator::Dot.as_str() && parse_float(parsing_str).is_ok() {
            continue;
        }

        return Box::new(
            [
                split_special_operators(&parsing_str[..op_pos]),
                Box::new(std::iter::once(token_str)),
                split_special_operators(&parsing_str[(op_pos + token_str.len())..]),
            ]
            .into_iter()
            .flatten(),
        );
    }

    Box::new(std::iter::once(parsing_str))
}

#[cfg(test)]
mod tokenize_test {
    use super::*;

    use crate::{Keyword, Operator};

    fn test_tokenize(input: &str, expected: &[Token]) {
        let expr = tokenize(input)
            .collect::<Result<Vec<_>, _>>()
            .expect("Test should tokenize");

        assert_eq!(
            expr, expected,
            "Input: {input}\nExpected: {expected:#?}\nActual: {expr:#?}"
        );
    }

    #[test]
    fn numbers() {
        test_tokenize("1", &[Token::literal(1.into())]);
    }

    #[test]
    fn keywords() {
        test_tokenize("let", &[Token::kw(Keyword::let_())]);
    }

    #[test]
    fn operators() {
        test_tokenize("+", &[Token::Op(Operator::Add)]);
    }

    #[test]
    fn expressions() {
        test_tokenize(
            "1 + 2",
            &[
                Token::literal(1.into()),
                Token::Op(Operator::Add),
                Token::literal(2.into()),
            ],
        );
    }

    #[test]
    fn compare() {
        test_tokenize(
            "1 < 2",
            &[
                Token::literal(1.into()),
                Token::Op(Operator::Lt),
                Token::literal(2.into()),
            ],
        );
    }

    #[test]
    fn compare_gt_eq() {
        test_tokenize(
            "1 >= 2",
            &[
                Token::literal(1.into()),
                Token::Op(Operator::Geq),
                Token::literal(2.into()),
            ],
        );
    }

    #[test]
    fn lt_eq_with_space() {
        test_tokenize(
            "1 < = 2",
            &[
                Token::literal(1.into()),
                Token::Op(Operator::Lt),
                Token::Op(Operator::Assign),
                Token::literal(2.into()),
            ],
        );
    }

    #[test]
    fn variables() {
        test_tokenize("a", &[Token::var("a")]);
    }

    #[test]
    fn semicolon() {
        test_tokenize(";", &[Token::Semicolon()]);
    }

    #[test]
    fn vector() {
        test_tokenize(
            "[1, 2, 3]",
            &[
                Token::Op(Operator::OpeningSquareBracket),
                Token::literal(1.into()),
                Token::Comma(),
                Token::literal(2.into()),
                Token::Comma(),
                Token::literal(3.into()),
                Token::Op(Operator::ClosingSquareBracket),
            ],
        );
    }

    #[test]
    fn multiple_tokens() {
        test_tokenize(
            "let a = 1;",
            &[
                Token::kw(Keyword::let_()),
                Token::var("a"),
                Token::Op(Operator::Assign),
                Token::literal(1.into()),
                Token::Semicolon(),
            ],
        );
    }

    #[test]
    fn multiple_tokens_with_semicolons() {
        test_tokenize(
            "let a = 1;; let b = 2;",
            &[
                Token::kw(Keyword::let_()),
                Token::var("a"),
                Token::Op(Operator::Assign),
                Token::literal(1.into()),
                Token::Semicolon(),
                Token::Semicolon(),
                Token::kw(Keyword::let_()),
                Token::var("b"),
                Token::Op(Operator::Assign),
                Token::literal(2.into()),
                Token::Semicolon(),
            ],
        );
    }

    #[test]
    fn braced_expressions() {
        test_tokenize(
            "(1 + 2)",
            &[
                Token::Op(Operator::OpeningBracket),
                Token::literal(1.into()),
                Token::Op(Operator::Add),
                Token::literal(2.into()),
                Token::Op(Operator::ClosingBracket),
            ],
        );
    }

    #[test]
    fn nested_braces() {
        test_tokenize(
            "((1 + 2))",
            &[
                Token::Op(Operator::OpeningBracket),
                Token::Op(Operator::OpeningBracket),
                Token::literal(1.into()),
                Token::Op(Operator::Add),
                Token::literal(2.into()),
                Token::Op(Operator::ClosingBracket),
                Token::Op(Operator::ClosingBracket),
            ],
        );
    }

    #[test]
    fn blocks() {
        test_tokenize(
            "{ -1 + 2 }",
            &[
                Token::Op(Operator::OpeningCurlyBrace),
                Token::Op(Operator::Sub),
                Token::literal(1.into()),
                Token::Op(Operator::Add),
                Token::literal(2.into()),
                Token::Op(Operator::ClosingCurlyBrace),
            ],
        );
    }

    #[test]
    fn nested_blocks() {
        test_tokenize(
            "{ let a = { 1 + 2 } }",
            &[
                Token::Op(Operator::OpeningCurlyBrace),
                Token::kw(Keyword::let_()),
                Token::var("a"),
                Token::Op(Operator::Assign),
                Token::Op(Operator::OpeningCurlyBrace),
                Token::literal(1.into()),
                Token::Op(Operator::Add),
                Token::literal(2.into()),
                Token::Op(Operator::ClosingCurlyBrace),
                Token::Op(Operator::ClosingCurlyBrace),
            ],
        );
    }

    #[test]
    fn nested_blocks_with_braces() {
        test_tokenize(
            "{ let a = ( 1 + 2 ); { let b = ( 3 * 4 ) } }",
            &[
                Token::Op(Operator::OpeningCurlyBrace),
                Token::kw(Keyword::let_()),
                Token::var("a"),
                Token::Op(Operator::Assign),
                Token::Op(Operator::OpeningBracket),
                Token::literal(1.into()),
                Token::Op(Operator::Add),
                Token::literal(2.into()),
                Token::Op(Operator::ClosingBracket),
                Token::Semicolon(),
                Token::Op(Operator::OpeningCurlyBrace),
                Token::kw(Keyword::let_()),
                Token::var("b"),
                Token::Op(Operator::Assign),
                Token::Op(Operator::OpeningBracket),
                Token::literal(3.into()),
                Token::Op(Operator::Mul),
                Token::literal(4.into()),
                Token::Op(Operator::ClosingBracket),
                Token::Op(Operator::ClosingCurlyBrace),
                Token::Op(Operator::ClosingCurlyBrace),
            ],
        );
    }
}
