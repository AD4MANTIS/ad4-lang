use std::{collections::HashMap, str::FromStr};

use color_eyre::owo_colors::OwoColorize;

use crate::Value;

use super::*;

fn parse_test_expression(input: &str, expected: &str) -> Expression {
    let expr = match Expression::from_str(input) {
        Ok(expr) => expr,
        Err(e) => {
            panic!(
                "{}\n{input}\n{}: {e}",
                "Failed to parse expression:".bright_red(),
                "Error".on_bright_red()
            );
        }
    };

    assert_eq!(expr.to_string(), expected, "Input: {input}");

    expr
}

fn test_expr(input: &str, expected: &str, result: Option<Value>) {
    let expr = parse_test_expression(input, expected);

    if let Some(result) = result {
        let variables = &mut [("a", 1i64.into()), ("b", 2i64.into()), ("c", 3i64.into())]
            .into_iter()
            .map(|x| (Variable::new(x.0.to_string()), x.1))
            .collect::<HashMap<_, _>>();

        if let Ok(eval_result) = expr.eval(variables) {
            assert_eq!(eval_result, result);
        }
    }
}

macro_rules! expr_cases {
    ($($name:ident: $input:expr => $expected:expr $(=> $result:expr )?),* $(,)?) => {
        use super::*;

        $(
            #[test]
            fn $name() {
                #[allow(unused_variables)]
                let result: Option<Value> = None;

                $(
                    let result = Some($result.into());
                )?

                test_expr($input, $expected, result);
            }
        )*
    };
}

mod literals {
    expr_cases! {
        number: "1" => "1" => 1,
        char: "'c'" => "'c'" => 'c',
        char_whitespace: "' '" => "' '" => ' ',
        string: r#""Hi""# => r#""Hi""# => "Hi".to_string(),
        string_with_spaces: r#""Hello, World!""# => r#""Hello, World!""# => "Hello, World!".to_string(),
        negativ_number: "-1" => "(- 0 1)" => -1,
        signed_number: "12i" => "12" => 12,
        unsigned_number: "123u" => "123" => 123,
        double: "1.23" => "1.23" => 1.23,
        explicit_float: "32.1f" => "32.1" => 32.1f32,
        explicit_double: "4.99d" => "4.99" => 4.99,
        float_no_fractions: "1f" => "1" => 1f32,
        double_no_fraction_implicit: "1." => "1" => 1.0,
        boolean_true: "true" => "true" => true,
        boolean_false: "false" => "false" => false,
        vector: "[1]" => "[1]" => vec![1.into()],
        vector_2: "[1, 2, 3]" => r"[
    1,
    2,
    3,
]" => vec![1.into(), 2.into(), 3.into()],
        vector_trailing_comma: "[1,]" => "[1]" => vec![1.into()],
    }
}

mod math {
    expr_cases! {
        precedence_mul_before_add: "1+2 * 3" => "(+ 1 (* 2 3))" => 7,
        left_associative_mul: "a * 2 * b" => "(* (* a 2) b)" => 4,
        mixed_precedence_and_div: "a + b * 2 * c + a/4" => "(+ (+ a (* (* b 2) c)) (/ a 4))" => 13,
        complex_chain: "2 + b * 5 - 3 / 5 + 5 - 3" => "(- (+ (- (+ 2 (* b 5)) (/ 3 5)) 5) 3)" => 14,
        parens_affect_precedence: "(2 + b) * 5" => "(* (+ 2 b) 5)" => 20,
        nested_parens: "(( ( a )) )" => "a" => 1,
        parens_with_mul_and_add: "a + b * 2 * ( c + a ) / 4" => "(+ a (/ (* (* b 2) (+ c a)) 4))" => 5,
        add_floats: "1.5 + 4.0" => "(+ 1.5 4)" => 5.5,
        prefix_sub: "-5 + -5" => "(+ (- 0 5) (- 0 5))" => -10,
    }
}

mod variables {
    expr_cases! {
        variable_x: "a" => "a" => 1,
        add_two_vars: "a + b" => "(+ a b)" => 3,
        mul_then_add: "a * b + c" => "(+ (* a b) c)" => 5,
        left_associative_add: "1 + 2 + 3" => "(+ (+ 1 2) 3)" => 6,
        left_associative_mul_numbers: "1 * 2 * 3" => "(* (* 1 2) 3)" => 6,
        parens_around_number: "((1))" => "1" => 1,
        add_with_parens_right: "a + (b + c)" => "(+ a (+ b c))" => 6,
        mul_with_parens_both: "(a + b) * (c + d)" => "(* (+ a b) (+ c d))" => 7,
        div_then_mul_zero: "a / b * c" => "(* (/ a b) c)" => 0,
        div_then_mul: "16 / 4 * 2" => "(* (/ 16 4) 2)" => 8,
        long_add_chain: "a + b + c + d" => "(+ (+ (+ a b) c) d)" => 6,

        assign_variable: "p = 3.1" => "(= p 3.1)" => 3.1,
        assign_from_expr: "x = a == 2" => "(= x (== a 2))" => false,
    }
}

mod dot {
    expr_cases! {
        dot_operator: "a.b" => "(. a b)",
        chained_dot: "a.b.c.d" => "(. (. (. a b) c) d)",
        spaced_dots: "a . b. c" => "(. (. a b) c)",
    }
}

mod equality {
    expr_cases! {
        basic_eq: "1 == 1" => "(== 1 1)" => true,
        boolean_neq: "true != false" => "(!= true false)" => true,
        number_eq: "1 == 3" => "(== 1 3)" => false,
        sub_expr_eq: "(1 + 2) * 3 == 10 - 1" => "(== (* (+ 1 2) 3) (- 10 1))" => true,
    }
}

mod compare {
    expr_cases! {
        basic_lt: "1 < 2" => "(< 1 2)" => true,
        number_lt: "1 < -3" => "(< 1 (- 0 3))" => false,
        ge: "1 >= 1" => "(>= 1 1)" => true,
        sub_expr_lt: "(1 + 2) * 3 < 10 - 1" => "(< (* (+ 1 2) 3) (- 10 1))" => false,
    }
}

mod blocks {
    expr_cases! {
        curly: "{ 1 }" => "{ 1 }" => 1,
        curly_no_space: "{1+2} + 3" => "(+ { (+ 1 2) } 3)" => 6,
        multiple_statements: "{
            let x = 1;
            let y = 2;
            x + y
    }" => r"{
    let x = 1;
    let y = 2;
    (+ x y)
}" => 3,
        multiple_expressions: "{
            let x = 1;
            x = x + 1;
            x + x
}" => r"{
    let x = 1;
    (= x (+ x 1));
    (+ x x)
}" => 4,
        empty_block: "{}" => "{ }" => (),
    }

    #[test]
    fn lt_eq_with_space() {
        Expression::from_str("1 < = 2").expect_err("`< =` should not be valid");
    }
}

mod if_else {
    expr_cases! {
        r#if: "if true { 1 }" => "if true { 1 }" => 1,
        not_if_void: "if 1 == 2 { 1 }" => "if (== 1 2) { 1 }" => (),
        if_else: "if true { 1 } else { 2 }" => "if true { 1 }\nelse { 2 }" => 1,
        if_else_if: "if false { 1 } else if false { 2 } else { 3 }" => "if false { 1 }\nelse if false { 2 }\nelse { 3 }" => 3,
        if_else_nested: "if true { if false { 1 } else { 2 } } else { 3 }" => "if true { if false { 1 }\nelse { 2 } }\nelse { 3 }" => 2,
    }

    #[test]
    fn variable_definition_in_conditional_block() -> Result<(), Box<dyn std::error::Error>> {
        // TODO: Indenting multiple levels
        let expr = parse_test_expression(
            r#"{
    let a = 2.2;
    if a == 1.1 {
        let b = "Hi";
        b
    }
}"#,
            r#"{
    let a = 2.2;
    if (== a 1.1) {
    let b = "Hi";
    b
}
}"#,
        );

        let mut vars = HashMap::new();

        let result = expr.eval(&mut vars);

        assert_eq!(result?, Value::Void);
        assert!(vars.contains_key(&Variable::new("a".to_string())));
        assert!(!vars.contains_key(&Variable::new("b".to_string())));

        Ok(())
    }
}

mod loops {
    expr_cases! {
        normal_while: "while true { }" => "while true { }",
        iterating_while: "while a < 5 { a = a + 1 }" => "while (< a 5) { (= a (+ a 1)) }",
        fibonacci_while: r"{
            let a = 0;
            let b = 1;

            while b < 50 {
                let temp = a + b;
                a = b;
                b = temp;
            }

            b
}" => r"{
    let a = 0;
    let b = 1;
    while (< b 50) {
    let temp = (+ a b);
    (= a b);
    (= b temp);
};
    b
}" => 55,
        // todo
        // while_with_result: "while a < 10 { if a == 5 { break 42; } }" => "while (< a 10) {
        //     if (== a 5) {
        //     break 42;
        //     }
        // }"
    }
}
