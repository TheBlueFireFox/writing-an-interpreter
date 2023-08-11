use crate::{
    ast::{Expression, Program, Statement, BE},
    lexer::tokenize,
    token::Token,
};

pub fn parse(input: &str) -> Result<Program, Vec<String>> {
    let tokens = tokenize(input);
    Parser::new(&tokens)
        .parse()
        .map(|s| Program { statements: s })
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
enum Predecence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
}

impl Predecence {
    fn map(token: &Token) -> Self {
        match token {
            Token::Eq => Predecence::Equals,
            Token::Neq => Predecence::Equals,
            Token::Lt => Predecence::LessGreater,
            Token::Gt => Predecence::LessGreater,
            Token::Plus => Predecence::Sum,
            Token::Minus => Predecence::Sum,
            Token::Slash => Predecence::Product,
            Token::Asterisk => Predecence::Product,
            Token::LParen => Predecence::Call,
            _ => Predecence::Lowest,
        }
    }
}

#[derive(Debug)]
struct Parser<'toks, 'input> {
    tokens: &'toks [Token<'input>],
}

impl<'toks, 'input> Parser<'toks, 'input> {
    fn new(tokens: &'toks [Token<'input>]) -> Self {
        Self { tokens }
    }

    fn parse(&mut self) -> Result<Vec<Statement<'input>>, Vec<String>> {
        let mut res = Vec::new();
        let mut err = Vec::new();

        loop {
            let statement = match self.tokens.get(0) {
                Some(Token::Eof) => break,
                Some(_) => self.parse_statement(),
                None => {
                    err.push("Missing EOF".to_string());
                    break;
                }
            };

            match statement {
                Ok(s) => {
                    res.push(s);
                }
                Err(e) => {
                    err.push(e);
                    self.advance_after_semicolon();
                }
            }
        }

        if err.len() == 0 {
            Ok(res)
        } else {
            Err(err)
        }
    }

    fn parse_statement(&mut self) -> Result<Statement<'input>, String> {
        match self.tokens.get(0) {
            Some(Token::Let) => self.let_statement(),
            Some(Token::Return) => self.return_statement(),
            Some(_) => self.expression_statement(),
            None => Err("Empty token list".to_string()),
        }
    }

    fn advance(&mut self, by: usize) {
        self.tokens = &self.tokens[by..];
    }

    fn advance_after_semicolon(&mut self) {
        loop {
            match self.tokens.get(0) {
                Some(Token::Semicolon) => {
                    self.advance(1);
                    break;
                }
                Some(Token::Eof) => break,
                Some(_) => self.advance(1),
                None => break,
            }
        }
    }

    fn expression_statement(&mut self) -> Result<Statement<'input>, String> {
        let statement = self.parse_expression(Predecence::Lowest)?;

        if let Token::Semicolon = self.tokens[0] {
            self.advance(1);
        }

        Ok(Statement::Expression(statement))
    }

    fn let_statement(&mut self) -> Result<Statement<'input>, String> {
        match self.tokens {
            &[Token::Let, Token::Ident(name), Token::Assign, ..] => {
                self.advance(3);
                let expr = self.parse_expression(Predecence::Lowest)?;
                self.advance_after_semicolon();
                Ok(Statement::Let(name, expr))
            }
            &[Token::Let, Token::Ident(_), ..] => {
                Err("Expected an '=' after the let identifier".to_string())
            }
            &[Token::Let, _, ..] => Err("Expected an identifier after the let keyword".to_string()),
            _ => Err("Unable to parse the let statement, missing let keyword".to_string()),
        }
    }

    fn return_statement(&mut self) -> Result<Statement<'input>, String> {
        // remove return
        self.advance(1);
        let expr = self.parse_expression(Predecence::Lowest)?;
        self.advance_after_semicolon();
        Ok(Statement::Return(expr))
    }

    fn parse_expression(&mut self, pred: Predecence) -> Result<Expression<'input>, String> {
        // TODO:
        let mut left = self.parse_prefix()?;

        loop {
            if let Token::Semicolon | Token::Eof = &self.tokens[0] {
                break;
            }

            let curr_pred = Predecence::map(&self.tokens[0]);
            if pred >= curr_pred {
                break;
            }

            left = self.parse_infix_expression(left)?;
        }

        Ok(left)
    }

    fn parse_prefix(&mut self) -> Result<Expression<'input>, String> {
        match self.tokens.get(0) {
            Some(&Token::Ident(ident)) => self.parse_ident(ident),
            Some(&Token::Int(val)) => self.parse_int(val),
            Some(Token::Bang) => self.parse_not(),
            Some(Token::Minus) => self.parse_neg(),
            Some(Token::False) => self.parse_bool(false),
            Some(Token::True) => self.parse_bool(true),
            Some(Token::If) => self.parse_if(),
            Some(Token::Fun) => self.parse_fn(),
            Some(Token::LParen) => self.parse_grouped(),
            Some(token) => Err(format!("Illegal token <{:?}> is not a prefix", token)),
            None => Err("No toke for a prefix found".to_string()),
        }
    }

    fn parse_infix_expression(
        &mut self,
        left: Expression<'input>,
    ) -> Result<Expression<'input>, String> {
        type Runner<'a> = dyn Fn(BE<'a>, BE<'a>) -> Expression<'a>;
        let runner: &Runner = match self.tokens[0] {
            Token::Plus => &|l, r| Expression::Add(l, r),
            Token::Minus => &|l, r| Expression::Min(l, r),
            Token::Slash => &|l, r| Expression::Div(l, r),
            Token::Asterisk => &|l, r| Expression::Mul(l, r),
            Token::Eq => &|l, r| Expression::Eq(l, r),
            Token::Neq => &|l, r| Expression::Neq(l, r),
            Token::Lt => &|l, r| Expression::Le(l, r),
            Token::Gt => &|l, r| Expression::Gt(l, r),
            Token::LParen => {
                self.advance(1);
                return self.parse_call(left);
            }
            _ => return Ok(left),
        };

        let curr = &self.tokens[0];
        self.advance(1);

        let right = self.parse_expression(Predecence::map(curr))?;

        Ok(runner(Box::new(left), Box::new(right)))
    }

    fn parse_grouped(&mut self) -> Result<Expression<'input>, String> {
        self.advance(1);
        let expr = self.parse_expression(Predecence::Lowest)?;

        if let Token::RParen = self.tokens[0] {
            self.advance(1);
            return Ok(expr);
        }
        Err("Missing Right Paren )".to_string())
    }

    fn parse_if(&mut self) -> Result<Expression<'input>, String> {
        self.advance(1);

        let curr = &self.tokens[0];
        self.advance(1);
        if Token::LParen != *curr {
            return Err(format!("Invalid token expected an '(' but got {:?}", curr));
        }
        let cond = self.parse_expression(Predecence::Lowest)?;
        let cons = match self.tokens {
            &[Token::RParen, Token::LBrace, ..] => {
                self.advance(2);
                self.parse_block()
            }
            &[Token::RParen, ..] => Err("Missing {".to_string()),
            _ => Err("Missing )".to_string()),
        }?;

        let alt = if let Some(Token::Else) = self.tokens.get(0) {
            if let Some(Token::LBrace) = self.tokens.get(1) {
                self.advance(2);
                Some(Box::new(self.parse_block()?))
            } else {
                return Err("Missing LBrace {".to_string());
            }
        } else {
            None
        };

        Ok(Expression::If(Box::new(cond), Box::new(cons), alt))
    }

    fn parse_block(&mut self) -> Result<Statement<'input>, String> {
        let mut statements = Vec::new();

        loop {
            match self.tokens.get(0) {
                Some(Token::RBrace) => {
                    self.advance(1);
                    break;
                }
                Some(Token::Eof) => return Err("Missing RPrace".to_string()),
                None => return Err("Missing EOF".to_string()),
                Some(_) => {
                    let blk = self.parse_statement()?;
                    statements.push(blk);
                }
            }
        }

        Ok(Statement::Block(statements))
    }

    fn parse_fn(&mut self) -> Result<Expression<'input>, String> {
        self.advance(1);
        if Token::LParen != self.tokens[0] {
            return Err("Err missing LParen".to_string());
        }
        self.advance(1);
        let params = self.parse_fn_params()?;

        if Token::LBrace != self.tokens[0] {
            return Err("Err missing LBrace".to_string());
        }
        self.advance(1);

        let body = self.parse_block()?;

        Ok(Expression::Fn(params, Box::new(body)))
    }

    fn parse_fn_params(&mut self) -> Result<Vec<Expression<'input>>, String> {
        let mut res = Vec::new();

        if Token::RParen == self.tokens[0] {
            self.advance(1);
            return Ok(res);
        }

        let ident = |v: &Token<'input>| {
            if let Token::Ident(v) = *v {
                Ok(Expression::Ident(v))
            } else {
                Err(format!("Incorrect token type expected ident {:?}", v))
            }
        };

        res.push(ident(&self.tokens[0])?);
        self.advance(1);

        while let Token::Comma = self.tokens[0] {
            res.push(ident(&self.tokens[1])?);
            self.advance(2);
        }

        if Token::RParen != self.tokens[0] {
            return Err("Missing RParen".to_string());
        }
        self.advance(1);

        Ok(res)
    }

    fn parse_call(&mut self, left: Expression<'input>) -> Result<Expression<'input>, String> {
        let mut args = Vec::new();

        args.push(self.parse_expression(Predecence::Lowest)?);

        while let Token::Comma = self.tokens[0] {
            self.advance(1);
            args.push(self.parse_expression(Predecence::Lowest)?);
        }

        if Token::RParen != self.tokens[0] {
            return Err("Missing RParen".to_string());
        }
        self.advance(1);

        Ok(Expression::Call(Box::new(left), args))
    }

    fn parse_ident(&mut self, ident: &'input str) -> Result<Expression<'input>, String> {
        self.advance(1);
        Ok(Expression::Ident(ident))
    }

    fn parse_int(&mut self, val: i64) -> Result<Expression<'input>, String> {
        self.advance(1);
        Ok(Expression::Int(val))
    }

    fn parse_bool(&mut self, val: bool) -> Result<Expression<'input>, String> {
        self.advance(1);
        Ok(Expression::Bool(val))
    }

    fn parse_not(&mut self) -> Result<Expression<'input>, String> {
        self.advance(1);
        let inner = self.parse_expression(Predecence::Prefix)?;
        Ok(Expression::Not(Box::new(inner)))
    }

    fn parse_neg(&mut self) -> Result<Expression<'input>, String> {
        self.advance(1);
        let inner = self.parse_expression(Predecence::Prefix)?;
        Ok(Expression::Neg(Box::new(inner)))
    }
}

#[cfg(test)]
mod tests {
    use std::vec;

    use super::*;
    use crate::ast::{Expression, Statement, BE};

    fn eq(input: &str, exp: &[Statement<'_>]) {
        let got = parse(input);

        match got {
            Ok(got) => {
                for (ref e, ref g) in std::iter::zip(exp, &got.statements) {
                    assert_eq!(e, g);
                }
            }
            Err(errs) => {
                for err in errs {
                    eprintln!("{}", err);
                }
                assert!(false);
            }
        }
    }

    #[test]
    fn test_let_statement() {
        let input = "
            let x = 5; 
            let y = true;
            let foobar = y;
            ";

        // TODO: replace Expression::Invalid with correct variant
        let exp = [
            Statement::Let("x", Expression::Int(5)),
            Statement::Let("y", Expression::Bool(true)),
            Statement::Let("foobar", Expression::Ident("y")),
        ];

        eq(input, &exp)
    }

    #[test]
    fn test_return_statement() {
        let input = "
                return 5;
                return false;
                return y;
                ";
        let exp = [
            Statement::Return(Expression::Int(5)),
            Statement::Return(Expression::Bool(false)),
            Statement::Return(Expression::Ident("y")),
        ];

        eq(input, &exp);
    }

    #[test]
    fn test_infix_expressions() {
        let input = "foobar;";
        let exp = [Statement::Expression(Expression::Ident("foobar"))];
        eq(input, &exp);
    }

    #[test]
    fn test_int_expressions() {
        let input = "5;";
        let exp = [Statement::Expression(Expression::Int(5))];
        eq(input, &exp);
    }

    #[test]
    fn test_prefix_int_with_semi() {
        let input = ["!5;", "-15;"];
        let exp = [
            [Statement::Expression(Expression::Not(Box::new(
                Expression::Int(5),
            )))],
            [Statement::Expression(Expression::Neg(Box::new(
                Expression::Int(15),
            )))],
        ];

        for (i, e) in std::iter::zip(&input, &exp) {
            eq(i, e)
        }
    }

    #[test]
    fn test_prefix_int_no_semi() {
        let input = ["!5", "-15"];
        let exp = [
            [Statement::Expression(Expression::Not(Box::new(
                Expression::Int(5),
            )))],
            [Statement::Expression(Expression::Neg(Box::new(
                Expression::Int(15),
            )))],
        ];

        for (i, e) in std::iter::zip(&input, &exp) {
            eq(i, e)
        }
    }

    #[test]
    fn test_parsing_bool() {
        type Runner<'a> = dyn Fn(BE<'a>, BE<'a>) -> Expression<'a>;
        let exp: &[&Runner] = &[
            &|l, r| Expression::Eq(l, r),
            &|l, r| Expression::Neq(l, r),
            &|l, r| Expression::Neq(l, r),
            &|l, r| Expression::Eq(l, r),
        ];

        let input = [
            ("true == true", true, true),
            ("true != false;", true, false),
            ("false != true;", false, true),
            ("false == false", false, false),
        ];

        let make = |v| Box::new(Expression::Bool(v));
        for (&(input, l, r), fun) in std::iter::zip(&input, exp) {
            let exp = fun(make(l), make(r));
            let exp = Statement::Expression(exp);
            eq(input, &[exp]);
        }
    }

    #[test]
    fn test_parsing_infix_expression() {
        type Expr<'a> = dyn Fn(BE<'a>, BE<'a>) -> Expression<'a>;
        let cons: &[(&str, &Expr)] = &[
            ("5 + 5;", &|l, r| Expression::Add(l, r)),
            ("5 - 5;", &|l, r| Expression::Min(l, r)),
            ("5 * 5;", &|l, r| Expression::Mul(l, r)),
            ("5 / 5;", &|l, r| Expression::Div(l, r)),
            ("5 > 5;", &|l, r| Expression::Gt(l, r)),
            ("5 < 5;", &|l, r| Expression::Le(l, r)),
            ("5 == 5;", &|l, r| Expression::Eq(l, r)),
            ("5 != 5;", &|l, r| Expression::Neq(l, r)),
        ];
        let five = Box::new(Expression::Int(5));
        for (input, gen) in cons {
            let exp = gen(five.clone(), five.clone());
            let exp = Statement::Expression(exp);
            eq(input, &[exp]);
        }
    }

    #[test]
    fn test_operator_pred() {
        let lst = [
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            ("true", "true"),
            ("false", "false"),
            ("3 > 5 == false", "((3 > 5) == false)"),
            ("3 < 5 == true", "((3 < 5) == true)"),
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("-(5 + 5)", "(-(5 + 5))"),
            ("!(true == true)", "(!(true == true))"),
            ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
            (
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
            ),
            (
                "add(a + b + c * d / f + g)",
                "add((((a + b) + ((c * d) / f)) + g))",
            ),
        ];

        for (input, exp) in lst {
            assert_eq!(exp, format!("{}", parse(input).unwrap()));
        }
    }

    #[test]
    fn test_if() {
        let input = "if (x < y) { x }";
        let blk = |v| {
            Box::new(Statement::Block(vec![Statement::Expression(
                Expression::Ident(v),
            )]))
        };
        let ident = |v| Box::new(Expression::Ident(v));
        let cond = |l, r| Box::new(Expression::Le(l, r));
        let exp = [Statement::Expression(Expression::If(
            cond(ident("x"), ident("y")),
            blk("x"),
            None,
        ))];
        eq(input, &exp);
    }

    #[test]
    fn test_if_else() {
        let input = "if (x < y) { x } else { y }";
        let blk = |v| {
            Box::new(Statement::Block(vec![Statement::Expression(
                Expression::Ident(v),
            )]))
        };
        let ident = |v| Box::new(Expression::Ident(v));
        let cond = |l, r| Box::new(Expression::Le(l, r));
        let exp = [Statement::Expression(Expression::If(
            cond(ident("x"), ident("y")),
            blk("x"),
            Some(blk("y")),
        ))];
        eq(input, &exp);
    }

    #[test]
    fn test_fn_lit() {
        let input = "fn(x, y) { x + y; }";

        let ident = |v| Expression::Ident(v);
        let bident = |v| Box::new(ident(v));
        let blk = |v| Box::new(Statement::Block(vec![Statement::Expression(v)]));
        let add = |l, r| Expression::Add(l, r);

        let exp = [Statement::Expression(Expression::Fn(
            vec![ident("x"), ident("y")],
            blk(add(bident("x"), bident("y"))),
        ))];

        eq(input, &exp);
    }

    #[test]
    fn test_fn_lit_complex() {
        let input = ["fn() {};", "fn(x) {};", "fn(x, y) {};", "fn(x, y, z) {};"];

        let ident = |v| Expression::Ident(v);
        let blk = || Box::new(Statement::Block(vec![]));

        let exp = [
            [Statement::Expression(Expression::Fn(vec![], blk()))],
            [Statement::Expression(Expression::Fn(
                vec![ident("x")],
                blk(),
            ))],
            [Statement::Expression(Expression::Fn(
                vec![ident("x"), ident("y")],
                blk(),
            ))],
            [Statement::Expression(Expression::Fn(
                vec![ident("x"), ident("y"), ident("z")],
                blk(),
            ))],
        ];

        for (input, exp) in std::iter::zip(&input, &exp) {
            eq(input, exp);
        }
    }

    #[test]
    fn test_call_expression_parsing() {
        let input = "add(1, 2 * 3, 4 + 5);";

        let int = |v| Expression::Int(v);
        let bint = |v| Box::new(int(v));
        let ident = |v| Box::new(Expression::Ident(v));
        let add = |l, r| Expression::Add(l, r);
        let mul = |l, r| Expression::Mul(l, r);

        let exp = [Statement::Expression(Expression::Call(
            ident("add"),
            vec![int(1), mul(bint(2), bint(3)), add(bint(4), bint(5))],
        ))];

        eq(input, &exp);
    }
}
