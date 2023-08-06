use crate::{
    ast::{Expression, Program, Statement},
    lexer::tokenize,
    token::Token,
};

pub fn parse(input: &str) -> Result<Program, Vec<String>> {
    let tokens = tokenize(input);
    Parser::new(&tokens)
        .parse()
        .map(|s| Program { statements: s })
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
                Some(Token::Let) => self.let_statement(),
                Some(Token::Return) => self.return_statement(),
                Some(Token::Eof) => {
                    self.advance(1);
                    break;
                }
                Some(_) => {
                    todo!()
                }
                None => Err("Missing EOF".to_string()),
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
                Some(_) => self.advance(1),
                None => break,
            }
        }
    }

    fn let_statement(&mut self) -> Result<Statement<'input>, String> {
        match self.tokens {
            &[Token::Let, Token::Ident(name), Token::Assign, ..] => {
                self.advance(3);
                // TODO: skipping expressions for the moment
                self.advance_after_semicolon();
                Ok(Statement::Let(name, Expression::Invalid))
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

        // TODO: skipping expressions for the moment
        self.advance_after_semicolon();
        Ok(Statement::Return(Expression::Invalid))
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{Expression, Statement};

    use super::*;

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
            let y = 10;
            let foobar = 838383;
            ";

        // TODO: replace Expression::Invalid with correct variant
        let exp = [
            Statement::Let("x", Expression::Invalid),
            Statement::Let("y", Expression::Invalid),
            Statement::Let("foobar", Expression::Invalid),
        ];

        eq(input, &exp)
    }

    #[test]
    fn test_return_statement() {
        let input = "
                return 5;
                return 10;
                return 993322;
                ";
        let exp = [
            Statement::Return(Expression::Invalid),
            Statement::Return(Expression::Invalid),
            Statement::Return(Expression::Invalid),
        ];

        eq(input, &exp);
    }
}
