use crate::token::Token;

pub fn tokenize<'a>(input: &'a str) -> Vec<Token<'a>> {
    Lexer::tokenize(input)
}

struct Lexer<'input> {
    input: &'input [u8],
}

type Return<'input> = Option<(Token<'input>, usize)>;

impl<'i> Lexer<'i> {
    fn tokenize(input: &'i str) -> Vec<Token<'i>> {
        let mut lexer = Self {
            input: input.as_bytes(),
        };
        let mut tokens = Vec::new();

        lexer.skip_whitespace();

        while let Some((token, by)) = lexer.next_token() {
            lexer.advance(by);
            tokens.push(token.clone());

            match token {
                Token::Eof | Token::Illegal(_) => break,
                _ => {}
            }
            lexer.skip_whitespace();
        }
        tokens
    }

    fn advance(&mut self, by: usize) {
        self.input = &self.input[by..];
    }

    fn next_token(&self) -> Return<'i> {
        let lst = [
            Lexer::eof,
            Lexer::parse_symbols,
            Lexer::parse_keywords,
            Lexer::parse_identifier,
            Lexer::parse_int,
        ];

        for l in lst {
            if let Some(a) = l(self) {
                return Some(a);
            }
        }

        Some((Token::Illegal(self.input[0]), 1))
    }

    fn eof(&self) -> Return<'i> {
        if self.input.len() <= 0 {
            Some((Token::Eof, 0))
        } else {
            None
        }
    }

    fn parse_identifier(&self) -> Return<'i> {
        self.take_while(Lexer::is_letter).map(|s| {
            let l = s.len();
            let ident = std::str::from_utf8(s).expect("Unable to unwrap ascii string");

            (Token::Ident(ident), l)
        })
    }

    fn parse_symbols(&self) -> Return<'i> {
        match self.input {
            &[b'=', b'=', ..] => Some((Token::Eq, 2)),
            &[b'!', b'=', ..] => Some((Token::Neq, 2)),
            &[b'!', ..] => Some((Token::Bang, 1)),
            &[b'=', ..] => Some((Token::Assign, 1)),
            &[b';', ..] => Some((Token::Semicolon, 1)),
            &[b'(', ..] => Some((Token::LParen, 1)),
            &[b')', ..] => Some((Token::RParen, 1)),
            &[b',', ..] => Some((Token::Comma, 1)),
            &[b'+', ..] => Some((Token::Plus, 1)),
            &[b'-', ..] => Some((Token::Minus, 1)),
            &[b'*', ..] => Some((Token::Asterisk, 1)),
            &[b'/', ..] => Some((Token::Slash, 1)),
            &[b'<', ..] => Some((Token::Lt, 1)),
            &[b'>', ..] => Some((Token::Gt, 1)),
            &[b'{', ..] => Some((Token::LBrace, 1)),
            &[b'}', ..] => Some((Token::RBrace, 1)),
            _ => None,
        }
    }

    fn parse_keywords(&self) -> Return<'i> {
        let keyword = self.take_while(Lexer::is_letter).map(|s| {
            std::str::from_utf8(s).expect("Unable to unwrap ascii string for the keywords")
        })?;

        let k = match keyword {
            "fn" => Token::Fun,
            "let" => Token::Let,
            "false" => Token::False,
            "true" => Token::True,
            "return" => Token::Return,
            "if" => Token::If,
            "else" => Token::Else,
            _ => return None,
        };

        Some((k, keyword.len()))
    }

    fn parse_int(&self) -> Return<'i> {
        let blk = self.take_while(Lexer::is_digit)?;
        let blk_in =
            std::str::from_utf8(blk).expect("Unable to unwrap ascii string for the keywords");

        Some((
            Token::Int(i64::from_str_radix(blk_in, 10).expect("Umable to convert correctly")),
            blk.len(),
        ))
    }

    fn skip_whitespace(&mut self) {
        let is_whitespace = |c: &&_| [b' ', b'\n', b'\t', b'\r'].contains(c);

        let it = self.input.iter().take_while(is_whitespace);
        let len = it.count();

        self.advance(len);
    }

    fn take_while<P>(&self, p: P) -> Option<&'i [u8]>
    where
        P: Fn(&u8) -> bool,
    {
        let it = self.input.iter().take_while(|&c| p(c));
        let len = it.count();

        if len == 0 {
            return None;
        }

        let (ident, _) = self.input.split_at(len);

        Some(ident)
    }

    fn is_digit(c: &u8) -> bool {
        c.is_ascii_digit()
    }

    fn is_letter(c: &u8) -> bool {
        c.is_ascii_alphabetic() || *c == b'_'
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn eq(input: &str, exp: &[Token]) {
        let got = tokenize(input);
        for (ref e, ref g) in std::iter::zip(exp, &got) {
            assert_eq!(e, g);
        }
    }

    #[test]
    fn test_next_token_simple() {
        let input = "=+(){},;";
        let exp = [
            Token::Assign,
            Token::Plus,
            Token::LParen,
            Token::RParen,
            Token::LBrace,
            Token::RBrace,
            Token::Comma,
            Token::Semicolon,
            Token::Eof,
        ];
        eq(input, &exp);
    }

    #[test]
    fn test_next_token_more_tokens() {
        let input = "let five = 5;
                     let ten = 10;
                     let add = fn(x, y) { x + y; }; 
                     let result = add(five, ten);";

        let exp = [
            Token::Let,
            Token::Ident("five"),
            Token::Assign,
            Token::Int(5),
            Token::Semicolon,
            Token::Let,
            Token::Ident("ten"),
            Token::Assign,
            Token::Int(10),
            Token::Semicolon,
            Token::Let,
            Token::Ident("add"),
            Token::Assign,
            Token::Fun,
            Token::LParen,
            Token::Ident("x"),
            Token::Comma,
            Token::Ident("y"),
            Token::RParen,
            Token::LBrace,
            Token::Ident("x"),
            Token::Plus,
            Token::Ident("y"),
            Token::Semicolon,
            Token::RBrace,
            Token::Semicolon,
            Token::Let,
            Token::Ident("result"),
            Token::Assign,
            Token::Ident("add"),
            Token::LParen,
            Token::Ident("five"),
            Token::Comma,
            Token::Ident("ten"),
            Token::RParen,
            Token::Semicolon,
            Token::Eof,
        ];

        eq(input, &exp);
    }

    #[test]
    fn test_next_token_complet() {
        let input = "
            let five = 5;
            let ten = 10;
            let add = fn(x, y) { x + y; };
            let result = add(five, ten); 
            !-/*5;
            5 < 10 > 5;
        ";

        let exp = [
            Token::Let,
            Token::Ident("five"),
            Token::Assign,
            Token::Int(5),
            Token::Semicolon,
            Token::Let,
            Token::Ident("ten"),
            Token::Assign,
            Token::Int(10),
            Token::Semicolon,
            Token::Let,
            Token::Ident("add"),
            Token::Assign,
            Token::Fun,
            Token::LParen,
            Token::Ident("x"),
            Token::Comma,
            Token::Ident("y"),
            Token::RParen,
            Token::LBrace,
            Token::Ident("x"),
            Token::Plus,
            Token::Ident("y"),
            Token::Semicolon,
            Token::RBrace,
            Token::Semicolon,
            Token::Let,
            Token::Ident("result"),
            Token::Assign,
            Token::Ident("add"),
            Token::LParen,
            Token::Ident("five"),
            Token::Comma,
            Token::Ident("ten"),
            Token::RParen,
            Token::Semicolon,
            Token::Bang,
            Token::Minus,
            Token::Slash,
            Token::Asterisk,
            Token::Int(5),
            Token::Semicolon,
            Token::Int(5),
            Token::Lt,
            Token::Int(10),
            Token::Gt,
            Token::Int(5),
            Token::Semicolon,
            Token::Eof,
        ];

        eq(input, &exp);
    }
}
