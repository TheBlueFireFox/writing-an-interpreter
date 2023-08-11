use std::io::{BufRead, Write};

use crate::parser::parse;

const PROMT: &str = ">> ";


fn promt() {
    print!("{PROMT}");
    let _ = std::io::stdout().flush();
}

pub fn start() -> ! {
    let mut line = String::new();
    let stdin = std::io::stdin();
    loop {
        promt();

        line.clear();
        let _ = stdin.lock().read_line(&mut line);
        let tokens = parse(&line);
        match tokens {
            Ok(tokens) => {
                for token in tokens.statements {
                    println!("{}", token);
                }
            }
            Err(errors) => {
                for err in errors {
                    println!("{}", err);
                }
            }
        }
    }
}
