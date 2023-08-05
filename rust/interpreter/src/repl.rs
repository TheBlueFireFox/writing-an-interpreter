use std::io::{BufRead, Write};

use crate::lexer::tokenize;

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
        let tokens = tokenize(&line);
        for token in tokens {
            println!("{:?}", token);
        }
    }
}
