pub mod calc;

use crate::calc::expr_eval;

use std::fs::File;
use std::io::prelude::*;
use std::io::Write;

fn eval(expr: &str) -> Result<(), String> {
    let value = match expr_eval(&expr) {
        Ok(value) => value,
        _ => return Err("構文エラー".to_string()),
    };
    println!("{:?}", value);

    Ok(())
}

fn interpreter() -> Result<(), String> {
    // インタプリタモード
    print!("> ");
    std::io::stdout().flush().unwrap();

    let mut s = String::new();
    std::io::stdin().read_line(&mut s).ok();

    eval(&s)
}

fn read_file(filename: &str) -> Result<(), String> {
    // ファイル読み込み→実行モード
    match File::open(filename) {
        Ok(mut value) => {
            let mut contents = String::new();
            match value.read_to_string(&mut contents) {
                Ok(_) => eval(&contents.as_str()),
                // ファイルの読み込み中に問題がありました
                Err(e) => Err(e.to_string()),
            }
        }
        // ファイルが見つかりませんでした
        Err(e) => Err(e.to_string()),
    }
}

fn command_parse() -> Result<(), String> {
    // コマンド引数を取得
    let args: Vec<String> = std::env::args().collect();
    if args.len() > 1 {
        let mode = args[1].as_str();

        match mode {
            "-i" => loop {
                let _ = match interpreter() {
                    Ok(()) => (),
                    Err(e) => println!("!ERROR!: {}", e),
                };
            },
            _ => {
                println!("ファイル内実行モード");

                let _ = match read_file(mode) {
                    Ok(()) => (),
                    Err(e) => println!("!ERROR!: {}", e),
                };
            }
        }
    } else {
        println!("kanini ver 0.1");
    }
    Ok(())
}

fn main() {
    // スタックオーバーフローの際の原因特定
    unsafe { backtrace_on_stack_overflow::enable() };

    // TODO: エラーの際の実装
    let _ = command_parse();
}
