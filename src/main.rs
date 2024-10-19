pub mod calc;

use crate::calc::expr_eval;

use std::io::Write;

fn interpreter() -> Result<(), String> {
    // インタプリタモード
    print!("> ");
    std::io::stdout().flush().unwrap();

    let mut s = String::new();
    std::io::stdin().read_line(&mut s).ok();
    let value = match expr_eval(&s) {
        Ok(value) => value,
        _ => return Err("構文エラー".to_string()),
    };
    println!("{:?}", value);

    Ok(())
    // -----------------
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
                return Err("ファイルが見つかりません".to_string());
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
