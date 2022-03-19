use std::env;
use vba_interpreter_lib::prepare::{load_and_run, File};
  
fn main() {
  // 最初に読み込んだファイルのmain関数をエントリポイントとする。
  let args: Vec<String> = env::args().skip(1).collect();
  let args: Vec<&String> = args.iter().map(|arg| arg).collect();
  
  for arg in args.iter() {
    println!("input file: {}", arg);
  }
  
  let files: Vec<File> =
    args.iter().map(|arg|{
      let content = std::fs::read_to_string(arg)
        .expect("Something went wrong reading the file");
      let base_name = arg.split("/").last().unwrap().split(".").next().unwrap();
      let extension = arg.split(".").last().unwrap();
      File {
        content: content,
        base_name: base_name.to_string(),
        extension: extension.to_string()
      }
    }).collect();
  
  let result = load_and_run(files.iter().map(|f| f).collect());
  
  println!("result: {:?}", result);
}
