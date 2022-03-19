// use std::env;
// use std::error::Error;
// use std::process::Command;

fn main() {
  // let grammars = vec!["vba"];
  // let additional_args = vec![Some("-visitor"), None, None, None, None];
  // // This should be set by some config file
  // let path = env::current_dir().unwrap().join("grammars").join("antlr4-4.8-2-SNAPSHOT-complete.jar");
  // let antlr_path = path.to_str().unwrap();

  // for (grammar, arg) in grammars.into_iter().zip(additional_args) {
  //   // ignoring error because we do not need to run anything when deploying to crates.io
  //   let _ = gen_for_grammar(grammar, antlr_path, arg);
  // }

  // println!("cargo:rerun-if-changed=build.rs");
  // println!("cargo:rerun-if-changed={}", antlr_path.to_owned());
}

// fn gen_for_grammar(
//   grammar_file_name: &str,
//   antlr_path: &str,
//   additional_arg: Option<&str>,
// ) -> Result<(), Box<dyn Error>> {
//   // let out_dir = env::var("OUT_DIR").unwrap();
//   // let dest_path = Path::new(&out_dir);

//   let input = env::current_dir().unwrap().join("grammars");
//   let file_name = grammar_file_name.to_owned() + ".g4";

//   println!("cargo:rerun-if-changed=grammars/{}", file_name);
  
//   let output = Command::new("java")
//     .current_dir(input)
//     .arg("-jar")
//     .arg(antlr_path)
//     .arg("-Dlanguage=Rust")
//     .arg("-o")
//     .arg("../src/gen")
//     .arg(&file_name)
//     .args(additional_arg)
//     .spawn()
//     .expect("antlr tool failed to start")
//     .wait_with_output()?;
  
//   if !output.status.success() {
//     eprintln!("antlr: {}",String::from_utf8(output.stderr).unwrap());
//     panic!("antlr error");
//   } else {
//     Ok(())
//   }
// }
