// extern crate wasm_bindgen;
// extern crate serde_json;
use wasm_bindgen::prelude::*;
// extern crate console_error_panic_hook;

// use wasm_bindgen::prelude::*;
use web_sys::console;
mod counter_state;

use serde_derive::{Serialize, Deserialize};

  // use wasm_bindgen::prelude::*;
  
#[derive(Serialize, Deserialize, Debug)]
pub struct File {
  pub name: String,
  pub content: String,
}


#[wasm_bindgen]
pub fn test( js_objects: &JsValue )-> String {
  let elements: Vec<File> = js_objects.into_serde().unwrap();
  let debug_text = format!("{:?}", elements);
  debug_text
}

#[wasm_bindgen]
pub fn test2( js_objects: &JsValue )-> String {
  let files: Vec<File> = js_objects.into_serde().unwrap();
  let files: Vec<vba_interpreter_lib::prepare::File> =
    files.iter().map(|f|
      vba_interpreter_lib::prepare::File {
        base_name: f.name.clone(),
        extension: "bas".to_string(),
        content: f.content.clone() }
    ).collect();
  let (s, _) = vba_interpreter_lib::prepare::load_and_run(files.iter().map(|f| f).collect());
  s
}


// When the `wee_alloc` feature is enabled, this uses `wee_alloc` as the global
// allocator.
//
// If you don't want to use `wee_alloc`, you can safely delete this.
#[cfg(feature = "wee_alloc")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;


// This is like the `main` function, except for JavaScript.
#[wasm_bindgen(start)]
pub fn main_js() -> Result<(), JsValue> {
    // This provides better error messages in debug mode.
    // It's disabled in release mode so it doesn't bloat up the file size.
    #[cfg(debug_assertions)]
    console_error_panic_hook::set_once();


    // Your code goes here!
    console::log_1(&JsValue::from_str("Hello world!"));

    Ok(())
}
