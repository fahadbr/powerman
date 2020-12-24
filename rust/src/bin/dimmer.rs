extern crate powerman;

use anyhow::Result;
use powerman::dimmer::*;
use std::env;

pub fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();
    let cfg = Config::default();
    match &args[1][..] {
        "dim" => dim(cfg),
        "restore" => restore(cfg),
        _ => panic!(format!("couldnt match arg {}", args[1])),
    }
}
