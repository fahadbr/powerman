extern crate powerman;

use std::env;

use powerman::*;
use anyhow::{Result, bail};

pub fn main() -> Result<()> {
    env_logger::init();
    let args: Vec<String> = env::args().collect();
    if args.is_empty() {
        bail!("expected at least one argument");
    }

    run::<Commands, RealSource>(&args[1])
}
