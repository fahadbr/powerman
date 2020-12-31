extern crate powerman;

use std::env;

use powerman::*;
use anyhow::{Result, bail};

pub fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.is_empty() {
        bail!("expected at least one argument");
    }

    run::<Commands, RealSource>(&args[1])
}
