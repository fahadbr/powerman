extern crate powerman;

use powerman::*;
use anyhow::Result;

pub fn main() -> Result<()> {
    run::<Commands, RealSource>()
}
