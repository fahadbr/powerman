pub mod dimmer;
pub mod config;
mod commands;
mod sources;

use crate::{commands::*, sources::*};
use anyhow::{anyhow, Context, Result};
use std::{
    process::{Command, Output},
    time,
};

pub use commands::Commands;
pub use sources::RealSource;

pub fn run<A: Actions, S: Source>() -> Result<()> {
    check_cmd_exists("xprintidle")?;
    check_cmd_exists("pacmd")?;

    let sleep_time = time::Duration::from_secs(2);
    loop {
        let idle_time = S::get_idle_time()?;
        println!("idle for {}", idle_time);
        A::delay(sleep_time);
    }
}

fn check_cmd_exists(name: &'static str) -> Result<()> {
    match Command::new("which").arg(name).status() {
        Ok(status) if status.success() => Ok(()),
        Ok(_) => Err(anyhow!("command not found: {}", name)),
        Err(e) => Err(e).context(name),
    }
}

fn check_cmd_output<T, F>(cmdargs: &[&str], context: &'static str, f: F) -> Result<T>
where
    F: Fn(Output) -> Result<T>,
{
    assert!(!cmdargs.is_empty());

    let cmd = cmdargs[0];
    let args = cmdargs.split_at(1).1;

    match Command::new(cmd).args(args).output() {
        Ok(o) if o.status.success() => f(o),
        Ok(o) => Err(anyhow!(
            "nonzero exit code - stdout: {}, stderr: {}",
            String::from_utf8(o.stdout).unwrap(),
            String::from_utf8(o.stderr).unwrap()
        )),
        Err(e) => Err(e).context(context),
    }
}
