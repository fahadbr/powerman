mod commands;
pub mod config;
pub mod dimmer;
mod sources;

use crate::{commands::*, config::Config, sources::*};
use anyhow::{anyhow, Context, Result};
use log::trace;
use std::{
    env,
    path::Path,
    process::{Command, Output},
    time::{self, Duration},
};

pub use commands::Commands;
pub use sources::RealSource;

pub fn run<A: Actions, S: Source>(profile: &str) -> Result<()> {
    const CFG_VAR: &str = "POWERMAN_CONFIG";

    check_cmd_exists("xprintidle")?;
    check_cmd_exists("pacmd")?;

    let cfg = Config::new(Path::new(&env::var(CFG_VAR).context(CFG_VAR)?))?;

    let timeouts = cfg
        .timeouts
        .get(profile)
        .ok_or_else(|| anyhow!("no timeouts defined for {}", profile))?;

    let mut handlers: Vec<_> = vec![
        ActionWrapper::new(timeouts.dim, A::dim, Some(A::restore)),
        ActionWrapper::new(timeouts.dpms, A::monitor_standby, None),
        ActionWrapper::new(timeouts.lock, A::lock, None),
        ActionWrapper::new(timeouts.sleep, A::suspend, None),
    ]
    .into_iter()
    .filter(|a| a.timeout != Duration::default())
    .collect();

    let mut audio_idle_time = Duration::default();
    let sleep_time = time::Duration::from_millis(250);

    loop {
        let idle_time = S::get_idle_time()?;

        audio_idle_time = if S::audio_running()? {
            // if audio is running, freeze idle_time by setting
            // audio_idle_time equal to idle_time
            idle_time
        } else if idle_time > audio_idle_time{
            // if audio isnt running and we're still idle
            // return the last audio_idle_time so that it seems
            // like we only went idle after the audio stopped playing
            audio_idle_time
        } else {
            // otherwise reset the audio_idle_time
            Duration::default()
        };

        let effective_idle_time = idle_time - audio_idle_time;

        trace!("effectivly idle for {:?}", effective_idle_time);
        for handler in handlers.iter_mut() {
            handler.handle(&cfg, effective_idle_time)?;
        }

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
