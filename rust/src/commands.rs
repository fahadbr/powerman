use std::{process::Command, thread::sleep, time::Duration};

use crate::{config::Config, dimmer};
use anyhow::{Context, Result};
use log::info;

// originally made this a trait so that i could mock the functions
pub trait Actions {
    fn dim(cfg: &Config) -> Result<()>;
    fn restore(cfg: &Config) -> Result<()>;
    fn monitor_standby(cfg: &Config) -> Result<()>;
    fn lock(cfg: &Config) -> Result<()>;
    fn suspend(cfg: &Config) -> Result<()>;
    fn delay(d: Duration);
}

pub struct Commands {}

impl Actions for Commands {
    fn dim(cfg: &Config) -> Result<()> {
        dimmer::dim(&cfg.dimmer)
    }

    fn restore(cfg: &Config) -> Result<()> {
        dimmer::restore(&cfg.dimmer)
    }

    fn monitor_standby(_cfg: &Config) -> Result<()> {
        spawn_cmd(
            &["xset", "dpms", "force", "standby"],
            "failed to set monitor to standby",
        )
    }

    fn lock(cfg: &Config) -> Result<()> {
        spawn_cmd(&[&cfg.lock_cmd], "failed to lock")
    }

    fn suspend(_cfg: &Config) -> Result<()> {
        spawn_cmd(&["systemctl", "suspend"], "failed to suspend")
    }

    fn delay(d: Duration) {
        sleep(d)
    }
}

fn spawn_cmd(cmdargs: &[&str], context: &'static str) -> Result<()> {
    assert!(!cmdargs.is_empty());
    info!("running cmd: {:?}", cmdargs);

    let cmd = cmdargs[0];
    let args = cmdargs.split_at(1).1;

    Command::new(cmd)
        .args(args)
        .spawn()
        .map(|_| ())
        .context(context)
}

type ActionFn = fn(cfg: &Config) -> Result<()>;

pub struct ActionWrapper {
    pub timeout: Duration,
    active: bool,
    active_action: ActionFn,
    cancel_action: Option<ActionFn>,
}

impl ActionWrapper {
    pub fn new(timeout: u64, active_action: ActionFn, cancel_action: Option<ActionFn>) -> Self {
        Self {
            active: false,
            timeout: Duration::from_secs(timeout),
            active_action,
            cancel_action,
        }
    }

    pub fn handle(&mut self, cfg: &Config, idle_time: Duration) -> Result<()> {
        if self.active && idle_time < self.timeout {
            if let Some(cancel_action) = &self.cancel_action {
                cancel_action(cfg)?;
            }
            self.active = false;
        } else if !self.active && idle_time > self.timeout {
            (self.active_action)(cfg)?;
            self.active = true;
        }

        Ok(())
    }
}
