use std::{thread::sleep, time::Duration};

use crate::{check_cmd_output, config::Config, dimmer};
use anyhow::Result;

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
        println!("running monitor standby");
        check_cmd_output(
            &["xset", "dpms", "force", "standby"],
            "failed to set monitor to standby",
            |_| Ok(()),
        )
    }

    fn lock(cfg: &Config) -> Result<()> {
        println!("running lock");
        check_cmd_output(&[&cfg.lock_cmd], "failed to lock", |_| Ok(()))
    }

    fn suspend(_cfg: &Config) -> Result<()> {
        println!("running suspend");
        check_cmd_output(&["systemctl", "suspend"], "failed to suspend", |_| Ok(()))
    }

    fn delay(d: Duration) {
        sleep(d)
    }
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
