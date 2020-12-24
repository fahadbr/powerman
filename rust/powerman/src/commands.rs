use std::{thread::sleep, time::Duration};

use crate::{check_cmd_output, dimmer};
use anyhow::Result;

pub trait Actions {
    fn dim() -> Result<()>;
    fn restore() -> Result<()>;
    fn lock() -> Result<()>;
    fn suspend() -> Result<()>;
    fn delay(d: Duration);
}

pub struct Commands {}

impl Actions for Commands {
    fn dim() -> Result<()> {
        dimmer::dim(dimmer::Config::default())
    }

    fn restore() -> Result<()> {
        dimmer::restore(dimmer::Config::default())
    }

    fn lock() -> Result<()> {
        check_cmd_output(&["lock.sh"], "failed to lock", |_| Ok(()))
    }

    fn suspend() -> Result<()> {
        check_cmd_output(&["systemctl", "suspend"], "failed to suspend", |_| Ok(()))
    }

    fn delay(d: Duration) {
        sleep(d)
    }
}
