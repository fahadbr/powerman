use std::time::Duration;

use crate::check_cmd_output;
use anyhow::{Context, Result};

pub trait Source {
    fn get_idle_time() -> Result<Duration>;
    fn audio_running() -> Result<bool>;
}

pub struct RealSource {}

impl Source for RealSource {
    fn get_idle_time() -> Result<Duration> {
        check_cmd_output(&["xprintidle"], "failed to get idle time", |o| {
            let s = String::from_utf8(o.stdout)?;
            let i: u64 = s.trim().parse()?;
            Ok(Duration::from_millis(i))
        })
    }

    fn audio_running() -> Result<bool> {
        check_cmd_output(
            &["pacmd", "list-sink-inputs"],
            "failed to get sink outputs",
            |o| {
                String::from_utf8(o.stdout).map(|output| {
                    output
                        .lines()
                        .any(|line| line.contains("state: RUNNING"))
                }).context("converting stdout to utf8")
            },
        )
    }
}
