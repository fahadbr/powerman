use crate::check_cmd_output;
use anyhow::Result;

pub trait Source {
    fn get_idle_time() -> Result<usize>;
    fn audio_running() -> Result<bool>;
}

pub struct RealSource {}

impl Source for RealSource {
    fn get_idle_time() -> Result<usize> {
        check_cmd_output(&["xprintidle"], "failed to get idle time", |o| {
            let s = String::from_utf8(o.stdout)?;
            let i: usize = s.trim().parse()?;
            Ok(i)
        })
    }

    fn audio_running() -> Result<bool> {
        todo!()
    }
}
