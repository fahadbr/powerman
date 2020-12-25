use anyhow::Result;
use fs::remove_file;
use std::{
    fs::{self, File},
    io::{prelude::*, BufReader},
    path::Path,
    thread::sleep,
    time::Duration,
};

use crate::config::DimmerConfig;

pub fn dim(cfg: DimmerConfig) -> Result<()> {
    let last_b_file = Path::new(&cfg.files.last_b);
    if last_b_file.exists() {
        return Ok(());
    }

    let current_b_val = read_b(Path::new(&cfg.files.current_b))?;
    transition(&cfg, current_b_val, cfg.targets.dimmed_b, cfg.targets.dim_dur)?;
    write_b(last_b_file, current_b_val)
}

pub fn restore(cfg: DimmerConfig) -> Result<()> {
    let last_b_file = Path::new(&cfg.files.last_b);
    if !last_b_file.exists() {
        return Ok(());
    }

    let last_b_path = Path::new(&cfg.files.last_b);
    let last_b_val = read_b(last_b_path)?;
    let current_b_val = read_b(Path::new(&cfg.files.current_b))?;
    transition(&cfg, current_b_val, last_b_val, cfg.targets.undim_dur)?;

    Ok(remove_file(last_b_path)?)
}

type Itr<T> = Box<dyn Iterator<Item = T>>;

fn transition(cfg: &DimmerConfig, current_b: usize, target_b: usize, target_dur: usize) -> Result<()> {
    let brightness_diff = (target_b as isize - current_b as isize).abs() as usize;
    let inc = brightness_diff / ((cfg.targets.fps * target_dur) / 1000);

    let rng: Itr<usize> = if target_b > current_b {
        Box::new((current_b..=target_b).step_by(inc))
    } else {
        Box::new((target_b..=current_b).rev().step_by(inc))
    };

    let current_b_path = Path::new(&cfg.files.current_b);
    let mut file = File::create(current_b_path)?;
    for i in rng {
        write!(&mut file, "{}", i)?;
        sleep(Duration::from_millis(cfg.targets.delay));
    }

    Ok(())
}

fn read_b(path: &Path) -> Result<usize> {
    let file = File::open(path)?;
    let mut reader = BufReader::new(file);

    let mut buf = String::new();
    reader.read_to_string(&mut buf)?;

    let res = buf.trim_end().parse::<usize>()?;
    Ok(res)
}

fn write_b(path: &Path, value: usize) -> Result<()> {
    let mut file = File::create(path)?;
    Ok(write!(&mut file, "{}", value)?)
}
