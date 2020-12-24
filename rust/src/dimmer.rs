use anyhow::Result;
use std::{
    fs::{self, File},
    io::{prelude::*, BufReader},
    path::Path,
    thread,
    time::Duration,
};

pub struct Config {
    file: Files,
    target: Targets,
}

impl Config {
    pub fn default() -> Self {
        let fps = 60;
        let delay = 1000 as u64 / 60 as u64;
        Self {
            file: Files {
                last_b: "/home/fahad/.local/lastbrightness",
                current_b: "/sys/class/backlight/intel_backlight/brightness",
            },
            target: Targets {
                dim_duration: 300,
                undim_duration: 300,
                dim_brightness: 100,
                fps,
                delay,
            },
        }
    }
}

struct Targets {
    dim_duration: usize,
    undim_duration: usize,
    fps: usize,
    dim_brightness: usize,
    delay: u64,
}

struct Files {
    last_b: &'static str,
    current_b: &'static str,
}

pub fn dim(cfg: Config) -> Result<()> {
    let last_b_file = Path::new(cfg.file.last_b);
    if last_b_file.exists() {
        return Ok(());
    }

    let current_b_val = read_b(Path::new(cfg.file.current_b))?;
    transition(
        &cfg,
        current_b_val,
        cfg.target.dim_brightness,
        cfg.target.dim_duration,
    )?;
    write_b(last_b_file, current_b_val)
}

pub fn restore(cfg: Config) -> Result<()> {
    let last_b_file = Path::new(cfg.file.last_b);
    if !last_b_file.exists() {
        return Ok(());
    }

    let last_b_path = Path::new(cfg.file.last_b);
    let last_b_val = read_b(last_b_path)?;
    let current_b_val = read_b(Path::new(cfg.file.current_b))?;
    transition(&cfg, current_b_val, last_b_val, cfg.target.undim_duration)?;

    Ok(fs::remove_file(last_b_path)?)
}

type Itr<T> = Box<dyn Iterator<Item = T>>;

fn transition(cfg: &Config, current_b: usize, target_b: usize, target_dur: usize) -> Result<()> {
    let inc = (target_b as isize - current_b as isize).abs() as usize
        / ((cfg.target.fps * target_dur) / 1000);
    let rng: Itr<usize> = if target_b > current_b {
        Box::new((current_b..=target_b).step_by(inc))
    } else {
        Box::new((target_b..=current_b).rev().step_by(inc))
    };

    let current_b_path = Path::new(cfg.file.current_b);
    let mut file = File::create(current_b_path)?;
    for i in rng {
        write!(&mut file, "{}", i)?;
        thread::sleep(Duration::from_millis(cfg.target.delay));
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
