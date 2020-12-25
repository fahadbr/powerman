use anyhow::Result;
use std::{collections::HashMap, fs::File, io::prelude::*, path::Path};

use serde::Deserialize;

#[derive(Debug, PartialEq, Deserialize)]
pub struct Config {
    pub dimmer: DimmerConfig,
    pub timeouts: HashMap<String, Timeouts>,
}

#[derive(Debug, Clone, PartialEq, Deserialize)]
pub struct Timeouts {
    pub dim: usize,
    pub dpms: usize,
    pub lock: usize,
    pub sleep: usize,
}

#[derive(Debug, PartialEq, Deserialize)]
pub struct DimmerConfig {
    pub files: Files,
    pub targets: Targets,
}

#[derive(Debug, PartialEq, Deserialize)]
pub struct Targets {
    pub dim_dur: usize,
    pub undim_dur: usize,
    pub fps: usize,
    pub dimmed_b: usize,
    pub delay: u64,
}

#[derive(Debug, PartialEq, Deserialize)]
pub struct Files {
    pub last_b: String,
    pub current_b: String,
}

impl Default for DimmerConfig {
    fn default() -> Self {
        let fps = 60usize;
        let delay = 1000u64 / fps as u64;
        Self {
            files: Files {
                last_b: env!("HOME").to_owned() + "/.local/lastbrightness",
                current_b: "/sys/class/backlight/intel_backlight/brightness".to_string(),
            },
            targets: Targets {
                dim_dur: 300,
                undim_dur: 300,
                dimmed_b: 100,
                fps,
                delay,
            },
        }
    }
}

impl Config {
    pub fn new(cfg_path: &Path) -> Result<Self> {
        let mut file = File::open(cfg_path)?;
        let mut buf = String::new();
        file.read_to_string(&mut buf)?;
        Self::from_yaml_str(&buf)
    }

    fn from_yaml_str(yaml_str: &str) -> Result<Self> {
        let cfg: Self = serde_yaml::from_str(yaml_str)?;
        Ok(cfg)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn config_test() {
        let yaml_input = "
---
dimmer:
  files:
    last_b: /path/last_b
    current_b: /path/current_b
  targets:
    dim_dur: 300
    undim_dur: 300
    dimmed_b: 100
    fps: 60
    delay: 16
timeouts:
  ac:
    dim: 60
    dpms: 3600
    lock: 1800
    sleep: 0
  bat:
    dim: 60
    dpms: 1800
    lock: 600
    sleep: 3600
";
        let deserialized_cfg = Config::from_yaml_str(yaml_input).unwrap();
        let expected_cfg = Config {
            dimmer: DimmerConfig {
                files: Files {
                    last_b: String::from("/path/last_b"),
                    current_b: String::from("/path/current_b"),
                },
                targets: Targets {
                    dim_dur: 300,
                    undim_dur: 300,
                    dimmed_b: 100,
                    fps: 60,
                    delay: 16,
                },
            },
            timeouts: [
                (
                    String::from("ac"),
                    Timeouts {
                        dim: 60,
                        dpms: 3600,
                        lock: 1800,
                        sleep: 0,
                    },
                ),
                (
                    String::from("bat"),
                    Timeouts {
                        dim: 60,
                        dpms: 1800,
                        lock: 600,
                        sleep: 3600,
                    },
                ),
            ]
            .iter()
            .cloned()
            .collect(),
        };
        assert_eq!(expected_cfg, deserialized_cfg);
        assert_eq!(
            expected_cfg.timeouts.get("ac"),
            deserialized_cfg.timeouts.get("ac")
        );
    }
}
