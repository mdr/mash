//! The REPL's settings, read from the `config` global (Scala `ConfigWrapper`): `config.language.bareWords`,
//! `config.cli.showStartupTips`, `config.view.fuzzyTime`, … Missing or mistyped settings take their defaults.

use mash_core::value::{Object, Value};

/// Settings that affect the REPL.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ReplConfig {
    /// `language.bareWords`: unbound identifiers are strings
    pub bare_words: bool,
    /// `cli.showStartupTips`
    pub show_startup_tips: bool,
    /// `view.fuzzyTime`: show times as "3 hours ago"
    pub fuzzy_time: bool,
}

impl Default for ReplConfig {
    fn default() -> Self {
        ReplConfig {
            bare_words: false,
            show_startup_tips: true,
            fuzzy_time: true,
        }
    }
}

impl ReplConfig {
    /// The settings in `globals.config`
    pub fn from_globals(globals: &Object) -> ReplConfig {
        let config = globals.get_str("config");
        let get = |path: &str, default: bool| {
            let mut value = config.clone();
            for segment in path.split('.') {
                value = match value {
                    Some(Value::Object(o)) => o.get_str(segment),
                    _ => None,
                };
            }
            value.map_or(default, |v| v.is_truthy())
        };
        let defaults = ReplConfig::default();
        ReplConfig {
            bare_words: get("language.bareWords", defaults.bare_words),
            show_startup_tips: get("cli.showStartupTips", defaults.show_startup_tips),
            fuzzy_time: get("view.fuzzyTime", defaults.fuzzy_time),
        }
    }
}
