//! The `yaml` namespace: converting between Mash values and YAML text and files (through `yaml-rust2`, YAML 1.2).

use mash_macros::builtin;
use yaml_rust2::yaml::Hash;
use yaml_rust2::{Yaml, YamlEmitter, YamlLoader};

use super::json::to_json;
use super::os::paths::{PathArg, fs, io_error};
use super::os::{complete_files, read_text};
use crate::eval::{Ctx, MashError, R};
use crate::function::BuiltinFunction;
use crate::infer::Type;
use crate::value::{Object, Str, Value};

pub(crate) const FUNCTIONS: &[&dyn BuiltinFunction] =
    &[&FromStringFunction, &ReadFunction, &ToStringFunction, &WriteFunction];

/// Read the given string and parse it as YAML
///
/// See yaml.read for how YAML becomes Mash values.
#[builtin(namespace = "yaml", returns = Type::ANY)]
pub fn from_string(#[param("String to parse as YAML")] string: Str) -> R<Value> {
    parse_yaml(string.as_str())
}

/// Read the given file and parse it as YAML
///
/// YAML 1.2 is followed, so yes, no, on and off are strings, not booleans (as YAML 1.1 had them). A file with
/// several documents (separated by ---) gives a list of them; with one, that document. Anchors, aliases and
/// merge keys (<<: *anchor) are resolved; custom tags (as !Ref) are dropped, keeping their values. Keys become
/// strings, and dates stay strings. Comments are lost, so writing back what was read loses them.
///
/// Examples:
/// <mash>
///   yaml.read "docker-compose.yml" | .services.fields.name
///   yaml.read "deployment.yaml" | .spec.replicas
/// </mash>
#[builtin(namespace = "yaml", returns = Type::ANY, completion = complete_files)]
pub fn read(ctx: &mut Ctx, #[param("File from which to read YAML")] file: PathArg) -> R<Value> {
    parse_yaml(&read_text(ctx, &file.0)?)
}

/// Convert the given value to a YAML string
///
/// Values are converted as for JSON (see json.prettyPrint): values that aren't YAML types are written as
/// strings. Strings that would read back as something else (true, no, 123) are quoted.
#[builtin(namespace = "yaml", name = "toString", returns = Type::STRING)]
pub fn to_string(ctx: &mut Ctx, #[param("Value to convert to a YAML string")] value: Value) -> R<String> {
    yaml_string(ctx, &value)
}

/// Write the given value as YAML to the given file
///
/// See yaml.toString for how values are written.
#[builtin(namespace = "yaml", returns = Type::UNIT, completion = complete_files)]
pub fn write(
    ctx: &mut Ctx,
    #[param("File to write YAML to")] file: PathArg,
    #[param("Value to write")] value: Value,
) -> R<()> {
    let text = yaml_string(ctx, &value)?;
    ctx.platform()
        .write_file(fs(&file.0), text.as_bytes(), false)
        .map_err(|e| io_error("write", &file.0, &e))
}

/// Parse YAML text: one document as it is, several as a list (none as null)
fn parse_yaml(text: &str) -> R<Value> {
    let documents = YamlLoader::load_from_str(text).map_err(|e| MashError::new(format!("Invalid YAML: {e}")))?;
    Ok(match documents.as_slice() {
        [] => Value::Null,
        [document] => from_yaml(document),
        documents => Value::list(documents.iter().map(from_yaml).collect()),
    })
}

fn from_yaml(yaml: &Yaml) -> Value {
    match yaml {
        Yaml::Null | Yaml::BadValue | Yaml::Alias(_) => Value::Null,
        Yaml::Boolean(b) => Value::Bool(*b),
        Yaml::Integer(i) => Value::num(*i as f64),
        Yaml::Real(text) => Value::num(yaml.as_f64().unwrap_or_else(|| text.parse().unwrap_or(f64::NAN))),
        Yaml::String(s) => Value::str(s),
        Yaml::Array(items) => Value::list(items.iter().map(from_yaml).collect()),
        Yaml::Hash(hash) => {
            let object = Object::new();
            merge_into(&object, hash);
            Value::Object(object)
        }
    }
}

/// The fields of a mapping, into `object`, in order. A merge key (`<<`) brings in, where it is, the fields of a
/// mapping, or of each of a list of them (earlier ones first), except those the mapping has itself.
fn merge_into(object: &Object, hash: &Hash) {
    let merge = Yaml::String("<<".into());
    let own: Vec<String> = hash.keys().filter(|k| **k != merge).map(key_string).collect();
    for (key, value) in hash {
        if *key != merge {
            object.set_str(&key_string(key), from_yaml(value));
            continue;
        }
        let sources: Vec<&Hash> = match value {
            Yaml::Hash(source) => vec![source],
            Yaml::Array(items) => items.iter().filter_map(Yaml::as_hash).collect(),
            _ => vec![],
        };
        for source in sources {
            let merged = Object::new();
            merge_into(&merged, source);
            for (key, value) in merged.entries() {
                let mine = matches!(&key, Value::Str(k) if own.iter().any(|o| o == k.as_str()));
                if !mine && object.get(&key).is_none() {
                    object.set(key, value);
                }
            }
        }
    }
}

/// A key as a string (as JSON and other tools have them)
fn key_string(key: &Yaml) -> String {
    match key {
        Yaml::String(s) | Yaml::Real(s) => s.clone(),
        Yaml::Integer(i) => i.to_string(),
        Yaml::Boolean(b) => b.to_string(),
        Yaml::Null => "null".into(),
        other => {
            let mut text = String::new();
            let _ = YamlEmitter::new(&mut text).dump(other);
            text.trim_start_matches("---").trim().to_string()
        }
    }
}

/// A value as YAML text: converted as for JSON, then written in block style, ending with a newline
fn yaml_string(ctx: &mut Ctx, value: &Value) -> R<String> {
    let yaml = to_yaml(&to_json(ctx, value)?);
    let mut text = String::new();
    YamlEmitter::new(&mut text)
        .dump(&yaml)
        .map_err(|e| MashError::new(format!("Could not write YAML: {e}")))?;
    // The emitter starts with a document marker, which a single document doesn't need
    let text = text
        .strip_prefix("---")
        .unwrap_or(&text)
        .trim_start_matches([' ', '\n']);
    Ok(format!("{text}\n"))
}

fn to_yaml(json: &serde_json::Value) -> Yaml {
    match json {
        serde_json::Value::Null => Yaml::Null,
        serde_json::Value::Bool(b) => Yaml::Boolean(*b),
        serde_json::Value::Number(n) => match n.as_i64() {
            Some(i) => Yaml::Integer(i),
            None => Yaml::Real(n.to_string()),
        },
        serde_json::Value::String(s) => Yaml::String(s.clone()),
        serde_json::Value::Array(items) => Yaml::Array(items.iter().map(to_yaml).collect()),
        serde_json::Value::Object(fields) => Yaml::Hash(
            fields
                .iter()
                .map(|(k, v)| (Yaml::String(k.clone()), to_yaml(v)))
                .collect(),
        ),
    }
}
