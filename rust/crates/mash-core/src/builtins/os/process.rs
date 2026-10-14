//! Running external programs: `run`, the `ProcessResult` class, and the helpers the evaluator uses for mish
//! (`!{…}`) and system command functions (`!ls`) (Scala `ProcessRunner`, `RunFunction`,
//! `ProcessResultClass`).

use mash_macros::{builtin, builtin_method};

use crate::builtins::collections::split_into_lines;
use crate::builtins::time::MILLISECONDS_CLASS;
use crate::eval::{Ctx, MashError, R};
use crate::function::Param;
use crate::infer::Type;
use crate::platform::{ProcessInput, ProcessOutcome, ProcessOutput, ProcessRequest};
use crate::value::{ClassRef, List, Number, Object, Str, Value};

use super::PROCESS_RESULT_CLASS;
use super::files::{complete_files, date_time};
use super::paths::{describe, path_value};

/// Where a process's standard input comes from
#[derive(Debug, Clone)]
pub(crate) enum Input {
    Inherit,
    Text(String),
    File(String),
}

/// Where a process's standard output goes
#[derive(Debug, Clone)]
pub(crate) enum Output {
    Inherit,
    Capture,
    File(String),
}

/// The environment for a child process: the contents of Mash's `env` global (as Scala does), or the host's
/// environment if `env` isn't an object
pub(crate) fn child_environment(ctx: &mut Ctx) -> R<Vec<(String, String)>> {
    match ctx.globals().get_str("env") {
        Some(Value::Object(env)) => {
            let mut vars = vec![];
            for (name, value) in env.entries() {
                vars.push((ctx.stringify(&name)?, ctx.stringify(&value)?));
            }
            Ok(vars)
        }
        _ => Ok(ctx.platform().env_vars()),
    }
}

/// Run a program (the first argument) with arguments, converted to strings. A process killed by Ctrl-C
/// interrupts evaluation.
pub(crate) fn run_command(ctx: &mut Ctx, args: &[Value], input: Input, output: Output) -> R<ProcessOutcome> {
    let mut argv = Vec::with_capacity(args.len());
    for arg in args {
        argv.push(ctx.stringify(arg)?);
    }
    if argv.is_empty() {
        return Err(MashError::new("Must provide at least one argument for the command"));
    }
    let env = child_environment(ctx)?;
    let request = ProcessRequest {
        argv: &argv,
        env: &env,
        stdin: match &input {
            Input::Inherit => ProcessInput::Inherit,
            Input::Text(text) => ProcessInput::Text(text),
            Input::File(path) => ProcessInput::File(path),
        },
        stdout: match &output {
            Output::Inherit => ProcessOutput::Inherit,
            Output::Capture => ProcessOutput::Capture,
            Output::File(path) => ProcessOutput::File(path),
        },
    };
    let outcome = ctx
        .platform()
        .run_process(&request)
        .map_err(|e| MashError::new(format!("Could not run '{}': {}", argv[0], describe(&e))))?;
    if outcome.interrupted {
        return Err(MashError::interrupted());
    }
    Ok(outcome)
}

/// Text to send to a process's standard input: a list's items as lines, each ending in a newline (as POSIX
/// tools such as `wc -l` expect), or any other value as a string, as it is
pub(crate) fn stdin_text(ctx: &mut Ctx, value: &Value) -> R<String> {
    match value {
        Value::List(xs) => {
            let mut text = String::new();
            for x in xs.snapshot() {
                text.push_str(&ctx.stringify(&x)?);
                text.push('\n');
            }
            Ok(text)
        }
        x => ctx.stringify(x),
    }
}

/// An `os.ProcessResult` object
pub(crate) fn process_result(outcome: ProcessOutcome) -> Value {
    let object = Object::with_class(Some(ClassRef::Builtin(&PROCESS_RESULT_CLASS)));
    object.set_str("exitStatus", Value::num(outcome.exit_status as f64));
    object.set_str("stdout", Value::Str(Str::new(outcome.stdout)));
    object.set_str("started", date_time(outcome.started));
    object.set_str("finished", date_time(outcome.finished));
    Value::Object(object)
}

/// How `--standardIn` is sent
pub(crate) const STANDARD_IN_DESCRIPTION: &str = "A list is sent one item per line, each line ending in a newline \
    (so [\"a\", \"b\"] sends \"a\\nb\\n\"; an empty list sends nothing). Anything else is sent as a string, as it is.";

pub(crate) fn standard_in_param() -> Param {
    Param::named("standardIn")
        .summary("What to send to the standard input of the process (default nothing)")
        .description(STANDARD_IN_DESCRIPTION)
        .flag()
        .mandatory_flag_value()
        .optional()
}

/// The words of a command given as one string, split at whitespace (as `run` and `Path.run` do)
fn command_words(items: Vec<Value>) -> Vec<Value> {
    match items.as_slice() {
        [Value::List(xs)] => xs.snapshot(),
        [Value::Str(s)] => s.as_str().split_whitespace().map(Value::str).collect(),
        _ => items,
    }
}

fn process_result_type() -> Type {
    Type::Instance(&PROCESS_RESULT_CLASS)
}

/// Run a system command
///
/// The command's output is captured. A list given as `--standardIn` is sent as lines, each ending in a
/// newline.
///
/// Examples:
/// <mash>
///   run "ls -l"                            # a string is split into words
///   run "echo" "two words"                 # one argument per word
///   run --standardIn="text" "wc" "-c"      # send text to the command
///   run --standardIn=["a", "b"] "wc" "-l"  # send lines, "a\nb\n": wc counts 2
/// </mash>
#[builtin(namespace = "os", returns = process_result_type(), completion = complete_files)]
pub(crate) fn run(
    ctx: &mut Ctx,
    #[param(def = standard_in_param)] standard_in: Option<Value>,
    #[param("Command to execute", variadic, at_least_one)] command: List,
) -> R<Value> {
    let args = command_words(command.snapshot());
    let input = match standard_in {
        Some(value) => Input::Text(stdin_text(ctx, &value)?),
        None => Input::Inherit,
    };
    Ok(process_result(run_command(ctx, &args, input, Output::Capture)?))
}

/// Execute the command at the given path, with the given arguments
///
/// Each argument is passed as it is (`"./script".run "two words"` passes one argument). A list given as
/// `--standardIn` is sent as lines, each ending in a newline.
#[builtin_method(name = "run", returns = process_result_type())]
pub(crate) fn path_run(
    ctx: &mut Ctx,
    target: &Value,
    #[param("Arguments to command", variadic)] args: List,
    #[param(def = standard_in_param)] standard_in: Option<Value>,
) -> R<Value> {
    // Scala split a lone argument on whitespace, so its meaning depended on how many there were
    let mut command = vec![target.clone()];
    command.extend(args.snapshot());
    let input = match standard_in {
        Some(value) => Input::Text(stdin_text(ctx, &value)?),
        None => Input::Inherit,
    };
    Ok(process_result(run_command(ctx, &command, input, Output::Capture)?))
}

// ------------------------------------------------------------------------------------------------ methods

fn field(target: &Value, name: &str) -> R<Value> {
    match target {
        Value::Object(o) => o
            .get_str(name)
            .ok_or_else(|| MashError::new(format!("No field '{name}' found in object"))),
        other => Err(MashError::new(format!(
            "target is of type {}, not Object",
            other.type_name()
        ))),
    }
}

fn exit_status(target: &Value) -> R<f64> {
    field(target, "exitStatus")?
        .as_number()
        .map(Number::as_f64)
        .ok_or_else(|| MashError::new("Field 'exitStatus' must be a Number"))
}

fn stdout(target: &Value) -> R<String> {
    match field(target, "stdout")? {
        Value::Str(s) => Ok(s.as_str().to_string()),
        _ => Err(MashError::new("Field 'stdout' must be a String")),
    }
}

/// The first line of standard output, or empty
fn first_line(target: &Value) -> R<String> {
    Ok(split_into_lines(&stdout(target)?)
        .into_iter()
        .next()
        .unwrap_or_default())
}

/// True if the status had a zero exit code
#[builtin_method(returns = Type::BOOLEAN)]
pub(crate) fn succeeded(target: &Value) -> R<bool> {
    Ok(exit_status(target)? == 0.0)
}

/// True if the status had a non-zero exit code
#[builtin_method(returns = Type::BOOLEAN)]
pub(crate) fn failed(target: &Value) -> R<bool> {
    Ok(exit_status(target)? != 0.0)
}

/// The standard output of the process as as sequence of lines
#[builtin_method(aliases = ["toList"], returns = Type::seq(Type::STRING))]
pub(crate) fn lines(target: &Value) -> R<List> {
    Ok(List::new(
        split_into_lines(&stdout(target)?)
            .into_iter()
            .map(Value::from)
            .collect(),
    ))
}

/// The first line of the standard output of the process (or empty, if no output)
#[builtin_method(returns = Type::STRING)]
pub(crate) fn line(target: &Value) -> R<String> {
    first_line(target)
}

/// Parse the stdout of the process as a number
#[builtin_method(returns = Type::NUMBER)]
pub(crate) fn to_number(target: &Value) -> R<Number> {
    let line = first_line(target)?;
    Number::parse(&line).ok_or_else(|| MashError::new(format!("Could not parse '{line}' as a number")))
}

/// Tag the stdout as a path
#[builtin_method(returns = Type::PATH)]
pub(crate) fn to_path(target: &Value) -> R<Value> {
    Ok(path_value(first_line(target)?))
}

/// How long the process ran for, in milliseconds
#[builtin_method(returns = Type::tagged_number(&MILLISECONDS_CLASS))]
pub(crate) fn duration(target: &Value) -> R<Value> {
    let time = |name| match field(target, name)? {
        Value::DateTime(t) => Ok(t.0),
        _ => Err(MashError::new(format!("Field '{name}' must be a DateTime"))),
    };
    let millis = time("finished")?.duration_since(time("started")?).as_millis();
    Ok(Value::Number(Number::new(millis as f64), Some(&MILLISECONDS_CLASS)))
}
