//! Processes: `ps` (`processes`), `kill`, and the `Process`, `Pid` and `Signal` classes (Scala
//! `ProcessesFunction`, `KillFunction`, `ProcessClass`, `PidClass`, `SignalClass`).

use mash_macros::{builtin, builtin_method};

use crate::complete::CompletionSpec;
use crate::eval::{Ctx, MashError, R, render};
use crate::function::{BuiltinMethod, FromValue, Param, Params};
use crate::infer::{Type, TypedArgs};
use crate::platform::{Platform, ProcessInfo, SIGNAL_NAMES, Signal};
use crate::value::{BuiltinClass, ClassRef, Field, List, Number, Object, Str, Value};

use super::super::constructor::FieldsConstructor;
use super::super::core::{ANY_CLASS, BYTES_CLASS, OBJECT_CLASS};
use super::USERNAME_CLASS;
use super::lifted::{InfoMethod, LiftedField, LiftedMethod};
use super::paths::describe;

pub static PROCESS_CLASS: BuiltinClass = BuiltinClass {
    summary: "A process",
    fields: &[
        Field {
            name: "pid",
            summary: "Id of process",
            ty: || Type::tagged_number(&PID_CLASS),
        },
        Field {
            name: "parentPid",
            summary: "Id of parent process",
            ty: || Type::tagged_number(&PID_CLASS),
        },
        Field {
            name: "name",
            summary: "Name",
            ty: || Type::STRING,
        },
        Field {
            name: "command",
            summary: "Command",
            ty: || Type::STRING,
        },
        Field {
            name: "owner",
            summary: "Owner",
            ty: || Type::tagged_string(&USERNAME_CLASS),
        },
        Field {
            name: "residentSize",
            summary: "Resident set size (RSS) in bytes",
            ty: || Type::tagged_number(&BYTES_CLASS),
        },
        Field {
            name: "virtualSize",
            summary: "Virtual memory size (VSZ) in bytes",
            ty: || Type::tagged_number(&BYTES_CLASS),
        },
    ],
    methods: &[&ChildrenMethod, &ProcessKillMethod, &ParentMethod],
    static_methods: &[&PROCESS_NEW],
    ..BuiltinClass::new("os", "Process", Some(&OBJECT_CLASS))
};

pub static PID_CLASS: BuiltinClass = BuiltinClass {
    summary: "Tag class for process ID (PID)",
    methods: PID_METHODS,
    enumerate: Some(pids),
    ..BuiltinClass::new("os", "Pid", Some(&ANY_CLASS))
};

pub static SIGNAL_CLASS: BuiltinClass = BuiltinClass {
    summary: "A signal",
    enumeration: SIGNAL_NAMES,
    ..BuiltinClass::new("os", "Signal", Some(&ANY_CLASS))
};

static PROCESS_NEW: FieldsConstructor =
    FieldsConstructor::new("os.Process", &PROCESS_CLASS, "Construct a new Process object");

/// Methods of `os.Pid`: `info`, then `Process`'s methods and fields
static PID_METHODS: &[&dyn BuiltinMethod] = &[
    &InfoMethod {
        class: &PROCESS_CLASS,
        summary: "Get information about the process with this pid",
        resolve: process_by_pid,
    },
    &lifted(&ChildrenMethod),
    &lifted(&ProcessKillMethod),
    &lifted(&ParentMethod),
    &field("pid"),
    &field("parentPid"),
    &field("name"),
    &field("command"),
    &field("owner"),
    &field("residentSize"),
    &field("virtualSize"),
];

const fn lifted(method: &'static dyn BuiltinMethod) -> LiftedMethod {
    LiftedMethod {
        class: &PROCESS_CLASS,
        method,
        resolve: process_by_pid,
    }
}

const fn field(name: &'static str) -> LiftedField {
    LiftedField {
        class: &PROCESS_CLASS,
        name,
        resolve: process_by_pid,
    }
}

// ------------------------------------------------------------------------------------------------ data

fn all_processes(platform: &dyn Platform) -> R<Vec<ProcessInfo>> {
    platform
        .processes()
        .map_err(|e| MashError::new(format!("Could not list processes: {}", describe(&e))))
}

/// The ids of running processes (the values of `os.Pid`, for completion)
fn pids(platform: &dyn Platform) -> Vec<String> {
    platform
        .processes()
        .unwrap_or_default()
        .iter()
        .map(|p| p.pid.to_string())
        .collect()
}

fn pid_value(pid: u32) -> Value {
    Value::Number(Number::new(f64::from(pid)), Some(&PID_CLASS))
}

fn bytes(n: u64) -> Value {
    Value::Number(Number::new(n as f64), Some(&BYTES_CLASS))
}

/// An `os.Process` object
fn process_object(info: &ProcessInfo) -> Value {
    let object = Object::with_class(Some(ClassRef::Builtin(&PROCESS_CLASS)));
    object.set_str("pid", pid_value(info.pid));
    object.set_str("parentPid", info.parent_pid.map_or(Value::Null, pid_value));
    object.set_str("name", Value::str(&info.name));
    object.set_str("command", Value::str(&info.command));
    let owner = info
        .owner
        .as_deref()
        .map(|o| Value::Str(Str::tagged(o, &USERNAME_CLASS)));
    object.set_str("owner", owner.unwrap_or(Value::Null));
    object.set_str("residentSize", bytes(info.resident_size));
    object.set_str("virtualSize", bytes(info.virtual_size));
    Value::Object(object)
}

/// A process id: a number, or a `Process` object's `pid`
fn pid_of(value: &Value) -> Option<u32> {
    let number = match value {
        Value::Object(o) if o.class().is_some_and(|c| c.is_builtin(&PROCESS_CLASS)) => o.get_str("pid")?,
        other => other.clone(),
    };
    number.as_number()?.as_i64().and_then(|n| u32::try_from(n).ok())
}

fn target_pid(target: &Value) -> R<u32> {
    pid_of(target).ok_or_else(|| MashError::new(format!("Invalid pid: {}", render(target))))
}

/// The `Process` for a pid
fn process_by_pid(ctx: &mut Ctx, target: &Value) -> R<Value> {
    let pid = target_pid(target)?;
    let info = ctx
        .platform()
        .process(pid)
        .map_err(|e| MashError::new(format!("Could not list processes: {}", describe(&e))))?
        .ok_or_else(|| MashError::new(format!("Cannot find process with pid {pid}")))?;
    Ok(process_object(&info))
}

impl FromValue for Signal {
    fn from_value(value: &Value) -> Result<Self, String> {
        let invalid = || format!("Invalid signal: {}", render(value));
        match value {
            Value::Number(n, _) => n
                .as_i64()
                .and_then(|n| i32::try_from(n).ok())
                .map(Signal::Number)
                .ok_or_else(invalid),
            Value::Str(s) if SIGNAL_NAMES.contains(&s.as_str()) => Ok(Signal::Named(s.as_str().to_string())),
            _ => Err(invalid()),
        }
    }
}

fn send_signal(ctx: &Ctx, pid: u32, signal: &Signal) -> R<()> {
    ctx.platform()
        .kill(pid, signal)
        .map_err(|e| MashError::new(format!("Unable to kill process with pid {pid}: {}", describe(&e))))
}

const SIGNAL_DESCRIPTION: &str = "The signal can either be given as a number, or by name (e.g. \"KILL\", \"TERM\", \
                                  \"HUP\" etc).\nThe default signal is TERM.";

fn term() -> Value {
    Value::Str(Str::tagged("TERM", &SIGNAL_CLASS))
}

/// `kill`'s `--signal`
fn signal_flag_param() -> Param {
    Param::named("signal")
        .summary("Signal to send to the process")
        .description(SIGNAL_DESCRIPTION)
        .default_value(term)
        .flag()
        .mandatory_flag_value()
        .flag_value_name("signal")
}

/// `Process.kill`'s signal
fn signal_param() -> Param {
    Param::named("signal")
        .summary("Signal to send to the process")
        .description(SIGNAL_DESCRIPTION)
        .default_value(term)
}

fn complete_signal(params: &Params, arg_pos: usize, args: &TypedArgs) -> Vec<CompletionSpec> {
    match args.bind(params).param_at(params, arg_pos) {
        Some("signal") => vec![signal_items()],
        _ => vec![],
    }
}

fn complete_method_signal(params: &Params, arg_pos: usize, _: Option<&Type>, args: &TypedArgs) -> Vec<CompletionSpec> {
    complete_signal(params, arg_pos, args)
}

fn signal_items() -> CompletionSpec {
    CompletionSpec::Items(SIGNAL_NAMES.iter().map(|s| s.to_string()).collect())
}

// ------------------------------------------------------------------------------------------------ functions

/// List processes
#[builtin(namespace = "os", returns = Type::seq(Type::Instance(&PROCESS_CLASS)))]
pub(crate) fn processes(ctx: &mut Ctx) -> R<List> {
    Ok(List::new(
        all_processes(ctx.platform())?.iter().map(process_object).collect(),
    ))
}

/// Send a signal to a process
///
/// Examples:
/// <mash>
///   ps | grep "java" | kill   # Kill all Java processes
///   kill --signal="HUP" 1280  # Send the HUP signal to process with PID 1280
/// </mash>
#[builtin(namespace = "os", returns = Type::UNIT, completion = complete_signal)]
pub(crate) fn kill(
    ctx: &mut Ctx,
    #[param(def = signal_flag_param)] signal: Signal,
    #[param(
        "Processes to kill",
        description = "Processes can be provided either as numeric PIDs or Process objects.",
        variadic,
        at_least_one,
        flatten
    )]
    processes: List,
) -> R<()> {
    let mut pids = vec![];
    for process in processes.snapshot() {
        pids.push(pid_of(&process).ok_or_else(|| MashError::new(format!("Invalid process ID: {}", render(&process))))?);
    }
    for pid in pids {
        send_signal(ctx, pid, &signal)?;
    }
    Ok(())
}

// ------------------------------------------------------------------------------------------------ methods

/// Kill this process
///
/// Examples:
/// <mash>
///   process.kill "HUP" # Send the HUP signal to the process
/// </mash>
#[builtin_method(name = "kill", returns = Type::UNIT, completion = complete_method_signal)]
fn process_kill(ctx: &mut Ctx, target: &Value, #[param(def = signal_param)] signal: Signal) -> R<()> {
    send_signal(ctx, target_pid(target)?, &signal)
}

/// The parent process
#[builtin_method(returns = Type::Instance(&PROCESS_CLASS))]
fn parent(ctx: &mut Ctx, target: &Value) -> R<Value> {
    let parent_pid = match target {
        Value::Object(o) => o.get_str("parentPid").as_ref().and_then(pid_of),
        _ => None,
    };
    let Some(parent_pid) = parent_pid else {
        return Ok(Value::Null);
    };
    let parent = ctx
        .platform()
        .process(parent_pid)
        .map_err(|e| MashError::new(format!("Could not list processes: {}", describe(&e))))?;
    Ok(parent.as_ref().map_or(Value::Null, process_object))
}

/// Children of this process
#[builtin_method(returns = Type::seq(Type::Instance(&PROCESS_CLASS)))]
fn children(ctx: &mut Ctx, target: &Value) -> R<List> {
    let pid = target_pid(target)?;
    Ok(List::new(
        all_processes(ctx.platform())?
            .iter()
            .filter(|p| p.parent_pid == Some(pid))
            .map(process_object)
            .collect(),
    ))
}
