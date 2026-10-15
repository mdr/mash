//! General-purpose functions in `core`.

use mash_macros::{builtin, builtin_method};

use crate::eval::{Ctx, ErrorKind, MashError, R};
use crate::function::Callable;
use crate::value::{ClassRef, DateTime, Number, Object, Str, Value};

/// Return the argument unchanged
///
/// Examples:
/// <mash>
///   identity 42 # 42
/// </mash>
#[builtin(namespace = "core", infer = super::infer_rules::identity)]
pub fn identity(#[param("Item to return")] item: Value) -> R<Value> {
    Ok(item)
}

/// Logically negate the given item
///
/// The given argument is interpreted as truthy or falsey, then negated.
///
/// Examples:
/// <mash>
///   not true  # false
///   not false # true
///   not 0     # true
///   not 1     # false
/// </mash>
#[builtin(namespace = "core", returns = crate::infer::Type::BOOLEAN)]
pub fn not(#[param("Item to negate")] item: Value) -> R<bool> {
    Ok(!item.is_truthy())
}

/// Check whether or not the given argument is null
///
/// Examples:
/// <mash>
///   isNull null # true
///   isNull 0    # false
/// </mash>
#[builtin(namespace = "core")]
pub fn is_null(#[param("Value to test for nullness")] value: Value) -> R<bool> {
    Ok(value.is_null())
}

/// Print the given argument to standard output
#[builtin(namespace = "core", returns = crate::infer::Type::UNIT)]
pub fn print(ctx: &mut Ctx, #[param("Item to print")] item: Value) -> R<()> {
    let mut text = ctx.stringify(&item)?;
    text.push('\n');
    ctx.platform().write_output(&text);
    Ok(())
}

/// Throw an exception with the given message
#[builtin(namespace = "core")]
pub fn error(#[param("Error message to produce")] message: Str) -> R<Value> {
    Err(MashError::new(message.as_str()))
}

/// Value that represents no argument being provided, for use as sentinel values in default arguments
#[builtin(namespace = "core")]
pub fn no_arg() -> R<Value> {
    Ok(no_arg_value())
}

/// The value returned by `noArg`: an empty object of class `NoArg`
pub(crate) fn no_arg_value() -> Value {
    Value::Object(Object::with_class(Some(ClassRef::Builtin(&super::NO_ARG_CLASS))))
}

/// Parse the given string as a number
///
/// Examples:
/// <mash>
///   parseNumber "42" # 42
/// </mash>
#[builtin(namespace = "core", returns = crate::infer::Type::NUMBER)]
pub fn parse_number(#[param("String to parse as a number")] string: Str) -> R<Number> {
    Number::parse(string.as_str())
        .ok_or_else(|| MashError::new(format!("Could not parse '{}' as a number", string.as_str())))
}

/// Access the argument without triggering nullary evaluation
///
/// Examples:
/// <mash>
///   safe ls               # Returns the function value, rather than executing it
///   safe "string".reverse # Returns the bound method value, rather than executing it
/// </mash>
#[builtin(namespace = "core")]
pub fn safe(#[param(safe)] value: Value) -> R<Value> {
    Ok(value)
}

/// Apply a function to a value, ignore the result, and return the original value.
#[builtin(namespace = "core", returns = crate::infer::Type::BOOLEAN)]
pub fn tap(
    ctx: &mut Ctx,
    #[param("Function to apply to the item")] f: Callable,
    #[param("Value to tap")] value: Value,
) -> R<Value> {
    f.call1(ctx, value.clone())?;
    Ok(value)
}

/// Execute the given code, catching any exceptions
#[builtin(namespace = "core", name = "try", infer = super::infer_rules::try_)]
pub fn try_(
    ctx: &mut Ctx,
    #[param("Code to execute", lazy)] body: Value,
    #[param("Code to execute if an exception is thrown in the body", lazy)] catch: Option<Value>,
    #[param(
        "Code to execute after execution of the body, regardless of whether an exception is thrown or not",
        lazy
    )]
    finally: Option<Value>,
) -> R<Value> {
    let result = match ctx.call_values(&body, vec![]) {
        Err(error) if !error.is_interrupted() => match &catch {
            Some(catch) => ctx.call_values(catch, vec![]),
            None => Ok(Value::Unit),
        },
        other => other,
    };
    if let Some(finally) = &finally {
        // Even when a `timeout`'s deadline has passed
        ctx.without_deadlines(|ctx| ctx.call_values(finally, vec![]))?;
    }
    result
}

/// Loop executing the given block while the condition remains true
///
/// The block can come first, through a pipe. To run a block a fixed number of times, use times on a number;
/// to run it until interrupted, use forever.
///
/// Examples:
/// <mash>
///   x = 0; while (x < 5) (x += 1); x    # 5
///   (x += 1) | while (x < 10)
/// </mash>
#[builtin(namespace = "core", name = "while", returns = crate::infer::Type::UNIT)]
pub fn while_(
    ctx: &mut Ctx,
    #[param("Condition of the while loop", lazy)] condition: Value,
    #[param("Code to execute", lazy)] block: Value,
) -> R<()> {
    while ctx.call_values(&condition, vec![])?.is_truthy() {
        ctx.call_values(&block, vec![])?;
    }
    Ok(())
}

/// Run the given code over and over, until interrupted (Ctrl-C) or it throws an error
///
/// The code is typically given through a pipe. To loop while a condition holds, use while; to run code a
/// fixed number of times, use times on a number.
///
/// Examples:
/// <mash>
///   print "y" | forever                            # As the shell's yes
///   (print time.now; sleep 1.second) | forever     # A clock
/// </mash>
#[builtin(namespace = "core", returns = crate::infer::Type::UNIT)]
pub fn forever(ctx: &mut Ctx, #[param("Code to execute", lazy)] block: Value) -> R<()> {
    loop {
        // Checked here too, as a tiny block may not reach the evaluator's own checks
        ctx.check_interrupted()?;
        ctx.call_values(&block, vec![])?;
    }
}

/// Run the given code every so often, showing its latest result, until interrupted (Ctrl-C)
///
/// As the shell's watch: at the prompt, each result is drawn in place of the last, as the result of a command
/// would be (a list of objects as a table), under a header with the time, and cut to fit the terminal. An
/// error is shown in place of the result, and watching carries on. Elsewhere (in a script, or with mash -e),
/// each result is printed as plain text after the last.
///
/// Examples:
/// <mash>
///   git.status | watch
///   ls "build" | watch 5.seconds
///   watch 1.minute (http.get "https://example.com").status
/// </mash>
#[builtin(namespace = "core", returns = crate::infer::Type::UNIT)]
pub fn watch(
    ctx: &mut Ctx,
    #[param(
        "How often to run the code (default 2 seconds)",
        description = "Accepts tagged durations. If given an untagged Number, that number of milliseconds"
    )]
    interval: Option<Value>,
    #[param("Code to execute", lazy)] block: Value,
) -> R<()> {
    let interval = match &interval {
        None => std::time::Duration::from_secs(2),
        Some(value) => crate::builtins::time::duration_of(ctx, value)?.ok_or_else(|| {
            MashError::new(format!(
                "Invalid argument 'interval'. Invalid duration of type {}",
                value.type_name()
            ))
        })?,
    };
    let every = if interval.subsec_millis() == 0 {
        format!("{}s", interval.as_secs())
    } else {
        format!("{}ms", interval.as_millis())
    };
    // Lines drawn by the last frame, to move back over
    let mut drawn = 0;
    loop {
        ctx.check_interrupted()?;
        let result = ctx.call_values(&block, vec![]);
        if let Err(error) = &result
            && error.is_interrupted()
        {
            return result.map(|_| ());
        }
        let time = ctx
            .platform()
            .now()
            .to_zoned(ctx.platform().time_zone())
            .strftime("%H:%M:%S");
        let header = format!("Every {every} · {time}");
        let text = match ctx.display() {
            Some(display) => {
                // Room for the header, and for the cursor below the frame
                let room = display.rows.saturating_sub(2);
                let mut lines = (display.render)(ctx, &result)?;
                lines.truncate(room);
                let up = if drawn > 0 {
                    format!("\x1b[{drawn}A")
                } else {
                    String::new()
                };
                drawn = lines.len() + 1;
                // Back to the start of the last frame, clear from there down, and draw
                format!(
                    "{up}\r\x1b[J\x1b[90m{header}\x1b[0m\n{}",
                    lines.iter().map(|l| format!("{l}\n")).collect::<String>()
                )
            }
            None => {
                let shown = match &result {
                    Ok(value) => ctx.stringify(value)?,
                    Err(error) => format!("Error: {}", error.message),
                };
                format!("{header}\n{shown}\n")
            }
        };
        ctx.platform().write_output(&text);
        crate::builtins::time::sleep_for(ctx, interval)?;
    }
}

/// Run the given code, stopping it with an error if it hasn't finished in time
///
/// Returns the code's result if it finishes in time. Otherwise it's stopped, and this raises a "Timed out"
/// error, which try (around the timeout) can catch, or retry can retry. A try within the code can't catch it,
/// though its finally block still runs.
///
/// The code is stopped as Ctrl-C would stop it: Mash code, sleeps and HTTP requests at once; an external
/// command is sent SIGTERM, then SIGKILL if it's still running 2 seconds later. Something else that blocks
/// (looking up a host name, say) is stopped when it returns.
///
/// Examples:
/// <mash>
///   !{make test} | timeout 30.seconds
///   http.get url | timeout 5.seconds | retry 3
///   try (slowThing | timeout 2.seconds) "fallback"
/// </mash>
#[builtin(namespace = "core", infer = super::infer_rules::time_limited)]
pub fn timeout(
    ctx: &mut Ctx,
    #[param(
        "How long to allow",
        description = "Accepts tagged durations. If given an untagged Number, that number of milliseconds"
    )]
    duration: Value,
    #[param("Code to execute", lazy)] block: Value,
) -> R<Value> {
    let Some(duration) = crate::builtins::time::duration_of(ctx, &duration)? else {
        return Err(MashError::new(format!(
            "Invalid argument 'duration'. Invalid duration of type {}",
            duration.type_name()
        )));
    };
    let at = ctx.platform().now() + duration;
    let id = ctx.start_deadline(at);
    let result = ctx.call_values(&block, vec![]);
    ctx.end_deadline(id);
    match result {
        Err(error) if error.kind == ErrorKind::Expired(id) => Err(MashError::new(format!(
            "Timed out after {}",
            crate::builtins::time::describe_duration(duration)
        ))),
        other => other,
    }
}

/// Measure how long it takes to execute a piece of code
///
/// Examples:
/// <mash>
///    timeTaken (ls --recursive) | .duration # 854
/// </mash>
#[builtin(namespace = "core", infer = super::infer_rules::time_taken)]
pub fn time_taken(ctx: &mut Ctx, #[param("Code to execute", lazy)] block: Value) -> R<Object> {
    let start = ctx.platform().now();
    let result = ctx.call_values(&block, vec![])?;
    let finish = ctx.platform().now();
    let millis = finish.duration_since(start).as_millis();
    let object = Object::with_class(Some(ClassRef::Builtin(&super::TIMED_RESULT_CLASS)));
    object.set_str("result", result);
    object.set_str("started", Value::DateTime(DateTime(start)));
    object.set_str(
        "duration",
        Value::Number(
            Number::new(millis as f64),
            Some(&crate::builtins::time::MILLISECONDS_CLASS),
        ),
    );
    Ok(object)
}

/// When the timed code finished
#[builtin_method(returns = crate::infer::Type::Instance(&crate::builtins::time::DATE_TIME_CLASS))]
pub fn finished(target: Object) -> R<Value> {
    let started = match target.get_str("started") {
        Some(Value::DateTime(t)) => t.0,
        _ => return Err(MashError::new("Field 'started' must be a DateTime")),
    };
    let duration = target
        .get_str("duration")
        .and_then(|d| d.as_number())
        .and_then(|n| n.as_i64())
        .unwrap_or(0);
    let finished = started
        .checked_add(jiff::SignedDuration::from_millis(duration))
        .map_err(|e| MashError::new(e.to_string()))?;
    Ok(Value::DateTime(DateTime(finished)))
}
