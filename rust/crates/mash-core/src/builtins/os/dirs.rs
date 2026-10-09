//! The working directory: changing it, and the history of directories visited (Scala `WorkingDirectoryStack`,
//! `CurrentDirectoryManager`, `ChangeDirectoryFunction`, `UpFunction` …).

use mash_macros::builtin;

use crate::complete::CompletionSpec;
use crate::eval::{Ctx, MashError, R};
use crate::function::{Args, Params};
use crate::infer::{Inferencer, Type, TypedArgs};
use crate::value::{List, Value};

use super::files::is_dir;
use super::paths::{PathArg, fs, io_error, path_value};

/// The directories visited with `cd`, for `back`, `forward` and `oldDirs`: like a browser's history, going
/// somewhere new discards the directories ahead.
#[derive(Debug, Clone)]
pub struct WorkingDirectoryStack {
    dirs: Vec<String>,
    pos: usize,
}

impl WorkingDirectoryStack {
    pub fn new(initial: impl Into<String>) -> WorkingDirectoryStack {
        WorkingDirectoryStack {
            dirs: vec![initial.into()],
            pos: 0,
        }
    }

    /// Record a visit to `path`
    pub fn push(&mut self, path: impl Into<String>) {
        self.dirs.truncate(self.pos + 1);
        self.dirs.push(path.into());
        self.pos = self.dirs.len() - 1;
    }

    /// The directories before the current one
    pub fn old_dirs(&self) -> &[String] {
        &self.dirs[..self.pos]
    }

    /// Step back, returning the previous directory, if any
    pub fn back(&mut self) -> Option<&str> {
        if self.pos > 0 {
            self.pos -= 1;
            Some(&self.dirs[self.pos])
        } else {
            None
        }
    }

    /// Step forward, returning the next directory, if any
    pub fn forward(&mut self) -> Option<&str> {
        if self.pos + 1 < self.dirs.len() {
            self.pos += 1;
            Some(&self.dirs[self.pos])
        } else {
            None
        }
    }
}

/// The current directory
pub(crate) fn current_dir(ctx: &Ctx) -> R<String> {
    ctx.platform()
        .current_dir()
        .ok_or_else(|| MashError::new("There is no current directory"))
}

fn home_dir(ctx: &Ctx) -> R<String> {
    ctx.platform()
        .home_dir()
        .ok_or_else(|| MashError::new("There is no home directory"))
}

fn set_dir(ctx: &Ctx, path: &str) -> R<()> {
    ctx.platform()
        .set_current_dir(fs(path))
        .map_err(|e| io_error("change directory to", path, &e))
}

/// Change into a directory, recording it in the history (Scala `CurrentDirectoryManager`). Returns false if
/// it isn't a directory.
pub(crate) fn enter_directory(ctx: &mut Ctx, path: &str) -> R<bool> {
    if !is_dir(ctx, path) {
        return Ok(false);
    }
    set_dir(ctx, path)?;
    let now = current_dir(ctx)?;
    ctx.working_directories().push(now);
    Ok(true)
}

/// `cd` in mish: no arguments to go home, else one directory
pub(crate) fn mish_cd(ctx: &mut Ctx, args: &[Value]) -> R<()> {
    let path = match args {
        [] => home_dir(ctx)?,
        [path] => ctx.stringify(path)?,
        _ => return Err(MashError::new("Too many arguments to 'cd'")),
    };
    if enter_directory(ctx, &path)? {
        Ok(())
    } else {
        Err(MashError::new(format!(
            "Could not change directory to '{path}', not a directory"
        )))
    }
}

fn complete_directory(_: &Params, _: usize, _: &TypedArgs) -> Vec<CompletionSpec> {
    vec![CompletionSpec::Directory]
}

/// Change the current working directory
///
/// Examples:
/// <mash>
///    cd "/tmp"
///    cd # cd to home directory
/// </mash>
#[builtin(namespace = "os", returns = Type::UNIT, completion = complete_directory)]
pub(crate) fn change_directory(
    ctx: &mut Ctx,
    args: &Args,
    #[param(
        "Directory to change into; defaults to the current user's home directory.",
        optional
    )]
    directory: Option<PathArg>,
) -> R<()> {
    let path = match directory {
        Some(path) => path.0,
        None => home_dir(ctx)?,
    };
    if enter_directory(ctx, &path)? {
        Ok(())
    } else {
        Err(args.invalid(
            "directory",
            &format!("Could not change directory to '{path}', not a directory"),
        ))
    }
}

/// Return the current working directory
#[builtin(namespace = "os", returns = Type::PATH)]
pub(crate) fn current_directory(ctx: &mut Ctx) -> R<Value> {
    Ok(path_value(current_dir(ctx)?))
}

/// Return the current user's home directory
#[builtin(namespace = "os", returns = Type::PATH)]
pub(crate) fn home(ctx: &mut Ctx) -> R<Value> {
    Ok(path_value(home_dir(ctx)?))
}

/// Change the current working directory to a parent (or ancestor)
#[builtin(namespace = "os", returns = Type::UNIT)]
pub(crate) fn up(
    ctx: &mut Ctx,
    #[param("Number of parent directories to move up (default 1)", default = 1i64)] n: i64,
) -> R<()> {
    for _ in 0..n {
        set_dir(ctx, "..")?;
    }
    let now = current_dir(ctx)?;
    ctx.working_directories().push(now);
    Ok(())
}

/// Go to the previous directory in the history
#[builtin(namespace = "os", returns = Type::UNIT)]
pub(crate) fn back(ctx: &mut Ctx) -> R<()> {
    let previous = ctx
        .working_directories()
        .back()
        .map(str::to_string)
        .ok_or_else(|| MashError::new("No previous directory in history"))?;
    set_dir(ctx, &previous)
}

/// Go forwards to the next directory in the history
#[builtin(namespace = "os", returns = Type::UNIT)]
pub(crate) fn forward(ctx: &mut Ctx) -> R<()> {
    let next = ctx
        .working_directories()
        .forward()
        .map(str::to_string)
        .ok_or_else(|| MashError::new("No next directory in history"))?;
    set_dir(ctx, &next)
}

/// Previous working directories
#[builtin(namespace = "os", returns = Type::seq(Type::PATH))]
pub(crate) fn old_dirs(ctx: &mut Ctx) -> R<List> {
    let dirs = ctx.working_directories().old_dirs().to_vec();
    Ok(List::new(dirs.into_iter().map(path_value).collect()))
}

fn within_infer(_: &mut Inferencer<'_>, params: &Params, args: &TypedArgs) -> Option<Type> {
    args.bind(params).ty(params, "block")
}

fn within_completion(params: &Params, arg_pos: usize, args: &TypedArgs) -> Vec<CompletionSpec> {
    match args.bind(params).param_at(params, arg_pos) {
        Some("directory") => vec![CompletionSpec::Directory],
        _ => vec![],
    }
}

/// Temporarily change the current working directory to execute a task, then change back
///
/// Examples:
/// <mash>
///    within "~/project" git.log
/// </mash>
#[builtin(namespace = "os", infer = within_infer, completion = within_completion)]
pub(crate) fn within(
    ctx: &mut Ctx,
    #[param("Directory to change into")] directory: PathArg,
    #[param("Code to execute", lazy)] block: Value,
) -> R<Value> {
    let path = directory.0;
    let old_dir = current_dir(ctx)?;
    if !is_dir(ctx, &path) {
        return Err(MashError::new(format!(
            "Could not change directory to '{path}', not a directory"
        )));
    }
    set_dir(ctx, &path)?;
    let result = ctx.call_values(&block, vec![]);
    set_dir(ctx, &old_dir)?;
    result
}

#[cfg(test)]
mod tests {
    use super::WorkingDirectoryStack;

    // Port of WorkingDirectoryStackTest
    #[test]
    fn not_have_a_bug() {
        let mut stack = WorkingDirectoryStack::new("/initial");
        assert!(stack.old_dirs().is_empty());
        stack.push("/foo");
        assert_eq!(stack.old_dirs(), ["/initial"]);
        stack.back();
        assert!(stack.old_dirs().is_empty());
        stack.push("/bar");
        assert_eq!(stack.old_dirs(), ["/initial"]);
        stack.push("/baz");
        assert_eq!(stack.old_dirs(), ["/initial", "/bar"]);
    }

    #[test]
    fn allow_forward_and_backward_navigation() {
        let mut stack = WorkingDirectoryStack::new("/initial");
        stack.push("/foo");
        assert_eq!(stack.back(), Some("/initial"));
        assert_eq!(stack.forward(), Some("/foo"));
        assert_eq!(stack.old_dirs(), ["/initial"]);
        assert_eq!(stack.forward(), None);
    }
}
