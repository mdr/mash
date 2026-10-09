//! Functions on files and directories: listing, reading, writing, creating, copying, moving and deleting
//! (Scala `ListFilesFunction`, `ChildrenFunction`, `ReadFunction` … and `LinuxFileSystem`).

use std::collections::HashSet;

use mash_macros::builtin;

use crate::builtins::core::BYTES_CLASS;
use crate::complete::CompletionSpec;
use crate::eval::{Ctx, MashError, R};
use crate::function::{Args, Params};
use crate::infer::{Type, TypedArgs};
use crate::platform::{FileKind, Metadata};
use crate::value::{ClassRef, DateTime, List, Number, Object, Str, Value};

use super::paths::{self, PathArg, fs, io_error, join, path_value};
use super::{FILE_TYPE_CLASS, GROUP_CLASS, PATH_SUMMARY_CLASS, PERMISSIONS_CLASS, PERMISSIONS_SECTION_CLASS, USERNAME_CLASS};

pub(crate) fn path_summaries() -> Type {
    Type::seq(Type::Instance(&PATH_SUMMARY_CLASS))
}

/// Complete arguments as files (Scala `CompletionSpec.File`)
pub(crate) fn complete_files(_: &Params, _: usize, _: &TypedArgs) -> Vec<CompletionSpec> {
    vec![CompletionSpec::File]
}

// ------------------------------------------------------------------------------------------------ summaries

/// The `FileType` of a path (`dir`, `file`, `link` or `other`)
pub(crate) fn file_type_name(kind: FileKind) -> &'static str {
    match kind {
        FileKind::Directory => "dir",
        FileKind::File => "file",
        FileKind::Symlink => "link",
        FileKind::Other => "other",
    }
}

/// An `os.PermissionsSection` object
fn permissions_section(bits: u32) -> Value {
    let object = Object::with_class(Some(ClassRef::Builtin(&PERMISSIONS_SECTION_CLASS)));
    object.set_str("canRead", Value::Bool(bits & 4 != 0));
    object.set_str("canWrite", Value::Bool(bits & 2 != 0));
    object.set_str("canExecute", Value::Bool(bits & 1 != 0));
    Value::Object(object)
}

/// An `os.Permissions` object for permission bits
pub(crate) fn permissions_object(bits: u32) -> Value {
    let object = Object::with_class(Some(ClassRef::Builtin(&PERMISSIONS_CLASS)));
    object.set_str("owner", permissions_section(bits >> 6));
    object.set_str("group", permissions_section(bits >> 3));
    object.set_str("others", permissions_section(bits));
    Value::Object(object)
}

pub(crate) fn date_time(timestamp: jiff::Timestamp) -> Value {
    Value::DateTime(DateTime(timestamp))
}

/// A tagged string
fn tagged(text: &str, tag: &'static crate::value::BuiltinClass) -> Value {
    Value::Str(Str::tagged(text, tag))
}

/// The `os.PathSummary` object for a path and its metadata
fn summary_object(path: &str, metadata: &Metadata) -> Value {
    let object = Object::with_class(Some(ClassRef::Builtin(&PATH_SUMMARY_CLASS)));
    object.set_str("path", path_value(path));
    object.set_str("type", tagged(file_type_name(metadata.kind), &FILE_TYPE_CLASS));
    object.set_str("size", Value::Number(Number::new(metadata.size as f64), Some(&BYTES_CLASS)));
    object.set_str("owner", tagged(&metadata.owner, &USERNAME_CLASS));
    object.set_str("group", tagged(&metadata.group, &GROUP_CLASS));
    object.set_str("permissions", permissions_object(metadata.permissions));
    object.set_str("lastModified", date_time(metadata.modified));
    Value::Object(object)
}

/// Metadata of the path itself (not following a final symbolic link)
pub(crate) fn path_metadata(ctx: &Ctx, path: &str) -> R<Metadata> {
    ctx.platform()
        .symlink_metadata(fs(path))
        .map_err(|e| io_error("read", path, &e))
}

/// The `PathSummary` of a path (Scala `getPathSummary`)
pub(crate) fn summary(ctx: &Ctx, path: &str) -> R<Value> {
    Ok(summary_object(path, &path_metadata(ctx, path)?))
}

/// Whether a path is a directory (following symbolic links); false if it doesn't exist
pub(crate) fn is_dir(ctx: &Ctx, path: &str) -> bool {
    ctx.platform().metadata(fs(path)).is_ok_and(|m| m.is_dir())
}

/// Whether anything exists at a path (following symbolic links)
pub(crate) fn exists(ctx: &Ctx, path: &str) -> bool {
    ctx.platform().metadata(fs(path)).is_ok()
}

/// Summaries of the children of a directory, sorted by name; with `recursive`, each followed by its
/// descendants. Unreadable subdirectories are skipped (Scala `getChildren`).
pub(crate) fn list_children(ctx: &mut Ctx, dir: &str, ignore_dot_files: bool, recursive: bool) -> R<Vec<Value>> {
    let mut results = vec![];
    let mut ancestors = HashSet::new();
    if let Ok(real) = ctx.platform().canonicalize(fs(dir)) {
        ancestors.insert(real);
    }
    let entries = ctx.platform().read_dir(fs(dir)).map_err(|e| io_error("list", dir, &e))?;
    walk(ctx, dir, entries, ignore_dot_files, recursive, &mut ancestors, &mut results)?;
    Ok(results)
}

fn walk(
    ctx: &mut Ctx,
    dir: &str,
    mut entries: Vec<crate::platform::DirEntry>,
    ignore_dot_files: bool,
    recursive: bool,
    ancestors: &mut HashSet<String>,
    results: &mut Vec<Value>,
) -> R<()> {
    entries.sort_by(|a, b| a.name.cmp(&b.name));
    for entry in entries {
        ctx.check_interrupted()?;
        if ignore_dot_files && entry.name.starts_with('.') {
            continue;
        }
        let path = join(dir, &entry.name);
        let Ok(metadata) = ctx.platform().symlink_metadata(&path) else {
            continue;
        };
        results.push(summary_object(&path, &metadata));
        if recursive && entry.kind == FileKind::Directory {
            // Follow links to directories, but not round in circles
            let Ok(real) = ctx.platform().canonicalize(&path) else {
                continue;
            };
            if !ancestors.insert(real.clone()) {
                continue;
            }
            if let Ok(children) = ctx.platform().read_dir(&path) {
                walk(ctx, &path, children, ignore_dot_files, recursive, ancestors, results)?;
            }
            ancestors.remove(&real);
        }
    }
    Ok(())
}

/// The paths in a list argument (Scala `validatePaths`)
fn paths_in(args: &Args, name: &str, list: &List) -> R<Vec<String>> {
    list.snapshot()
        .iter()
        .map(|value| {
            paths::path_of(value).ok_or_else(|| {
                args.invalid(
                    name,
                    &format!("Could not interpret value of type {} as a path.", value.type_name()),
                )
            })
        })
        .collect()
}

// ------------------------------------------------------------------------------------------------ listing

/// List files
///
/// List files and directories, returning a sequence of os.PathSummary objects.
/// If no paths are supplied, the current directory is used as the default.
#[builtin(namespace = "os", returns = path_summaries(), completion = complete_files)]
pub(crate) fn list_files(
    ctx: &mut Ctx,
    args: &Args,
    #[param("Include files starting with a dot (default false)", short = 'a', boolean_flag)] all: bool,
    #[param(
        "Recursively retrieve results from directories (default false)",
        short = 'r',
        boolean_flag
    )]
    recursive: bool,
    #[param(
        "List directories themselves, not their contents (default false)",
        short = 'd',
        boolean_flag
    )]
    directory: bool,
    #[param(
        "Paths to list files",
        variadic,
        flatten,
        description = "Paths can either be strings or os.PathSummary objects.\nIf a given path is a file, it will be included in the output.\nIf a given path is a directory, its children will be included, unless the\n  directory parameter is true, then it will be included directly.\nIf no paths are provided, the default is the current working directory."
    )]
    paths: List,
) -> R<List> {
    let mut paths = paths_in(args, "paths", &paths)?;
    if paths.is_empty() {
        paths.push(String::new());
    }
    let mut results = vec![];
    for path in paths {
        if is_dir(ctx, &path) && !directory {
            results.extend(list_children(ctx, &path, !all, recursive)?);
        } else {
            match ctx.platform().symlink_metadata(fs(&path)) {
                Ok(metadata) => results.push(summary_object(&path, &metadata)),
                Err(_) => return Err(args.invalid("paths", &format!("No such file '{path}'"))),
            }
        }
    }
    Ok(List::new(results))
}

/// Check that a path is an existing directory
pub(crate) fn check_directory(ctx: &Ctx, path: &str) -> Result<(), String> {
    if !exists(ctx, path) {
        Err(format!("'{path}' does not exist"))
    } else if !is_dir(ctx, path) {
        Err(format!("'{path}' is not a directory"))
    } else {
        Ok(())
    }
}

/// Return the children of a directory
///
/// Return sequence of PathSummary objects representing the children of the given path.
/// If no path is supplied, the current directory is used as the default.
#[builtin(namespace = "os", returns = path_summaries(), completion = complete_files)]
pub(crate) fn children(
    ctx: &mut Ctx,
    args: &Args,
    #[param("Directory to inspect", optional)] directory: Option<PathArg>,
    #[param(def = ignore_dot_files_param)] ignore_dot_files: bool,
    #[param(def = recursive_param)] recursive: bool,
) -> R<List> {
    let dir = directory.map(|d| d.0).unwrap_or_default();
    check_directory(ctx, &dir).map_err(|message| args.invalid("directory", &message))?;
    Ok(List::new(list_children(ctx, &dir, ignore_dot_files, recursive)?))
}

pub(crate) fn ignore_dot_files_param() -> crate::function::Param {
    crate::function::Param::named("ignoreDotFiles")
        .summary("Ignore files starting with a dot (.) (default false)")
        .short('i')
        .boolean_flag()
}

pub(crate) fn recursive_param() -> crate::function::Param {
    crate::function::Param::named("recursive")
        .summary("Recursively retrieve results from subdirectories (default false)")
        .short('r')
        .boolean_flag()
}

// ------------------------------------------------------------------------------------------------ reading

/// The contents of a file as text (lossily decoded as UTF-8)
pub(crate) fn read_text(ctx: &Ctx, path: &str) -> R<String> {
    let bytes = ctx.platform().read_file(fs(path)).map_err(|e| io_error("read", path, &e))?;
    Ok(match String::from_utf8(bytes) {
        Ok(text) => text,
        Err(e) => String::from_utf8_lossy(e.as_bytes()).into_owned(),
    })
}

/// Lines of text, split at `\n`, `\r\n` or `\r` (as Java's `BufferedReader.readLine`)
pub(crate) fn split_lines(text: &str) -> Vec<Value> {
    let mut lines = vec![];
    let mut rest = text;
    while !rest.is_empty() {
        match rest.find(['\n', '\r']) {
            Some(i) => {
                lines.push(Value::str(&rest[..i]));
                let terminator = if rest[i..].starts_with("\r\n") { 2 } else { 1 };
                rest = &rest[i + terminator..];
            }
            None => {
                lines.push(Value::str(rest));
                break;
            }
        }
    }
    lines
}

/// Read the contents of a file as a string
///
/// Returns a string with the contents of the given file.
/// The file is decoded as UTF-8.
#[builtin(namespace = "os", returns = Type::STRING, completion = complete_files)]
pub(crate) fn read(ctx: &mut Ctx, #[param("File from which to read text")] file: PathArg) -> R<String> {
    read_text(ctx, &file.0)
}

/// Read lines from a file
///
/// Returns a sequence of lines read from the given file.
/// The file is decoded as UTF-8; lines end with `\n`, `\r\n` or `\r`.
#[builtin(namespace = "os", returns = Type::seq(Type::STRING), completion = complete_files)]
pub(crate) fn read_lines(ctx: &mut Ctx, #[param("File from which to read lines")] file: PathArg) -> R<List> {
    Ok(List::new(split_lines(&read_text(ctx, &file.0)?)))
}

// ------------------------------------------------------------------------------------------------ writing

/// Write data to a file: a line for each item of a list, otherwise the item as a string (Scala
/// `WriteFunction.write`)
pub(crate) fn write_data(ctx: &mut Ctx, path: &str, data: &Value, append: bool) -> R<()> {
    let text = match data {
        Value::List(xs) => {
            let mut text = String::new();
            for x in xs.snapshot() {
                text.push_str(&ctx.stringify(&x)?);
                text.push('\n');
            }
            text
        }
        x => ctx.stringify(x)?,
    };
    ctx.platform()
        .write_file(fs(path), text.as_bytes(), append)
        .map_err(|e| io_error("write", path, &e))
}

pub(crate) fn append_param() -> crate::function::Param {
    crate::function::Param::named("append")
        .summary("Append to the end of the file, if it already exists")
        .short('a')
        .boolean_flag()
}

pub(crate) fn data_param() -> crate::function::Param {
    crate::function::Param::named("data")
        .summary("Data to write to the file")
        .description("If the given data is a sequence, write a line to the file for each item.\nOtherwise, write the item as a string.")
}

/// Write an object or sequence of objects to a file as a string
///
/// The strings are encoded as UTF-8.
/// If multiple lines are written, each ends with a newline.
#[builtin(namespace = "os", returns = Type::UNIT, completion = complete_files)]
pub(crate) fn write(
    ctx: &mut Ctx,
    #[param(def = append_param)] append: bool,
    #[param("File to write to")] file: PathArg,
    #[param(def = data_param)] data: Value,
) -> R<()> {
    write_data(ctx, &file.0, &data, append)
}

// ------------------------------------------------------------------------------------------------ creating

pub(crate) fn create_intermediates_param() -> crate::function::Param {
    crate::function::Param::named("createIntermediates")
        .summary("Create intermediate directories as required (default false)")
        .short('c')
        .boolean_flag()
}

pub(crate) fn create_directory_at(ctx: &Ctx, path: &str, intermediates: bool) -> R<Value> {
    ctx.platform()
        .create_dir(fs(path), intermediates)
        .map_err(|e| io_error("create directory", path, &e))?;
    Ok(path_value(path))
}

/// Create a new directory
#[builtin(namespace = "os", returns = Type::PATH)]
pub(crate) fn create_directory(
    ctx: &mut Ctx,
    args: &Args,
    #[param(def = create_intermediates_param)] create_intermediates: bool,
    #[param("Paths to create directories at", variadic, at_least_one, flatten)] paths: List,
) -> R<Value> {
    let paths = paths_in(args, "paths", &paths)?;
    let mut created = vec![];
    for path in &paths {
        created.push(create_directory_at(ctx, path, create_intermediates)?);
    }
    Ok(match <[Value; 1]>::try_from(created) {
        Ok([path]) => path,
        Err(created) => Value::list(created),
    })
}

/// Create a new temporary directory
#[builtin(namespace = "os", returns = Type::PATH)]
pub(crate) fn create_temp_directory(ctx: &mut Ctx) -> R<Value> {
    let path = ctx
        .platform()
        .create_temp_dir()
        .map_err(|e| MashError::new(format!("Could not create a temporary directory: {}", paths::describe(&e))))?;
    Ok(path_value(path))
}

/// Create a new temporary file
#[builtin(namespace = "os", returns = Type::PATH)]
pub(crate) fn create_temp_file(ctx: &mut Ctx) -> R<Value> {
    let path = ctx
        .platform()
        .create_temp_file()
        .map_err(|e| MashError::new(format!("Could not create a temporary file: {}", paths::describe(&e))))?;
    Ok(path_value(path))
}

// ------------------------------------------------------------------------------------------------ deleting

/// Delete a path: a file, a link, or a directory with everything in it
pub(crate) fn delete_path(ctx: &Ctx, path: &str) -> R<()> {
    ctx.platform().remove(fs(path)).map_err(|e| io_error("delete", path, &e))
}

/// Delete files and directories
#[builtin(namespace = "os", returns = Type::UNIT, completion = complete_files)]
pub(crate) fn delete(
    ctx: &mut Ctx,
    args: &Args,
    #[param("Paths to delete", variadic, at_least_one, flatten)] paths: List,
) -> R<()> {
    let mut paths = paths_in(args, "paths", &paths)?;
    if paths.is_empty() {
        return Err(MashError::new("Must provide at least one path to delete"));
    }
    // Children before their parents
    let cwd = ctx.platform().current_dir().unwrap_or_default();
    paths.sort_by_cached_key(|p| crate::platform::resolve(&cwd, p));
    for path in paths.iter().rev() {
        delete_path(ctx, path)?;
    }
    Ok(())
}

// ------------------------------------------------------------------------------------------------ copying

/// Copy a file, or a directory and its contents (Commons IO `copyDirectory`: merging into an existing
/// destination directory)
pub(crate) fn copy_path(ctx: &mut Ctx, from: &str, to: &str) -> R<()> {
    if is_dir(ctx, from) {
        let entries = ctx.platform().read_dir(fs(from)).map_err(|e| io_error("copy", from, &e))?;
        ctx.platform()
            .create_dir(to, true)
            .map_err(|e| io_error("create directory", to, &e))?;
        for entry in entries {
            ctx.check_interrupted()?;
            copy_path(ctx, &join(from, &entry.name), &join(to, &entry.name))?;
        }
        Ok(())
    } else {
        ctx.platform()
            .copy_file(fs(from), to)
            .map_err(|e| io_error("copy", from, &e))
    }
}

/// `dir/name`, where `name` is the last segment of `path`
pub(crate) fn path_in(dir: &str, path: &str) -> String {
    join(dir, &paths::file_name(path).unwrap_or_default())
}

/// Copy files and directories
#[builtin(namespace = "os", returns = Type::UNIT, completion = complete_files)]
pub(crate) fn copy(
    ctx: &mut Ctx,
    args: &Args,
    #[param(
        "Paths to copy",
        variadic,
        at_least_one,
        flatten,
        description = "There must be at least one path provided.\nIf there are multiple paths provided, the destination must be a directory.\nIf a path is a directory, it and all its contents (recursively) will be copied to the destination."
    )]
    source_paths: List,
    #[param(
        "Location to copy path(s)",
        description = "If the destination is a directory, the source paths will be copied into it.\nIf the destination is not a directory, only a single source path may be provided."
    )]
    destination: PathArg,
) -> R<()> {
    let sources = paths_in(args, "sourcePaths", &source_paths)?;
    let destination = destination.0;
    if sources.is_empty() {
        return Err(MashError::new("Must have at least one source path"));
    }
    if is_dir(ctx, &destination) {
        for source in &sources {
            copy_path(ctx, source, &path_in(&destination, source))?;
        }
    } else {
        if sources.len() > 1 {
            return Err(MashError::new(
                "Multiple source paths were provided, but the destination was not a directory",
            ));
        }
        let source = &sources[0];
        if is_dir(ctx, source) && exists(ctx, &destination) {
            return Err(MashError::new("Destination already exists"));
        }
        copy_path(ctx, source, &destination)?;
    }
    Ok(())
}

// ------------------------------------------------------------------------------------------------ moving

/// Move a path to a new location, copying then deleting if it's on another file system
pub(crate) fn move_path(ctx: &mut Ctx, from: &str, to: &str) -> R<()> {
    match ctx.platform().rename(fs(from), to) {
        Ok(()) => Ok(()),
        Err(e) if e.kind() == std::io::ErrorKind::CrossesDevices => {
            copy_path(ctx, from, to)?;
            delete_path(ctx, from)
        }
        Err(e) => Err(io_error("move", from, &e)),
    }
}

/// Move a path into a directory, which must not already contain something of the same name
pub(crate) fn move_into(ctx: &mut Ctx, from: &str, dir: &str) -> R<String> {
    let destination = path_in(dir, from);
    if ctx.platform().symlink_metadata(&destination).is_ok() {
        return Err(MashError::new(format!("Destination '{destination}' already exists")));
    }
    move_path(ctx, from, &destination)?;
    Ok(destination)
}

/// Move files or directories to a new location
///
/// If multiple paths are provided, the destination must be a directory.
#[builtin(name = "move", namespace = "os", returns = Type::UNIT, completion = complete_files)]
pub(crate) fn move_(
    ctx: &mut Ctx,
    args: &Args,
    #[param(
        "Paths to move",
        variadic,
        at_least_one,
        flatten,
        description = "At least one path must provided."
    )]
    paths: List,
    #[param("Location to move to")] destination: PathArg,
) -> R<()> {
    let sources = paths_in(args, "paths", &paths)?;
    let destination = destination.0;
    if sources.is_empty() {
        return Err(MashError::new("Must have at least one source path"));
    }
    if is_dir(ctx, &destination) {
        for source in &sources {
            move_into(ctx, source, &destination)?;
        }
    } else {
        if sources.len() > 1 {
            return Err(MashError::new("Multiple files can only be moved into a directory"));
        }
        // Relative to the source's directory (Java `resolveSibling`)
        let source = &sources[0];
        let target = join(&paths::parent(source).unwrap_or_default(), &destination);
        move_path(ctx, source, &target)?;
    }
    Ok(())
}

// ------------------------------------------------------------------------------------------------ PATH

/// Find full path to an executable on the path
#[builtin(namespace = "os", returns = Type::PATH)]
pub(crate) fn find_executable_in_path(ctx: &mut Ctx, #[param("Name of executable")] name: Str) -> R<Value> {
    let search_path = ctx.platform().env_var("PATH").unwrap_or_default();
    for dir in search_path.split(':') {
        let candidate = join(dir, name.as_str());
        if let Ok(metadata) = ctx.platform().metadata(fs(&candidate))
            && metadata.is_file()
            && metadata.permissions & 0o111 != 0
        {
            return Ok(path_value(candidate));
        }
    }
    Ok(Value::Null)
}
