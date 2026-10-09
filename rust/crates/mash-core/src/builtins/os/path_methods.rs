//! Methods of `os.Path` (path-tagged strings), also available on `os.PathSummary` objects (Scala
//! `ns/os/pathClass`).

use mash_macros::builtin_method;

use crate::complete::CompletionSpec;
use crate::eval::{Ctx, MashError, R};
use crate::function::{Args, BuiltinMethod, Callable, Params};
use crate::infer::{Inferencer, Type, TypedArgs};
use crate::value::{List, Number, Str, Value};

use super::dirs::enter_directory;
use super::files::{
    self, append_param, check_directory, copy_path, create_directory_at, create_intermediates_param, data_param,
    delete_path, file_type_name, ignore_dot_files_param, is_dir, list_children, move_path, path_in, path_metadata,
    path_summaries, permissions_object, read_text, recursive_param, split_lines, write_data,
};
use super::paths::{self, PathArg, fs, io_error, join, path_value, target_path};
use super::{FILE_TYPE_CLASS, GROUP_CLASS, PATH_SUMMARY_CLASS, PERMISSIONS_CLASS, USERNAME_CLASS};
use crate::builtins::core::BYTES_CLASS;

/// Methods of `os.Path`
pub(crate) const METHODS: &[&dyn BuiltinMethod] = &[
    &AbsoluteMethod,
    &BaseNameMethod,
    &ChangeDirectoryMethod,
    &ChildrenMethod,
    &CopyIntoMethod,
    &CopyMethod,
    &DeleteMethod,
    &ExistsMethod,
    &ExtensionMethod,
    &FollowLinkMethod,
    &GroupMethod,
    &InfoMethod,
    &IsDirectoryMethod,
    &IsEmptyDirMethod,
    &IsFileMethod,
    &LastModifiedMethod,
    &CreateDirectoryMethod,
    &MoveIntoMethod,
    &NameMethod,
    &super::system::PathOpenMethod,
    &OwnerMethod,
    &ParentMethod,
    &PermissionsMethod,
    &ReadMethod,
    &ReadLinesMethod,
    &RenameByMethod,
    &RenameToMethod,
    &super::process::PathRunMethod,
    &SegmentsMethod,
    &SetExecutableMethod,
    &SetReadableMethod,
    &SetWritableMethod,
    &SizeMethod,
    &WithTildeMethod,
    &TypeMethod,
    &WriteMethod,
];

/// The absolute path to this location
///
/// Symbolic links, `.` and `..` are resolved; the path must exist.
#[builtin_method(returns = Type::PATH)]
fn absolute(ctx: &mut Ctx, target: &Value) -> R<Value> {
    let path = target_path(target)?;
    let real = ctx
        .platform()
        .canonicalize(fs(&path))
        .map_err(|e| io_error("resolve", &path, &e))?;
    Ok(path_value(real))
}

/// The last segment of a path, if it has one
fn name_of(target: &Value) -> R<Option<String>> {
    Ok(paths::file_name(&target_path(target)?))
}

/// Name without extension
#[builtin_method(returns = Type::STRING)]
fn base_name(target: &Value) -> R<Value> {
    Ok(match name_of(target)? {
        Some(name) => Value::str(name.rfind('.').map_or(name.as_str(), |i| &name[..i])),
        None => Value::Null,
    })
}

/// File extension, if any, else null
#[builtin_method(returns = Type::STRING)]
fn extension(target: &Value) -> R<Value> {
    Ok(match name_of(target)? {
        Some(name) => match name.rfind('.') {
            Some(i) => Value::str(&name[i + 1..]),
            None => Value::Null,
        },
        None => Value::Null,
    })
}

/// Name (last segment) of this path
#[builtin_method(returns = Type::PATH)]
fn name(target: &Value) -> R<Value> {
    Ok(name_of(target)?.map_or(Value::Null, path_value))
}

/// The parent of this path
#[builtin_method(returns = Type::PATH)]
fn parent(target: &Value) -> R<Value> {
    Ok(paths::parent(&target_path(target)?).map_or(Value::Null, path_value))
}

/// A sequence of the segments of this path (the parts of the path separated by /)
#[builtin_method(returns = Type::seq(Type::STRING))]
fn segments(target: &Value) -> R<List> {
    let path = target_path(target)?;
    Ok(List::new(paths::segments(&path).into_iter().map(path_value).collect()))
}

/// Replace a home directory prefix with ~ if possible
#[builtin_method(returns = Type::STRING)]
fn with_tilde(ctx: &mut Ctx, target: &Value) -> R<String> {
    let path = target_path(target)?;
    let home = ctx.platform().home_dir().unwrap_or_default();
    Ok(match path.strip_prefix(&home) {
        Some(rest) if !home.is_empty() && (rest.is_empty() || rest.starts_with('/')) => format!("~{rest}"),
        _ => path,
    })
}

/// Change directory to this path
#[builtin_method(aliases = ["cd"], returns = Type::UNIT)]
fn change_directory(ctx: &mut Ctx, target: &Value) -> R<()> {
    let path = target_path(target)?;
    if enter_directory(ctx, &path)? {
        Ok(())
    } else {
        Err(MashError::new(format!(
            "Could not change directory to '{path}', not a directory"
        )))
    }
}

/// The children of this path
#[builtin_method(returns = path_summaries())]
fn children(
    ctx: &mut Ctx,
    target: &Value,
    #[param(def = ignore_dot_files_param)] ignore_dot_files: bool,
    #[param(def = recursive_param)] recursive: bool,
) -> R<List> {
    let dir = target_path(target)?;
    check_directory(ctx, &dir).map_err(MashError::new)?;
    Ok(List::new(list_children(ctx, &dir, ignore_dot_files, recursive)?))
}

fn complete_directory(_: &Params, _: usize, _: Option<&Type>, _: &TypedArgs) -> Vec<CompletionSpec> {
    vec![CompletionSpec::Directory]
}

/// Copy this file or directory to another location
#[builtin_method(returns = Type::UNIT)]
fn copy(
    ctx: &mut Ctx,
    target: &Value,
    args: &Args,
    #[param("Location to copy file to")] destination: PathArg,
) -> R<()> {
    let source = target_path(target)?;
    let destination = destination.0;
    if is_dir(ctx, &source) {
        if files::exists(ctx, &destination) {
            return Err(args.invalid("destination", "Destination already exists"));
        }
    } else if is_dir(ctx, &destination) {
        return Err(args.invalid("destination", "Destination already exists, and is a directory"));
    } else if files::exists(ctx, &destination) {
        return Err(args.invalid("destination", "Destination already exists"));
    }
    copy_path(ctx, &source, &destination)
}

/// Copy this path into another location
#[builtin_method(returns = Type::PATH, completion = complete_directory)]
fn copy_into(ctx: &mut Ctx, target: &Value, #[param("Location to copy file to")] destination: PathArg) -> R<Value> {
    let source = target_path(target)?;
    let dir = destination.0;
    if !is_dir(ctx, &dir) {
        return Err(MashError::new(format!("Cannot copy into {dir}, not a directory")));
    }
    let copy = path_in(&dir, &source);
    copy_path(ctx, &source, &copy)?;
    Ok(path_value(copy))
}

/// Move this path into the given directory
///
/// Returns the new path.
#[builtin_method(returns = Type::PATH, completion = complete_directory)]
fn move_into(
    ctx: &mut Ctx,
    target: &Value,
    args: &Args,
    #[param("Destination directory to move this file into")] destination: PathArg,
) -> R<Value> {
    let source = target_path(target)?;
    let dir = destination.0;
    if ctx.platform().symlink_metadata(fs(&source)).is_err() {
        return Err(MashError::new(format!("'{source}' does not exist")));
    }
    check_directory(ctx, &dir).map_err(|message| args.invalid("destination", &message))?;
    let new_path = path_in(&dir, &source);
    if is_dir(ctx, &source) && files::exists(ctx, &new_path) {
        return Err(MashError::new(format!("Destination '{new_path}' already exists")));
    }
    move_path(ctx, &source, &new_path)?;
    Ok(path_value(new_path))
}

/// Delete this path
#[builtin_method(aliases = ["rm"], returns = Type::UNIT)]
fn delete(ctx: &mut Ctx, target: &Value) -> R<()> {
    delete_path(ctx, &target_path(target)?)
}

/// Whether or not an item exists at this location
#[builtin_method(returns = Type::BOOLEAN)]
fn exists(ctx: &mut Ctx, target: &Value) -> R<bool> {
    Ok(files::exists(ctx, &target_path(target)?))
}

/// Check if path is a directory
#[builtin_method(returns = Type::BOOLEAN)]
fn is_directory(ctx: &mut Ctx, target: &Value) -> R<bool> {
    Ok(is_dir(ctx, &target_path(target)?))
}

/// Check if path is a file
#[builtin_method(returns = Type::BOOLEAN)]
fn is_file(ctx: &mut Ctx, target: &Value) -> R<bool> {
    let path = target_path(target)?;
    Ok(ctx.platform().metadata(fs(&path)).is_ok_and(|m| m.is_file()))
}

/// Check if path is an empty directory
#[builtin_method(returns = Type::BOOLEAN)]
fn is_empty_dir(ctx: &mut Ctx, target: &Value) -> R<bool> {
    let path = target_path(target)?;
    Ok(is_dir(ctx, &path)
        && ctx
            .platform()
            .read_dir(fs(&path))
            .is_ok_and(|entries| entries.is_empty()))
}

/// Follow this symbolic link
#[builtin_method(returns = Type::PATH)]
fn follow_link(ctx: &mut Ctx, target: &Value) -> R<Value> {
    let path = target_path(target)?;
    let metadata = ctx
        .platform()
        .symlink_metadata(fs(&path))
        .map_err(|_| MashError::new(format!("Path '{path}' does not exist")))?;
    if metadata.kind != crate::platform::FileKind::Symlink {
        return Err(MashError::new(format!("Path '{path}' is not a link")));
    }
    let resolved = ctx
        .platform()
        .read_link(fs(&path))
        .map_err(|e| io_error("read link", &path, &e))?;
    Ok(path_value(resolved))
}

/// Get PathSummary object for this path
#[builtin_method(returns = Type::Instance(&PATH_SUMMARY_CLASS))]
fn info(ctx: &mut Ctx, target: &Value) -> R<Value> {
    files::summary(ctx, &target_path(target)?)
}

/// Group owner of this path
#[builtin_method(returns = Type::tagged_string(&GROUP_CLASS))]
fn group(ctx: &mut Ctx, target: &Value) -> R<Value> {
    let metadata = path_metadata(ctx, &target_path(target)?)?;
    Ok(Value::Str(Str::tagged(metadata.group, &GROUP_CLASS)))
}

/// Owner of this path
#[builtin_method(returns = Type::tagged_string(&USERNAME_CLASS))]
fn owner(ctx: &mut Ctx, target: &Value) -> R<Value> {
    let metadata = path_metadata(ctx, &target_path(target)?)?;
    Ok(Value::Str(Str::tagged(metadata.owner, &USERNAME_CLASS)))
}

/// Last time path was modified
#[builtin_method(returns = Type::Instance(&crate::builtins::time::DATE_TIME_CLASS))]
fn last_modified(ctx: &mut Ctx, target: &Value) -> R<Value> {
    let metadata = path_metadata(ctx, &target_path(target)?)?;
    Ok(files::date_time(metadata.modified))
}

/// Permissions for this path
#[builtin_method(returns = Type::Instance(&PERMISSIONS_CLASS))]
fn permissions(ctx: &mut Ctx, target: &Value) -> R<Value> {
    let metadata = path_metadata(ctx, &target_path(target)?)?;
    Ok(permissions_object(metadata.permissions))
}

/// Size of the file at this path
#[builtin_method(returns = Type::tagged_number(&BYTES_CLASS))]
fn size(ctx: &mut Ctx, target: &Value) -> R<Value> {
    let metadata = path_metadata(ctx, &target_path(target)?)?;
    Ok(Value::Number(Number::new(metadata.size as f64), Some(&BYTES_CLASS)))
}

/// Type of object at this path (file, directory etc)
#[builtin_method(name = "type", returns = Type::tagged_string(&FILE_TYPE_CLASS))]
fn type_(ctx: &mut Ctx, target: &Value) -> R<Value> {
    let metadata = path_metadata(ctx, &target_path(target)?)?;
    Ok(Value::Str(Str::tagged(file_type_name(metadata.kind), &FILE_TYPE_CLASS)))
}

/// Create directory at this path
#[builtin_method(returns = Type::PATH)]
fn create_directory(
    ctx: &mut Ctx,
    target: &Value,
    #[param(def = create_intermediates_param)] create_intermediates: bool,
) -> R<Value> {
    create_directory_at(ctx, &target_path(target)?, create_intermediates)
}

/// Read the contents of this file as a string
///
/// Returns a string with the contents of this file.
/// The file is decoded as UTF-8.
#[builtin_method(returns = Type::STRING)]
fn read(ctx: &mut Ctx, target: &Value) -> R<String> {
    read_text(ctx, &target_path(target)?)
}

/// Read lines from this file
///
/// Returns a sequence of lines read from this file.
/// The file is decoded as UTF-8; lines end with `\n`, `\r\n` or `\r`.
#[builtin_method(returns = Type::seq(Type::STRING))]
fn read_lines(ctx: &mut Ctx, target: &Value) -> R<List> {
    Ok(List::new(split_lines(&read_text(ctx, &target_path(target)?)?)))
}

/// Write an object or sequence of objects to a file as a string
///
/// The strings are encoded as UTF-8.
/// If multiple lines are written, each ends with a newline.
#[builtin_method(returns = Type::UNIT)]
fn write(
    ctx: &mut Ctx,
    target: &Value,
    #[param(def = append_param)] append: bool,
    #[param(def = data_param)] data: Value,
) -> R<()> {
    write_data(ctx, &target_path(target)?, &data, append)
}

// ------------------------------------------------------------------------------------------------ renaming

/// Check a new name for a path (Scala `RenameToMethod.validateName`)
fn validate_name(name: &str) -> Result<(), &'static str> {
    if paths::segments(name).len() > 1 {
        Err("Name cannot contain directory separators")
    } else if name.is_empty() {
        Err("Name cannot be empty")
    } else if name == "." || name == ".." {
        Err("Name cannot be . or ..")
    } else {
        Ok(())
    }
}

/// Rename a path within its directory, returning the new path
fn rename_within(ctx: &mut Ctx, path: &str, name: &str) -> R<Value> {
    let new_path = join(&paths::parent(path).unwrap_or_default(), name);
    if ctx.platform().symlink_metadata(&new_path).is_ok() {
        return Err(MashError::new(format!(
            "Could not rename '{path}': '{new_path}' already exists"
        )));
    }
    move_path(ctx, path, &new_path)?;
    Ok(path_value(new_path))
}

/// Rename the file or directory
///
/// The file or directory is not moved out of its parent directory.
///
/// Examples:
/// <mash>
///   "/etc/hosts.conf".renameTo "hosts.conf.bak" # Moves file to /etc/hosts.conf.bak
/// </mash>
#[builtin_method(returns = Type::PATH)]
fn rename_to(
    ctx: &mut Ctx,
    target: &Value,
    args: &Args,
    #[param(
        "New name for the file or directory",
        description = "New name must not be \".\" or \"..\", contain directory separators, or be empty."
    )]
    new_name: PathArg,
) -> R<Value> {
    let path = target_path(target)?;
    validate_name(&new_name.0).map_err(|message| args.invalid("newName", message))?;
    rename_within(ctx, &path, &paths::normalise(&new_name.0))
}

fn rename_by_infer(
    inferencer: &mut Inferencer<'_>,
    target: Option<&Type>,
    params: &Params,
    args: &TypedArgs,
) -> Option<Type> {
    if let (Some(f), Some(target)) = (args.bind(params).ty(params, "f"), target) {
        inferencer.apply_function(&f, target, None);
    }
    Some(Type::PATH)
}

/// Rename this path using a function to transform the name
#[builtin_method(infer = rename_by_infer)]
fn rename_by(
    ctx: &mut Ctx,
    target: &Value,
    args: &Args,
    #[param("Function to transform the old name into a new name")] f: Callable,
) -> R<Value> {
    let path = target_path(target)?;
    let old_name = paths::file_name(&path).unwrap_or_default();
    let new_name = f.call1(ctx, path_value(old_name))?;
    let new_name = ctx.stringify(&new_name)?;
    validate_name(&new_name).map_err(|message| args.invalid("f", message))?;
    rename_within(ctx, &path, &paths::normalise(&new_name))
}

// ------------------------------------------------------------------------------------------------ permissions

fn party_param(name: &'static str, summary: &'static str, short: char) -> crate::function::Param {
    crate::function::Param::named(name)
        .summary(summary)
        .short(short)
        .boolean_flag()
        .optional()
}

fn owner_param() -> crate::function::Param {
    party_param("owner", "Set the bit for the owner (default false)", 'u')
}

fn group_param() -> crate::function::Param {
    party_param("group", "Set the bit for the group (default false)", 'g')
}

fn others_param() -> crate::function::Param {
    party_param("others", "Set the bit for others (default false)", 'o')
}

fn all_param() -> crate::function::Param {
    party_param(
        "all",
        "Set the bit for all (owner, group and others) (default false)",
        'a',
    )
}

fn value_param() -> crate::function::Param {
    crate::function::Param::named("value")
        .summary("The value for the bit (default true)")
        .default_value(|| Value::Bool(true))
}

/// Which parties' bits to change (Scala `SetExecutableMethod.getParties`): `bit` is 4 (read), 2 (write) or
/// 1 (execute); returns the mask to change
fn party_mask(bit: u32, owner: Option<bool>, group: Option<bool>, others: Option<bool>, all: Option<bool>) -> u32 {
    let none_given = owner.is_none() && group.is_none() && others.is_none() && all.is_none();
    let all = all.unwrap_or(false);
    let owner = all || owner.unwrap_or(false) || none_given;
    let group = all || group.unwrap_or(false);
    let others = all || others.unwrap_or(false);
    (u32::from(owner) * (bit << 6)) | (u32::from(group) * (bit << 3)) | (u32::from(others) * bit)
}

/// Set or clear permission bits, returning the new permissions
fn set_bits(ctx: &mut Ctx, target: &Value, mask: u32, value: bool) -> R<Value> {
    let path = target_path(target)?;
    let current = ctx
        .platform()
        .metadata(fs(&path))
        .map_err(|e| io_error("read", &path, &e))?
        .permissions;
    let new = if value { current | mask } else { current & !mask };
    ctx.platform()
        .set_permissions(fs(&path), new)
        .map_err(|e| io_error("set permissions of", &path, &e))?;
    let metadata = path_metadata(ctx, &path)?;
    Ok(permissions_object(metadata.permissions))
}

/// Set the execute/search bit on the given path
///
/// If none of the --owner, --group, --others or --all flags are specified, then the owner bit is changed.
///
/// Returns the updated permissions for the path.
///
/// Examples:
/// <mash>
///   path.setExecutable                  # Set permission for the owner to true
///   path.setExecutable false            # Set permission for the owner to false
///   path.setExecutable --group --others # Set permission for group and others to true
///   path.setExecutable -og              # Set permission for group and others to true
///   path.setExecutable --all false      # Set permission for everyone to false
/// </mash>
#[builtin_method(returns = Type::Instance(&PERMISSIONS_CLASS))]
fn set_executable(
    ctx: &mut Ctx,
    target: &Value,
    #[param(def = owner_param)] owner: Option<bool>,
    #[param(def = group_param)] group: Option<bool>,
    #[param(def = others_param)] others: Option<bool>,
    #[param(def = all_param)] all: Option<bool>,
    #[param(def = value_param)] value: bool,
) -> R<Value> {
    set_bits(ctx, target, party_mask(1, owner, group, others, all), value)
}

/// Set the read bit on the given path
///
/// If none of the --owner, --group, --others or --all flags are specified, then the owner bit is changed.
///
/// Returns the updated permissions for the path.
///
/// Examples:
/// <mash>
///   path.setReadable                  # Set permission for the owner to true
///   path.setReadable false            # Set permission for the owner to false
///   path.setReadable --group --others # Set permission for group and others to true
///   path.setReadable -og              # Set permission for group and others to true
///   path.setReadable --all false      # Set permission for everyone to false
/// </mash>
#[builtin_method(returns = Type::Instance(&PERMISSIONS_CLASS))]
fn set_readable(
    ctx: &mut Ctx,
    target: &Value,
    #[param(def = owner_param)] owner: Option<bool>,
    #[param(def = group_param)] group: Option<bool>,
    #[param(def = others_param)] others: Option<bool>,
    #[param(def = all_param)] all: Option<bool>,
    #[param(def = value_param)] value: bool,
) -> R<Value> {
    set_bits(ctx, target, party_mask(4, owner, group, others, all), value)
}

/// Set the write bit on the given path
///
/// If none of the --owner, --group, --others or --all flags are specified, then the owner bit is changed.
///
/// Returns the updated permissions for the path.
///
/// Examples:
/// <mash>
///   path.setWritable                  # Set permission for the owner to true
///   path.setWritable false            # Set permission for the owner to false
///   path.setWritable --group --others # Set permission for group and others to true
///   path.setWritable -og              # Set permission for group and others to true
///   path.setWritable --all false      # Set permission for everyone to false
/// </mash>
#[builtin_method(returns = Type::Instance(&PERMISSIONS_CLASS))]
fn set_writable(
    ctx: &mut Ctx,
    target: &Value,
    #[param(def = owner_param)] owner: Option<bool>,
    #[param(def = group_param)] group: Option<bool>,
    #[param(def = others_param)] others: Option<bool>,
    #[param(def = all_param)] all: Option<bool>,
    #[param(def = value_param)] value: bool,
) -> R<Value> {
    set_bits(ctx, target, party_mask(2, owner, group, others, all), value)
}

// ------------------------------------------------------------------------------------------------ PathSummary

/// A `Path` method as a method of `PathSummary`, applied to the summary's `path` (Scala
/// `PathSummaryClass.liftPathMethod`).
pub(crate) struct Lifted(pub &'static dyn BuiltinMethod);

impl std::fmt::Debug for Lifted {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Lifted({})", self.0.name())
    }
}

impl BuiltinMethod for Lifted {
    fn name(&self) -> &'static str {
        self.0.name()
    }
    fn aliases(&self) -> &'static [&'static str] {
        self.0.aliases()
    }
    fn params(&self) -> &Params {
        self.0.params()
    }
    fn call(&self, ctx: &mut Ctx, target: &Value, args: &Args) -> R<Value> {
        let path = match target {
            Value::Object(o) => o.get_str("path"),
            _ => None,
        }
        .ok_or_else(|| MashError::new("PathSummary has no 'path' field"))?;
        self.0.call(ctx, &path, args)
    }
    fn infer(&self, inferencer: &mut Inferencer<'_>, _target: Option<&Type>, args: &TypedArgs) -> Option<Type> {
        self.0.infer(inferencer, Some(&Type::PATH), args)
    }
    fn completion_specs(&self, arg_pos: usize, _target: Option<&Type>, args: &TypedArgs) -> Vec<CompletionSpec> {
        self.0.completion_specs(arg_pos, Some(&Type::PATH), args)
    }
    fn summary(&self) -> &'static str {
        self.0.summary()
    }
    fn description(&self) -> Option<&'static str> {
        self.0.description()
    }
}

/// Methods of `os.PathSummary`: the `Path` methods, except those that duplicate its fields, and `info`
pub(crate) const SUMMARY_METHODS: &[&dyn BuiltinMethod] = &[
    &Lifted(&AbsoluteMethod),
    &Lifted(&BaseNameMethod),
    &Lifted(&ChangeDirectoryMethod),
    &Lifted(&ChildrenMethod),
    &Lifted(&CopyIntoMethod),
    &Lifted(&CopyMethod),
    &Lifted(&DeleteMethod),
    &Lifted(&ExistsMethod),
    &Lifted(&ExtensionMethod),
    &Lifted(&FollowLinkMethod),
    &Lifted(&IsDirectoryMethod),
    &Lifted(&IsEmptyDirMethod),
    &Lifted(&IsFileMethod),
    &Lifted(&CreateDirectoryMethod),
    &Lifted(&MoveIntoMethod),
    &Lifted(&NameMethod),
    &Lifted(&super::system::PathOpenMethod),
    &Lifted(&ParentMethod),
    &Lifted(&ReadMethod),
    &Lifted(&ReadLinesMethod),
    &Lifted(&RenameByMethod),
    &Lifted(&RenameToMethod),
    &Lifted(&super::process::PathRunMethod),
    &Lifted(&SegmentsMethod),
    &Lifted(&SetExecutableMethod),
    &Lifted(&SetReadableMethod),
    &Lifted(&SetWritableMethod),
    &Lifted(&WithTildeMethod),
    &Lifted(&WriteMethod),
];
