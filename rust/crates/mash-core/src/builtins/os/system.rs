//! The clipboard, opening things with the default application, disk space and the terminal (Scala
//! `ClipboardFunction`, `OpenFunction`, `pathClass.OpenMethod`, `DiskSpaceFunction`, `TerminalFunction`).

use mash_macros::{builtin, builtin_method};

use crate::eval::{Ctx, MashError, MemberLookup, R};
use crate::function::Params;
use crate::infer::Type;
use crate::platform::DiskSpace;
use crate::value::{BuiltinClass, ClassRef, Field, List, Number, Object, Value};

use super::super::constructor::FieldsConstructor;
use super::super::core::{BYTES_CLASS, OBJECT_CLASS};
use super::paths::{describe, path_value};

pub static DISK_SPACE_ENTRY_CLASS: BuiltinClass = BuiltinClass {
    summary: "Information about disk space use for a partition",
    fields: &[
        Field {
            name: "filesystem",
            summary: "Name of the filesystem",
            ty: || Type::PATH,
        },
        Field {
            name: "total",
            summary: "Total space",
            ty: || Type::tagged_number(&BYTES_CLASS),
        },
        Field {
            name: "used",
            summary: "Used",
            ty: || Type::tagged_number(&BYTES_CLASS),
        },
        Field {
            name: "available",
            summary: "Available",
            ty: || Type::tagged_number(&BYTES_CLASS),
        },
        Field {
            name: "mountPoint",
            summary: "Mount point",
            ty: || Type::PATH,
        },
    ],
    ..BuiltinClass::new("os", "DiskSpaceEntry", Some(&OBJECT_CLASS))
};

pub static TERMINAL_CLASS: BuiltinClass = BuiltinClass {
    summary: "Information about the terminal",
    fields: &[
        Field {
            name: "rows",
            summary: "Number of rows in the terminal",
            ty: || Type::NUMBER,
        },
        Field {
            name: "columns",
            summary: "Number of columns in the terminal",
            ty: || Type::NUMBER,
        },
    ],
    static_methods: &[&TERMINAL_NEW],
    ..BuiltinClass::new("os", "Terminal", Some(&OBJECT_CLASS))
};

static TERMINAL_NEW: FieldsConstructor =
    FieldsConstructor::new("os.Terminal", &TERMINAL_CLASS, "Construct a new Terminal object");

/// Interact with the copy/paste clipboard
#[builtin(namespace = "os", returns = Type::STRING)]
pub(crate) fn clipboard(
    ctx: &mut Ctx,
    #[param(
        "Item to place on the clipboard. If not provided, the contents of the clipboard are returned.",
        optional
    )]
    item: Option<Value>,
) -> R<Value> {
    let error = |e: std::io::Error| MashError::new(format!("Could not use the clipboard: {}", describe(&e)));
    let text = match item {
        None => return ctx.platform().clipboard().map(Value::from).map_err(error),
        Some(Value::List(items)) => {
            let mut lines = vec![];
            for item in items.snapshot() {
                lines.push(ctx.stringify(&item)?);
            }
            lines.join("\n")
        }
        Some(item) => ctx.stringify(&item)?,
    };
    ctx.platform().set_clipboard(&text).map_err(error)?;
    Ok(Value::Unit)
}

/// Open something with the system's default application
fn open_with_system_opener(ctx: &mut Ctx, item: &Value) -> R<()> {
    let argument = ctx.stringify(item)?;
    ctx.platform()
        .open_with_default_application(&argument)
        .map_err(|e| MashError::new(format!("Could not open '{argument}': {}", describe(&e))))
}

/// Open a given item with the default application
///
/// An item with an `open` method (e.g. a path) is opened with that; anything else is converted to a string
/// and given to the system's opener.
#[builtin(namespace = "os", returns = Type::UNIT)]
pub(crate) fn open(ctx: &mut Ctx, #[param("Items to open", variadic, at_least_one, flatten)] items: List) -> R<()> {
    for item in items.snapshot() {
        let method = ctx
            .lookup_member(&item, "open", MemberLookup::DEFAULT)
            .filter(|m| ctx.params_of(m).is_some_and(Params::allows_nullary));
        match method {
            Some(method) => {
                ctx.call_values(&method, vec![])?;
            }
            None => open_with_system_opener(ctx, &item)?,
        }
    }
    Ok(())
}

/// Open this path with the default application
#[builtin_method(name = "open", returns = Type::UNIT)]
pub(crate) fn path_open(ctx: &mut Ctx, target: &Value) -> R<()> {
    open_with_system_opener(ctx, target)
}

fn bytes(n: u64) -> Value {
    Value::Number(Number::new(n as f64), Some(&BYTES_CLASS))
}

fn disk_space_entry(disk: &DiskSpace) -> Value {
    let object = Object::with_class(Some(ClassRef::Builtin(&DISK_SPACE_ENTRY_CLASS)));
    object.set_str("filesystem", path_value(disk.filesystem.as_str()));
    object.set_str("total", bytes(disk.total));
    object.set_str("used", bytes(disk.used));
    object.set_str("available", bytes(disk.available));
    object.set_str("mountPoint", path_value(disk.mount_point.as_str()));
    Value::Object(object)
}

/// Information about disk space on various partitions
#[builtin(namespace = "os", returns = Type::seq(Type::Instance(&DISK_SPACE_ENTRY_CLASS)))]
pub(crate) fn disk_space(ctx: &mut Ctx) -> R<List> {
    let disks = ctx
        .platform()
        .disk_space()
        .map_err(|e| MashError::new(format!("Could not find disk space: {}", describe(&e))))?;
    Ok(List::new(disks.iter().map(disk_space_entry).collect()))
}

/// Return information about the current terminal
#[builtin(namespace = "os", returns = Type::Instance(&TERMINAL_CLASS))]
pub(crate) fn terminal(ctx: &mut Ctx) -> R<Value> {
    let size = ctx
        .platform()
        .terminal_size()
        .ok_or_else(|| MashError::new("Not running in a terminal"))?;
    let object = Object::with_class(Some(ClassRef::Builtin(&TERMINAL_CLASS)));
    object.set_str("rows", Value::num(f64::from(size.rows)));
    object.set_str("columns", Value::num(f64::from(size.columns)));
    Ok(Value::Object(object))
}
