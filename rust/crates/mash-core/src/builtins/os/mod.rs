//! The `os` namespace: files, directories, the working directory, and running external programs. Everything
//! goes through the [`Platform`](crate::platform::Platform), so it works (and is tested) with an in-memory
//! file system as well as natively.

mod dirs;
mod files;
mod glob;
mod path_methods;
pub(crate) mod paths;
pub(crate) mod process;

use crate::function::BuiltinFunction;
use crate::infer::Type;
use crate::value::{BuiltinClass, Field};

use super::constructor::FieldsConstructor;
use super::core::{ANY_CLASS, OBJECT_CLASS};

pub use dirs::WorkingDirectoryStack;
pub(crate) use dirs::mish_cd;
pub(crate) use glob::StringGlobMethod;
pub use paths::{PathArg, PathArgs};

/// Functions in `os` (all in the global scope)
pub(crate) const FUNCTIONS: &[&dyn BuiltinFunction] = &[
    &dirs::BackFunction,
    &dirs::ChangeDirectoryFunction,
    &files::ChildrenFunction,
    &files::CopyFunction,
    &files::CreateDirectoryFunction,
    &files::CreateTempDirectoryFunction,
    &files::CreateTempFileFunction,
    &dirs::CurrentDirectoryFunction,
    &files::DeleteFunction,
    &files::FindExecutableInPathFunction,
    &dirs::ForwardFunction,
    &glob::GlobFunction,
    &dirs::HomeFunction,
    &files::ListFilesFunction,
    &files::MoveFunction,
    &dirs::OldDirsFunction,
    &files::ReadFunction,
    &files::ReadLinesFunction,
    &process::RunFunction,
    &dirs::UpFunction,
    &dirs::WithinFunction,
    &files::WriteFunction,
];

/// Short global names for `os` functions (Scala `MashRoot.Aliases`)
pub(crate) fn aliases() -> Vec<(&'static str, &'static dyn BuiltinFunction)> {
    vec![
        ("cat", &files::ReadLinesFunction),
        ("cd", &dirs::ChangeDirectoryFunction),
        ("cp", &files::CopyFunction),
        ("ls", &files::ListFilesFunction),
        ("mkdir", &files::CreateDirectoryFunction),
        ("mv", &files::MoveFunction),
        ("pwd", &dirs::CurrentDirectoryFunction),
        ("rm", &files::DeleteFunction),
        ("which", &files::FindExecutableInPathFunction),
    ]
}

/// Classes in `os`
pub(crate) const CLASSES: &[&BuiltinClass] = &[
    &FILE_TYPE_CLASS,
    &GROUP_CLASS,
    &PATH_CLASS,
    &PATH_SUMMARY_CLASS,
    &PERMISSIONS_CLASS,
    &PERMISSIONS_SECTION_CLASS,
    &PROCESS_RESULT_CLASS,
    &USERNAME_CLASS,
];

pub static PATH_CLASS: BuiltinClass = BuiltinClass {
    summary: "Tag class for a filesystem path",
    methods: path_methods::METHODS,
    ..BuiltinClass::new("os", "Path", Some(&ANY_CLASS))
};

pub static FILE_TYPE_CLASS: BuiltinClass = BuiltinClass {
    summary: "Type of filesystem object",
    enumeration: &["dir", "file", "link", "other"],
    ..BuiltinClass::new("os", "FileType", Some(&ANY_CLASS))
};

/// Tag class for user names (methods about the user arrive with the users namespace)
pub static USERNAME_CLASS: BuiltinClass = BuiltinClass {
    summary: "A username",
    ..BuiltinClass::new("os", "Username", Some(&ANY_CLASS))
};

/// Tag class for group names (methods about the group arrive with the users namespace)
pub static GROUP_CLASS: BuiltinClass = BuiltinClass {
    summary: "A tag class for user group names",
    ..BuiltinClass::new("os", "Group", Some(&ANY_CLASS))
};

pub static PATH_SUMMARY_CLASS: BuiltinClass = BuiltinClass {
    summary: "Information about a filesystem path",
    fields: &[
        Field {
            name: "path",
            summary: "Path",
            ty: || Type::PATH,
        },
        Field {
            name: "type",
            summary: "Type (file, dir, link)",
            ty: || Type::tagged_string(&FILE_TYPE_CLASS),
        },
        Field {
            name: "size",
            summary: "Size in bytes",
            ty: || Type::tagged_number(&super::core::BYTES_CLASS),
        },
        Field {
            name: "owner",
            summary: "User owner",
            ty: || Type::tagged_string(&USERNAME_CLASS),
        },
        Field {
            name: "group",
            summary: "Group owner",
            ty: || Type::tagged_string(&GROUP_CLASS),
        },
        Field {
            name: "permissions",
            summary: "Read/write/execute permissions",
            ty: || Type::Instance(&PERMISSIONS_CLASS),
        },
        Field {
            name: "lastModified",
            summary: "Last time path was modified",
            ty: || Type::Instance(&super::time::DATE_TIME_CLASS),
        },
    ],
    methods: path_methods::SUMMARY_METHODS,
    static_methods: &[&PATH_SUMMARY_NEW],
    ..BuiltinClass::new("os", "PathSummary", Some(&OBJECT_CLASS))
};

pub static PERMISSIONS_CLASS: BuiltinClass = BuiltinClass {
    summary: "File permissions",
    fields: &[
        Field {
            name: "owner",
            summary: "Owner permissions",
            ty: || Type::Instance(&PERMISSIONS_SECTION_CLASS),
        },
        Field {
            name: "group",
            summary: "Group permissions",
            ty: || Type::Instance(&PERMISSIONS_SECTION_CLASS),
        },
        Field {
            name: "others",
            summary: "Others permissions",
            ty: || Type::Instance(&PERMISSIONS_SECTION_CLASS),
        },
    ],
    static_methods: &[&PERMISSIONS_NEW],
    ..BuiltinClass::new("os", "Permissions", Some(&OBJECT_CLASS))
};

pub static PERMISSIONS_SECTION_CLASS: BuiltinClass = BuiltinClass {
    summary: "File permissions for particular class of user (owner, group or other)",
    fields: &[
        Field {
            name: "canRead",
            summary: "Can read",
            ty: || Type::BOOLEAN,
        },
        Field {
            name: "canWrite",
            summary: "Can write",
            ty: || Type::BOOLEAN,
        },
        Field {
            name: "canExecute",
            summary: "Can execute",
            ty: || Type::BOOLEAN,
        },
    ],
    static_methods: &[&PERMISSIONS_SECTION_NEW],
    ..BuiltinClass::new("os", "PermissionsSection", Some(&OBJECT_CLASS))
};

pub static PROCESS_RESULT_CLASS: BuiltinClass = BuiltinClass {
    summary: "The result of running a process",
    fields: &[
        Field {
            name: "exitStatus",
            summary: "Exit status of process",
            ty: || Type::NUMBER,
        },
        Field {
            name: "stdout",
            summary: "Captured stdout",
            ty: || Type::STRING,
        },
        Field {
            name: "started",
            summary: "Time process began running",
            ty: || Type::Instance(&super::time::DATE_TIME_CLASS),
        },
        Field {
            name: "finished",
            summary: "Time process finished",
            ty: || Type::Instance(&super::time::DATE_TIME_CLASS),
        },
    ],
    methods: &[
        &process::DurationMethod,
        &process::FailedMethod,
        &process::LineMethod,
        &process::LinesMethod,
        &process::SucceededMethod,
        &process::ToNumberMethod,
        &process::ToPathMethod,
    ],
    static_methods: &[&PROCESS_RESULT_NEW],
    ..BuiltinClass::new("os", "ProcessResult", Some(&OBJECT_CLASS))
};

static PATH_SUMMARY_NEW: FieldsConstructor = FieldsConstructor::new(
    "os.PathSummary",
    &PATH_SUMMARY_CLASS,
    "Construct a new PathSummary object",
);
static PERMISSIONS_NEW: FieldsConstructor = FieldsConstructor::new(
    "os.Permissions",
    &PERMISSIONS_CLASS,
    "Construct a new Permissions object",
);
static PERMISSIONS_SECTION_NEW: FieldsConstructor = FieldsConstructor::new(
    "os.PermissionsSection",
    &PERMISSIONS_SECTION_CLASS,
    "Construct a new PermissionsSection object",
);
static PROCESS_RESULT_NEW: FieldsConstructor = FieldsConstructor::new(
    "os.ProcessResult",
    &PROCESS_RESULT_CLASS,
    "Construct a new ProcessResult object",
);
