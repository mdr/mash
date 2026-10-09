//! Users and groups: `user`, `users`, `groups`, the `UserSummary` and `GroupInfo` classes, and the methods of
//! the `Username`, `Uid`, `Group` and `Gid` tag classes (Scala `UserFunction`, `UserSummaryClass`,
//! `UsernameClass` …).

use mash_macros::{builtin, builtin_method};

use crate::eval::{Ctx, MashError, R};
use crate::function::BuiltinMethod;
use crate::infer::Type;
use crate::platform::{GroupEntry, Platform, UserEntry};
use crate::value::{BuiltinClass, ClassRef, Field, List, Number, Object, Str, Value};

use super::super::constructor::FieldsConstructor;
use super::super::core::{ANY_CLASS, OBJECT_CLASS};
use super::lifted::{InfoMethod, LiftedField, LiftedMethod};
use super::paths::{describe, path_value};
use super::{GROUP_CLASS, USERNAME_CLASS};

pub static USER_SUMMARY_CLASS: BuiltinClass = BuiltinClass {
    summary: "A user",
    fields: &[
        Field {
            name: "name",
            summary: "Name of user",
            ty: || Type::tagged_string(&USERNAME_CLASS),
        },
        Field {
            name: "uid",
            summary: "Id of user (UID)",
            ty: || Type::tagged_number(&UID_CLASS),
        },
        Field {
            name: "primaryGroup",
            summary: "Primary group to which the user belongs",
            ty: || Type::tagged_string(&GROUP_CLASS),
        },
        Field {
            name: "home",
            summary: "Home directory of this user",
            ty: || Type::PATH,
        },
        Field {
            name: "shell",
            summary: "Shell used by this user",
            ty: || Type::PATH,
        },
    ],
    methods: &[&FullNameMethod, &UserGroupsMethod],
    static_methods: &[&USER_SUMMARY_NEW],
    ..BuiltinClass::new("os", "UserSummary", Some(&OBJECT_CLASS))
};

pub static GROUP_INFO_CLASS: BuiltinClass = BuiltinClass {
    summary: "A user group",
    fields: &[
        Field {
            name: "name",
            summary: "Name of the group",
            ty: || Type::tagged_string(&GROUP_CLASS),
        },
        Field {
            name: "gid",
            summary: "Id of the group",
            ty: || Type::tagged_number(&GID_CLASS),
        },
        Field {
            name: "users",
            summary: "Users in the group",
            ty: || Type::seq(Type::tagged_string(&USERNAME_CLASS)),
        },
    ],
    static_methods: &[&GROUP_INFO_NEW],
    ..BuiltinClass::new("os", "GroupInfo", Some(&OBJECT_CLASS))
};

pub static UID_CLASS: BuiltinClass = BuiltinClass {
    summary: "Tag class for user ID (UID)",
    methods: UID_METHODS,
    ..BuiltinClass::new("os", "Uid", Some(&ANY_CLASS))
};

pub static GID_CLASS: BuiltinClass = BuiltinClass {
    summary: "Tag class for a group ID (GID)",
    methods: &[&GidNameMethod, &GidUsersMethod],
    ..BuiltinClass::new("os", "Gid", Some(&ANY_CLASS))
};

static USER_SUMMARY_NEW: FieldsConstructor = FieldsConstructor::new(
    "os.UserSummary",
    &USER_SUMMARY_CLASS,
    "Construct a new UserSummary object",
);
static GROUP_INFO_NEW: FieldsConstructor =
    FieldsConstructor::new("os.GroupInfo", &GROUP_INFO_CLASS, "Construct a new GroupInfo object");

/// Methods of `os.Username`: `info`, then `UserSummary`'s fields and methods
pub(crate) static USERNAME_METHODS: &[&dyn BuiltinMethod] = &[
    &InfoMethod {
        class: &USER_SUMMARY_CLASS,
        summary: "Fetch information about the user with this username",
        resolve: user_by_name,
    },
    &by_name("name"),
    &by_name("uid"),
    &by_name("primaryGroup"),
    &by_name("home"),
    &by_name("shell"),
    &LiftedMethod {
        class: &USER_SUMMARY_CLASS,
        method: &FullNameMethod,
        resolve: user_by_name,
    },
    &LiftedMethod {
        class: &USER_SUMMARY_CLASS,
        method: &UserGroupsMethod,
        resolve: user_by_name,
    },
];

/// Methods of `os.Uid`: `info`, then `UserSummary`'s fields and methods
static UID_METHODS: &[&dyn BuiltinMethod] = &[
    &InfoMethod {
        class: &USER_SUMMARY_CLASS,
        summary: "Fetch information about the user with this uid",
        resolve: user_by_uid,
    },
    &by_uid("name"),
    &by_uid("uid"),
    &by_uid("primaryGroup"),
    &by_uid("home"),
    &by_uid("shell"),
    &LiftedMethod {
        class: &USER_SUMMARY_CLASS,
        method: &FullNameMethod,
        resolve: user_by_uid,
    },
    &LiftedMethod {
        class: &USER_SUMMARY_CLASS,
        method: &UserGroupsMethod,
        resolve: user_by_uid,
    },
];

/// Methods of `os.Group`
pub(crate) static GROUP_METHODS: &[&dyn BuiltinMethod] = &[&GroupGidMethod, &GroupUsersMethod];

const fn by_name(name: &'static str) -> LiftedField {
    LiftedField {
        class: &USER_SUMMARY_CLASS,
        name,
        resolve: user_by_name,
    }
}

const fn by_uid(name: &'static str) -> LiftedField {
    LiftedField {
        class: &USER_SUMMARY_CLASS,
        name,
        resolve: user_by_uid,
    }
}

// ------------------------------------------------------------------------------------------------ data

fn all_users(platform: &dyn Platform) -> R<Vec<UserEntry>> {
    platform
        .users()
        .map_err(|e| MashError::new(format!("Could not read the users: {}", describe(&e))))
}

fn all_groups(platform: &dyn Platform) -> R<Vec<GroupEntry>> {
    platform
        .groups()
        .map_err(|e| MashError::new(format!("Could not read the groups: {}", describe(&e))))
}

/// Usernames, sorted (the values of `os.Username`, for completion)
pub(crate) fn usernames(platform: &dyn Platform) -> Vec<String> {
    let mut names: Vec<String> = platform
        .users()
        .unwrap_or_default()
        .into_iter()
        .map(|u| u.name)
        .collect();
    names.sort();
    names
}

/// Group names, sorted (the values of `os.Group`, for completion)
pub(crate) fn group_names(platform: &dyn Platform) -> Vec<String> {
    let mut names: Vec<String> = platform
        .groups()
        .unwrap_or_default()
        .into_iter()
        .map(|g| g.name)
        .collect();
    names.sort();
    names
}

fn username(name: &str) -> Value {
    Value::Str(Str::tagged(name, &USERNAME_CLASS))
}

fn group_name(name: &str) -> Value {
    Value::Str(Str::tagged(name, &GROUP_CLASS))
}

/// An `os.UserSummary` object
fn user_summary(user: &UserEntry, groups: &[GroupEntry]) -> Value {
    let primary_group = groups
        .iter()
        .find(|g| g.gid == user.gid)
        .map_or(Value::Null, |g| group_name(&g.name));
    let object = Object::with_class(Some(ClassRef::Builtin(&USER_SUMMARY_CLASS)));
    object.set_str("name", username(&user.name));
    object.set_str("uid", Value::Number(Number::new(f64::from(user.uid)), Some(&UID_CLASS)));
    object.set_str("primaryGroup", primary_group);
    object.set_str("home", path_value(user.home.as_str()));
    object.set_str("shell", path_value(user.shell.as_str()));
    Value::Object(object)
}

/// The users in a group: those for whom it's the primary group, then its other members (each once)
fn group_members(group: &GroupEntry, users: &[UserEntry]) -> Vec<Value> {
    let primary = users.iter().filter(|u| u.gid == group.gid).map(|u| u.name.as_str());
    let secondary = group.members.iter().map(String::as_str);
    let mut names: Vec<&str> = vec![];
    for name in primary.chain(secondary) {
        if !names.contains(&name) {
            names.push(name);
        }
    }
    names.into_iter().map(username).collect()
}

/// An `os.GroupInfo` object
fn group_info(group: &GroupEntry, users: &[UserEntry]) -> Value {
    let object = Object::with_class(Some(ClassRef::Builtin(&GROUP_INFO_CLASS)));
    object.set_str("name", group_name(&group.name));
    object.set_str("gid", gid_value(group.gid));
    object.set_str("users", Value::list(group_members(group, users)));
    Value::Object(object)
}

fn gid_value(gid: u32) -> Value {
    Value::Number(Number::new(f64::from(gid)), Some(&GID_CLASS))
}

/// The `UserSummary` for a username
fn user_by_name(ctx: &mut Ctx, target: &Value) -> R<Value> {
    let name = target
        .as_str()
        .ok_or_else(|| MashError::new(format!("Invalid username: {}", target.type_name())))?;
    let user = all_users(ctx.platform())?
        .into_iter()
        .find(|u| u.name == name)
        .ok_or_else(|| MashError::new(format!("Could not find user '{name}'")))?;
    Ok(user_summary(&user, &all_groups(ctx.platform())?))
}

/// A UID or GID given as a (tagged) number
fn id_of(target: &Value, what: &str) -> R<u32> {
    target
        .as_number()
        .and_then(Number::as_i64)
        .and_then(|n| u32::try_from(n).ok())
        .ok_or_else(|| MashError::new(format!("Invalid {what}: {}", crate::eval::render(target))))
}

/// The `UserSummary` for a UID
fn user_by_uid(ctx: &mut Ctx, target: &Value) -> R<Value> {
    let uid = id_of(target, "uid")?;
    let user = all_users(ctx.platform())?
        .into_iter()
        .find(|u| u.uid == uid)
        .ok_or_else(|| MashError::new(format!("Could not find user with uid {uid}")))?;
    Ok(user_summary(&user, &all_groups(ctx.platform())?))
}

/// A `UserSummary`'s `name` field
fn summary_username(target: &Value) -> R<String> {
    match target {
        Value::Object(o) => match o.get_str("name") {
            Some(Value::Str(s)) => Ok(s.as_str().to_string()),
            _ => Err(MashError::new("Field 'name' must be a String")),
        },
        other => Err(MashError::new(format!(
            "target is of type {}, not Object",
            other.type_name()
        ))),
    }
}

// ------------------------------------------------------------------------------------------------ functions

/// The current user
#[builtin(namespace = "os", returns = Type::Instance(&USER_SUMMARY_CLASS))]
pub(crate) fn user(ctx: &mut Ctx) -> R<Value> {
    let name = ctx
        .platform()
        .current_user()
        .map_err(|e| MashError::new(format!("Could not find the current user: {}", describe(&e))))?;
    let user = all_users(ctx.platform())?
        .into_iter()
        .find(|u| u.name == name)
        .ok_or_else(|| MashError::new(format!("Could not find full user information for user '{name}'")))?;
    Ok(user_summary(&user, &all_groups(ctx.platform())?))
}

/// The users on the system
#[builtin(namespace = "os", returns = Type::seq(Type::Instance(&USER_SUMMARY_CLASS)))]
pub(crate) fn users(ctx: &mut Ctx) -> R<List> {
    let groups = all_groups(ctx.platform())?;
    let users = all_users(ctx.platform())?;
    Ok(List::new(users.iter().map(|u| user_summary(u, &groups)).collect()))
}

/// User groups on the system
#[builtin(namespace = "os", returns = Type::seq(Type::Instance(&GROUP_INFO_CLASS)))]
pub(crate) fn groups(ctx: &mut Ctx) -> R<List> {
    let users = all_users(ctx.platform())?;
    let groups = all_groups(ctx.platform())?;
    Ok(List::new(groups.iter().map(|g| group_info(g, &users)).collect()))
}

// ------------------------------------------------------------------------------------------------ methods

/// Full name of this user
#[builtin_method(returns = Type::STRING)]
fn full_name(ctx: &mut Ctx, target: &Value) -> R<Value> {
    let name = summary_username(target)?;
    let full_name = all_users(ctx.platform())?
        .into_iter()
        .find(|u| u.name == name)
        .and_then(|u| u.gecos.split(',').next().map(str::to_string));
    Ok(full_name.map_or(Value::Null, Value::from))
}

/// Groups this user is a member of
#[builtin_method(name = "groups", returns = Type::seq(Type::tagged_string(&GROUP_CLASS)))]
fn user_groups(ctx: &mut Ctx, target: &Value) -> R<List> {
    let name = summary_username(target)?;
    let primary = match target {
        Value::Object(o) => o.get_str("primaryGroup").filter(|g| !g.is_null()),
        _ => None,
    };
    let primary_name = primary.as_ref().and_then(|g| g.as_str().map(str::to_string));
    let mut secondary: Vec<String> = vec![];
    for group in all_groups(ctx.platform())? {
        if group.members.contains(&name)
            && primary_name.as_ref() != Some(&group.name)
            && !secondary.contains(&group.name)
        {
            secondary.push(group.name);
        }
    }
    let secondary = secondary.iter().map(|g| group_name(g));
    Ok(List::new(primary.into_iter().chain(secondary).collect()))
}

/// The group with this name, if there is one
fn find_group(ctx: &Ctx, target: &Value) -> R<Option<GroupEntry>> {
    let name = target
        .as_str()
        .ok_or_else(|| MashError::new(format!("Invalid group: {}", target.type_name())))?;
    Ok(all_groups(ctx.platform())?.into_iter().find(|g| g.name == name))
}

/// Id of this group (GID)
#[builtin_method(name = "gid", returns = Type::tagged_number(&GID_CLASS))]
fn group_gid(ctx: &mut Ctx, target: &Value) -> R<Value> {
    Ok(find_group(ctx, target)?.map_or(Value::Null, |g| gid_value(g.gid)))
}

/// Users within this group
#[builtin_method(name = "users", returns = Type::seq(Type::tagged_string(&USERNAME_CLASS)))]
fn group_users(ctx: &mut Ctx, target: &Value) -> R<List> {
    let members = match find_group(ctx, target)? {
        Some(group) => group_members(&group, &all_users(ctx.platform())?),
        None => vec![],
    };
    Ok(List::new(members))
}

/// The group with a GID
fn group_by_gid(ctx: &Ctx, target: &Value) -> R<GroupEntry> {
    let gid = id_of(target, "gid")?;
    all_groups(ctx.platform())?
        .into_iter()
        .find(|g| g.gid == gid)
        .ok_or_else(|| MashError::new(format!("Could not find group with gid {gid}")))
}

/// Name of the group with this id
#[builtin_method(name = "name", returns = Type::tagged_string(&GROUP_CLASS))]
fn gid_name(ctx: &mut Ctx, target: &Value) -> R<Value> {
    Ok(group_name(&group_by_gid(ctx, target)?.name))
}

/// Users within this group
#[builtin_method(name = "users", returns = Type::seq(Type::tagged_string(&USERNAME_CLASS)))]
fn gid_users(ctx: &mut Ctx, target: &Value) -> R<List> {
    let group = group_by_gid(ctx, target)?;
    Ok(List::new(group_members(&group, &all_users(ctx.platform())?)))
}
