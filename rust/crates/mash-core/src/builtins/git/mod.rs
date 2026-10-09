//! The `git` namespace (Scala `ns/git`): the working tree status, commits and the log, branches, tags,
//! remotes, the stash, and pushing, pulling and fetching.
//!
//! Scala used JGit; this runs the `git` command (through the [`Platform`](crate::platform::Platform)), parsing
//! its machine-readable output formats, so everything behaves as the user's own git does (configuration,
//! hooks, credentials). git must be on the `PATH`.
//!
//! Commit hashes, branch, remote and tag names are strings tagged with classes (`git.CommitHash`,
//! `git.branch.LocalBranchName`, …) whose methods look up the full object (Scala `MemberLifter`): e.g.
//! `git.status.branch.log`.

mod branch;
mod functions;
mod lifter;
mod objects;
mod parse;
mod remote;
mod run;
mod stash;
mod tag;

use crate::function::BuiltinFunction;
use crate::infer::Type;
use crate::value::{BuiltinClass, Field};

use super::constructor::FieldsConstructor;
use super::core::{ANY_CLASS, OBJECT_CLASS};
use super::time::DATE_TIME_CLASS;
use lifter::{LiftedField, LiftedMethod};

/// Functions in `git` and its sub-namespaces (Scala `GitNamespace.GitFunctions`)
pub(crate) const FUNCTIONS: &[&dyn BuiltinFunction] = &[
    &functions::AddFunction,
    &functions::AmendFunction,
    &functions::CloneFunction,
    &functions::CommitFunction,
    &functions::FetchFunction,
    &functions::InitFunction,
    &functions::IsRepoFunction,
    &functions::LogFunction,
    &functions::MergeFunction,
    &functions::PullFunction,
    &functions::PushFunction,
    &functions::ReflogFunction,
    &functions::RestoreFunction,
    &functions::StageFunction,
    &functions::StatusFunction,
    &functions::UnstageFunction,
    &branch::CreateFunction,
    &branch::CurrentFunction,
    &branch::DeleteFunction,
    &branch::GetFunction,
    &branch::LocalBranchesFunction,
    &branch::RemoteBranchesFunction,
    &branch::SetCommitFunction,
    &branch::SwitchFunction,
    &remote::ListFunction,
    &stash::ApplyFunction,
    &stash::CreateFunction,
    &tag::CreateFunction,
    &tag::DeleteFunction,
    &tag::ListFunction,
];

/// Classes in `git` and its sub-namespaces
pub(crate) const CLASSES: &[&BuiltinClass] = &[
    &COMMIT_CLASS,
    &COMMIT_HASH_CLASS,
    &IDENTITY_CLASS,
    &FETCH_BRANCH_UPDATE_CLASS,
    &FILE_DIFF_CLASS,
    &BRANCH_CLASS,
    &LOCAL_BRANCH_NAME_CLASS,
    &REFLOG_ENTRY_CLASS,
    &STATUS_CLASS,
    &REMOTE_BRANCH_CLASS,
    &REMOTE_BRANCH_NAME_CLASS,
    &TAG_CLASS,
    &TAG_NAME_CLASS,
    &REMOTE_CLASS,
    &REMOTE_NAME_CLASS,
];

fn commit_hash() -> Type {
    Type::tagged_string(&COMMIT_HASH_CLASS)
}

fn paths() -> Type {
    Type::seq(Type::PATH)
}

const AHEAD_COUNT: Field = Field {
    name: "aheadCount",
    summary: "Number of commits that the local branch is ahead of the remote-tracking branch",
    ty: || Type::NUMBER,
};

const BEHIND_COUNT: Field = Field {
    name: "behindCount",
    summary: "Number of commits that the local branch is behind the remote-tracking branch",
    ty: || Type::NUMBER,
};

// ------------------------------------------------------------------------------------------------ commits

pub static COMMIT_CLASS: BuiltinClass = BuiltinClass {
    summary: "A git commit object",
    fields: &[
        Field {
            name: "hash",
            summary: "Commit hash",
            ty: commit_hash,
        },
        Field {
            name: "commitTime",
            summary: "Commit time",
            ty: || Type::Instance(&DATE_TIME_CLASS),
        },
        Field {
            name: "author",
            summary: "Author of the commit",
            ty: || Type::Instance(&IDENTITY_CLASS),
        },
        Field {
            name: "committer",
            summary: "Committer of the commit",
            ty: || Type::Instance(&IDENTITY_CLASS),
        },
        Field {
            name: "summary",
            summary: "Summary message of the commit",
            ty: || Type::STRING,
        },
        Field {
            name: "message",
            summary: "Commit message",
            ty: || Type::STRING,
        },
        Field {
            name: "parents",
            summary: "Parents of this commit",
            ty: || Type::seq(commit_hash()),
        },
    ],
    methods: &[
        &functions::CommitDiffMethod,
        &functions::CommitIsAncestorOfMethod,
        &functions::CommitParentMethod,
    ],
    static_methods: &[&COMMIT_NEW],
    ..BuiltinClass::new("git", "Commit", Some(&OBJECT_CLASS))
};

/// Tag class for commit hashes: the commit's fields and methods, looked up from the hash
pub static COMMIT_HASH_CLASS: BuiltinClass = BuiltinClass {
    summary: "A git commit SHA-1 hash",
    methods: &[
        &LiftedField::new(&COMMIT_CLASS, "commitTime", objects::commit_by_hash),
        &LiftedField::new(&COMMIT_CLASS, "author", objects::commit_by_hash),
        &LiftedField::new(&COMMIT_CLASS, "committer", objects::commit_by_hash),
        &LiftedField::new(&COMMIT_CLASS, "summary", objects::commit_by_hash),
        &LiftedField::new(&COMMIT_CLASS, "parents", objects::commit_by_hash),
        &LiftedMethod::new(&COMMIT_CLASS, &functions::CommitDiffMethod, objects::commit_by_hash),
        &LiftedMethod::new(
            &COMMIT_CLASS,
            &functions::CommitIsAncestorOfMethod,
            objects::commit_by_hash,
        ),
        &LiftedMethod::new(&COMMIT_CLASS, &functions::CommitParentMethod, objects::commit_by_hash),
        &functions::CommitInfoMethod,
    ],
    ..BuiltinClass::new("git", "CommitHash", Some(&ANY_CLASS))
};

pub static IDENTITY_CLASS: BuiltinClass = BuiltinClass {
    summary: "A combination of a person identity and time in Git",
    fields: &[
        // (Scala names the name field "author")
        Field {
            name: "author",
            summary: "Name",
            ty: || Type::STRING,
        },
        Field {
            name: "email",
            summary: "Email address",
            ty: || Type::STRING,
        },
    ],
    methods: &[&functions::IdentityToStringMethod],
    static_methods: &[&IDENTITY_NEW],
    ..BuiltinClass::new("git", "Identity", Some(&OBJECT_CLASS))
};

pub static FILE_DIFF_CLASS: BuiltinClass = BuiltinClass {
    summary: "Summary of a change to a file",
    fields: &[
        Field {
            name: "type",
            summary: "Type of the change",
            ty: || Type::STRING,
        },
        Field {
            name: "oldPath",
            summary: "Old path",
            ty: || Type::PATH,
        },
        Field {
            name: "newPath",
            summary: "Old path",
            ty: || Type::PATH,
        },
    ],
    ..BuiltinClass::new("git", "FileDiff", Some(&OBJECT_CLASS))
};

pub static REFLOG_ENTRY_CLASS: BuiltinClass = BuiltinClass {
    summary: "An entry from the Git reflog",
    fields: &[
        Field {
            name: "commit",
            summary: "Commit hash",
            ty: commit_hash,
        },
        Field {
            name: "comment",
            summary: "Comment",
            ty: || Type::STRING,
        },
    ],
    ..BuiltinClass::new("git", "ReflogEntry", Some(&OBJECT_CLASS))
};

pub static FETCH_BRANCH_UPDATE_CLASS: BuiltinClass = BuiltinClass {
    summary: "A branch update after a fetch",
    fields: &[
        Field {
            name: "branch",
            summary: "Remote branch",
            ty: || Type::tagged_string(&REMOTE_BRANCH_NAME_CLASS),
        },
        Field {
            name: "oldCommit",
            summary: "Old commit prior to the fetch",
            ty: commit_hash,
        },
        Field {
            name: "newCommit",
            summary: "New commit prior to the fetch",
            ty: commit_hash,
        },
    ],
    static_methods: &[&FETCH_BRANCH_UPDATE_NEW],
    ..BuiltinClass::new("git", "FetchBranchUpdate", Some(&OBJECT_CLASS))
};

pub static STATUS_CLASS: BuiltinClass = BuiltinClass {
    summary: "The status of a git repository",
    fields: &[
        Field {
            name: "branch",
            summary: "Current branch",
            ty: || Type::tagged_string(&LOCAL_BRANCH_NAME_CLASS),
        },
        Field {
            name: "upstreamBranch",
            summary: "The name of the upstream branch (if any, else null)",
            ty: || Type::STRING,
        },
        AHEAD_COUNT,
        BEHIND_COUNT,
        Field {
            name: "added",
            summary: "New files that have been staged",
            ty: paths,
        },
        Field {
            name: "changed",
            summary: "Changed files that have been staged",
            ty: paths,
        },
        Field {
            name: "missing",
            summary: "Files that have been deleted, but not staged",
            ty: paths,
        },
        Field {
            name: "modified",
            summary: "Modified files that have not been staged",
            ty: paths,
        },
        Field {
            name: "removed",
            summary: "Files that have been deleted and staged",
            ty: paths,
        },
        Field {
            name: "untracked",
            summary: "Untracked files",
            ty: paths,
        },
        Field {
            name: "conflicting",
            summary: "Conflicting files",
            ty: paths,
        },
    ],
    methods: &[&functions::IsCleanMethod],
    ..BuiltinClass::new("git", "Status", Some(&OBJECT_CLASS))
};

// ------------------------------------------------------------------------------------------------ branches

pub static BRANCH_CLASS: BuiltinClass = BuiltinClass {
    summary: "A git branch",
    fields: &[
        Field {
            name: "name",
            summary: "Name of the branch",
            ty: || Type::tagged_string(&LOCAL_BRANCH_NAME_CLASS),
        },
        Field {
            name: "commit",
            summary: "The commit the branch is pointing to",
            ty: commit_hash,
        },
        Field {
            name: "upstreamBranch",
            summary: "The upstream branch this branch is tracking, if any, else null",
            ty: || Type::tagged_string(&REMOTE_BRANCH_NAME_CLASS),
        },
        AHEAD_COUNT,
        BEHIND_COUNT,
    ],
    methods: &[
        &branch::BranchDeleteMethod,
        &branch::BranchLogMethod,
        &branch::BranchIsAncestorOfMethod,
        &branch::BranchPushMethod,
        &branch::BranchSetCommitMethod,
        &branch::BranchSwitchMethod,
    ],
    static_methods: &[&BRANCH_NEW],
    ..BuiltinClass::new("git.branch", "Branch", Some(&OBJECT_CLASS))
};

/// Tag class for local branch names: the branch's fields and methods, looked up by name
pub static LOCAL_BRANCH_NAME_CLASS: BuiltinClass = BuiltinClass {
    summary: "A git local branch name",
    methods: &[
        &LiftedField::new(&BRANCH_CLASS, "commit", objects::local_branch_by_name),
        &LiftedField::new(&BRANCH_CLASS, "upstreamBranch", objects::local_branch_by_name),
        &LiftedMethod::new(
            &BRANCH_CLASS,
            &branch::BranchDeleteMethod,
            objects::local_branch_by_name,
        ),
        &LiftedMethod::new(
            &BRANCH_CLASS,
            &branch::BranchIsAncestorOfMethod,
            objects::local_branch_by_name,
        ),
        &LiftedMethod::new(&BRANCH_CLASS, &branch::BranchLogMethod, objects::local_branch_by_name),
        &LiftedMethod::new(
            &BRANCH_CLASS,
            &branch::BranchSetCommitMethod,
            objects::local_branch_by_name,
        ),
        &LiftedMethod::new(
            &BRANCH_CLASS,
            &branch::BranchSwitchMethod,
            objects::local_branch_by_name,
        ),
        &LiftedMethod::new(&BRANCH_CLASS, &branch::BranchPushMethod, objects::local_branch_by_name),
        &branch::LocalBranchInfoMethod,
    ],
    ..BuiltinClass::new("git.branch", "LocalBranchName", Some(&ANY_CLASS))
};

pub static REMOTE_BRANCH_CLASS: BuiltinClass = BuiltinClass {
    summary: "A remote git branch",
    fields: &[
        Field {
            name: "remote",
            summary: "Name of the remote this branch is in",
            ty: || Type::tagged_string(&REMOTE_NAME_CLASS),
        },
        Field {
            name: "name",
            summary: "Name of the remote this branch is in",
            ty: || Type::STRING,
        },
        Field {
            name: "commit",
            summary: "The commit the branch is pointing to",
            ty: commit_hash,
        },
    ],
    methods: &[
        &branch::CreateLocalMethod,
        &branch::RemoteBranchDeleteMethod,
        &branch::FullNameMethod,
        &branch::RemoteBranchIsAncestorOfMethod,
        &branch::RemoteBranchLogMethod,
    ],
    static_methods: &[&REMOTE_BRANCH_NEW],
    ..BuiltinClass::new("git.branch", "RemoteBranch", Some(&OBJECT_CLASS))
};

/// Tag class for remote-tracking branch names (`origin/master`): the branch's fields and methods, looked up by
/// name
pub static REMOTE_BRANCH_NAME_CLASS: BuiltinClass = BuiltinClass {
    summary: "A git remote branch name",
    methods: &[
        &LiftedField::new(&REMOTE_BRANCH_CLASS, "remote", objects::remote_branch_by_name),
        &LiftedField::new(&REMOTE_BRANCH_CLASS, "name", objects::remote_branch_by_name),
        &LiftedField::new(&REMOTE_BRANCH_CLASS, "commit", objects::remote_branch_by_name),
        &LiftedMethod::new(
            &REMOTE_BRANCH_CLASS,
            &branch::CreateLocalMethod,
            objects::remote_branch_by_name,
        ),
        &LiftedMethod::new(
            &REMOTE_BRANCH_CLASS,
            &branch::RemoteBranchIsAncestorOfMethod,
            objects::remote_branch_by_name,
        ),
        &LiftedMethod::new(
            &REMOTE_BRANCH_CLASS,
            &branch::RemoteBranchDeleteMethod,
            objects::remote_branch_by_name,
        ),
        &LiftedMethod::new(
            &REMOTE_BRANCH_CLASS,
            &branch::RemoteBranchLogMethod,
            objects::remote_branch_by_name,
        ),
        &branch::RemoteBranchInfoMethod,
    ],
    ..BuiltinClass::new("git.branch", "RemoteBranchName", Some(&ANY_CLASS))
};

// ------------------------------------------------------------------------------------------------ tags and remotes

pub static TAG_CLASS: BuiltinClass = BuiltinClass {
    summary: "A Git tag",
    fields: &[
        Field {
            name: "name",
            summary: "Name of the tag",
            ty: || Type::tagged_string(&TAG_NAME_CLASS),
        },
        Field {
            name: "commit",
            summary: "The commit the tag is pointing to",
            ty: commit_hash,
        },
    ],
    methods: &[&tag::TagDeleteMethod],
    static_methods: &[&TAG_NEW],
    ..BuiltinClass::new("git.tag", "Tag", Some(&OBJECT_CLASS))
};

pub static TAG_NAME_CLASS: BuiltinClass = BuiltinClass {
    summary: "Name of a Git tag",
    ..BuiltinClass::new("git.tag", "TagName", Some(&ANY_CLASS))
};

pub static REMOTE_CLASS: BuiltinClass = BuiltinClass {
    summary: "A Git remote",
    fields: &[
        Field {
            name: "name",
            summary: "Name of the remote",
            ty: || Type::tagged_string(&REMOTE_NAME_CLASS),
        },
        Field {
            name: "fetchUrls",
            summary: "URLs to fetch from",
            ty: || Type::seq(Type::STRING),
        },
        Field {
            name: "pushUrls",
            summary: "URLs to push to",
            ty: || Type::seq(Type::STRING),
        },
    ],
    static_methods: &[&REMOTE_NEW],
    ..BuiltinClass::new("git.remote", "Remote", Some(&OBJECT_CLASS))
};

/// (Scala names it `git.branch.RemoteName`, though it's defined with the remotes)
pub static REMOTE_NAME_CLASS: BuiltinClass = BuiltinClass {
    summary: "A git remote name",
    ..BuiltinClass::new("git.branch", "RemoteName", Some(&ANY_CLASS))
};

static COMMIT_NEW: FieldsConstructor =
    FieldsConstructor::new("git.Commit", &COMMIT_CLASS, "Construct a new Commit object");
static IDENTITY_NEW: FieldsConstructor =
    FieldsConstructor::new("git.Identity", &IDENTITY_CLASS, "Construct a new Identity object");
static FETCH_BRANCH_UPDATE_NEW: FieldsConstructor = FieldsConstructor::new(
    "git.FetchBranchUpdate",
    &FETCH_BRANCH_UPDATE_CLASS,
    "Construct a new FetchBranchUpdate object",
);
static BRANCH_NEW: FieldsConstructor =
    FieldsConstructor::new("git.branch.Branch", &BRANCH_CLASS, "Construct a new Branch object");
static REMOTE_BRANCH_NEW: FieldsConstructor = FieldsConstructor::new(
    "git.branch.RemoteBranch",
    &REMOTE_BRANCH_CLASS,
    "Construct a new RemoteBranch object",
);
static TAG_NEW: FieldsConstructor = FieldsConstructor::new("git.tag.Tag", &TAG_CLASS, "Construct a new Tag object");
static REMOTE_NEW: FieldsConstructor =
    FieldsConstructor::new("git.remote.Remote", &REMOTE_CLASS, "Construct a new Remote object");
