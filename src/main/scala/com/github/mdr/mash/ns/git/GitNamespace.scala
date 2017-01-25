package com.github.mdr.mash.ns.git

object GitNamespace {

  val name = Some("git")

  val GitFunctions = Seq(
    AddFunction,
    CloneFunction,
    CommitFunction,
    FetchFunction,
    InitFunction,
    IsRepoFunction,
    LogFunction,
    MergeFunction,
    PullFunction,
    PushFunction,
    RestoreFunction,
    StageFunction,
    StatusFunction,
    UnstageFunction,
    branch.CreateFunction,
    branch.CurrentFunction,
    branch.DeleteFunction,
    branch.GetFunction,
    branch.ListFunction,
    branch.ListRemoteFunction,
    branch.SetCommitFunction,
    branch.SwitchFunction,
    remote.ListFunction,
    stash.ApplyFunction,
    stash.CreateFunction,
    tag.CreateFunction,
    tag.DeleteFunction,
    tag.ListFunction)
}