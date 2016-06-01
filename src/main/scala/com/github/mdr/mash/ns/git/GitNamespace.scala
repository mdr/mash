package com.github.mdr.mash.ns.git

import com.github.mdr.mash.ns.git.stash._
import com.github.mdr.mash.ns.git.branch._

object GitNamespace {

  val name = "git"

  val GitFunctions = Seq(
    AddFunction,
    CloneFunction,
    CommitFunction,
    FetchFunction,
    InitFunction,
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
    branch.ListFunction,
    branch.ListRemoteFunction,
    branch.SwitchFunction,
    stash.ApplyFunction,
    stash.CreateFunction)
}