package com.github.mdr.mash.ns.git

import com.github.mdr.mash.ns.git.stash._
import com.github.mdr.mash.ns.git.branch._

object GitNamespace {

  val name = "git"

  val GitFunctions = Seq(
    AddFunction,
    CloneFunction,
    CommitFunction,
    CreateFunction,
    CurrentFunction,
    DeleteFunction,
    FetchFunction,
    ListFunction,
    LogFunction,
    PullFunction,
    PushFunction,
    StageFunction,
    StatusFunction,
    SwitchFunction,
    StashApplyFunction,
    StashCreateFunction,
    UnstageFunction)
}