package com.github.mdr.mash.ns.git

import com.github.mdr.mash.functions.HasName
import com.github.mdr.mash.ns.git.stash.StashCreateFunction

object GitNamespace {

  val name = "git"

  val GitFunctions = Seq(
    AddFunction,
    BranchesFunction,
    CloneFunction,
    CommitFunction,
    FetchFunction,
    LogFunction,
    PullFunction,
    PushFunction,
    StatusFunction,
    StashCreateFunction)
}