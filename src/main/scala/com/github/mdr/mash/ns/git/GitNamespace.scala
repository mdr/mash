package com.github.mdr.mash.ns.git

import com.github.mdr.mash.functions.HasName

object GitNamespace {

  val name = "git"

  val GitFunctions = Seq(
    AddFunction,
    BranchesFunction,
    CommitFunction,
    FetchFunction,
    LogFunction,
    PullFunction,
    PushFunction,
    StatusFunction)
}