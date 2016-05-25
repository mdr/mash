package com.github.mdr.mash.ns.git

import com.github.mdr.mash.ns.git.stash._
import com.github.mdr.mash.ns.git.branch.SwitchFunction
import com.github.mdr.mash.ns.git.branch.CurrentBranchFunction
import com.github.mdr.mash.ns.git.branch.CreateBranchFunction
import com.github.mdr.mash.ns.git.branch.DeleteBranchFunction
import com.github.mdr.mash.ns.git.branch.BranchesFunction

object GitNamespace {

  val name = "git"

  val GitFunctions = Seq(
    AddFunction,
    BranchesFunction,
    CloneFunction,
    CommitFunction,
    CreateBranchFunction,
    CurrentBranchFunction,
    DeleteBranchFunction,
    FetchFunction,
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