package com.github.mdr.mash.ns.git

object GitCommon {

  def trimRemoteBranchPrefix(remoteBranch: String): String =
    remoteBranch.replaceAll("^refs/remotes/", "")

}
