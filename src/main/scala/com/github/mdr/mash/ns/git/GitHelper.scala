package com.github.mdr.mash.ns.git

import org.eclipse.jgit.api.Git
import org.eclipse.jgit.lib.Repository
import org.eclipse.jgit.storage.file.FileRepositoryBuilder
import com.github.mdr.mash.os.linux.LinuxFileSystem

object GitHelper {

  private val filesystem = LinuxFileSystem

  def withGit[T](p: Git ⇒ T): T = withRepository(repo ⇒ p(new Git(repo)))

  def withRepository[T](p: Repository ⇒ T): T = {
    val repo = getRepository
    try
      p(repo)
    finally
      repo.close()
  }

  private def getRepository() =
    new FileRepositoryBuilder()
      .readEnvironment()
      .findGitDir(filesystem.pwd.toFile)
      .setMustExist(true)
      .build()

}