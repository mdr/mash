package com.github.mdr.mash.ns.git

import org.eclipse.jgit.api.Git
import org.eclipse.jgit.lib.Repository
import org.eclipse.jgit.storage.file.FileRepositoryBuilder
import com.github.mdr.mash.os.linux.LinuxFileSystem
import com.github.mdr.mash.evaluator.EvaluatorException
import java.nio.file.Path

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

  def isRepository(path: Path): Boolean = {
    val builder = new FileRepositoryBuilder
    builder.readEnvironment()
    builder.findGitDir(path.toFile)
    builder.setMustExist(true)
    builder.getGitDir != null
  }

  private def getRepository() = {
    val builder = new FileRepositoryBuilder
    builder.readEnvironment()
    builder.findGitDir(filesystem.pwd.toFile)
    builder.setMustExist(true)
    if (builder.getGitDir == null)
      throw new EvaluatorException("Not a git repository (or any of the parent directories)")
    else
      builder.build()
  }

}