package com.github.mdr.mash.evaluator

object ExecutionContext {

  private val contextThreadLocal = new ThreadLocal[ExecutionContext]

  def isInterrupted = Option(contextThreadLocal.get).exists(_.interrupted)

  def set(ctx: ExecutionContext) = contextThreadLocal.set(ctx)

  def checkInterrupted() = {
    if (isInterrupted)
      throw new EvaluationInterruptedException
  }

}

class ExecutionContext(thread: Thread) {

  private var _interrupted: Boolean = false

  def interrupt() = synchronized {
    _interrupted = true
    //    thread.interrupt()
  }

  def interrupted = synchronized {
    _interrupted
  }

}

class EvaluationInterruptedException extends RuntimeException