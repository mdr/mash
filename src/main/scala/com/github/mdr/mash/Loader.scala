package com.github.mdr.mash

import java.io.PrintStream
import java.nio.charset.StandardCharsets
import java.nio.file.{ Path, Paths }
import java.util.UUID

import com.github.mdr.mash.commands.ErrorPrinter
import com.github.mdr.mash.compiler.{ CompilationSettings, CompilationUnit, Compiler }
import com.github.mdr.mash.evaluator.{ EvaluationContext, ScopeStack, _ }
import com.github.mdr.mash.os.linux.LinuxFileSystem
import com.github.mdr.mash.parser.ParseError
import com.github.mdr.mash.runtime.{ MashObject, MashValue }
import com.github.mdr.mash.terminal.Terminal
import org.apache.commons.io.FileUtils

case class Namespace(segments: Seq[String])

case class LoadResult(namespace: Namespace, loadScope: MashObject)

class Loader(terminal: Terminal,
             output: PrintStream,
             sessionId: UUID,
             globalVariables: MashObject) {

  private val fileSystem = LinuxFileSystem
  private val errorPrinter = new ErrorPrinter(output, terminal.info)

  def load() {
    val libPath = Mash.MashDir.resolve("lib")
    val mashFileGlob = libPath.toString + "/**.mash"
    val mashFilePaths = fileSystem.glob(mashFileGlob).map(_.path)
    for (path ← mashFilePaths) {
      val LoadResult(namespace, loadScope) = load(path)
      for ((name, value) ← loadScope.immutableFields)
        populate(globalVariables, namespace.segments, name, value)
    }
  }

  private def populate(obj: MashObject, path: Seq[String], name: String, value: MashValue): Unit = path match {
    case Seq()               ⇒
      obj.set(name, value)
    case Seq(first, rest@_*) ⇒
      obj.get(first) match {
        case Some(subObject: MashObject) ⇒
          populate(subObject, rest, name, value)
        case _                           ⇒
          obj.set(first, MashObject.empty())
          populate(obj, path, name, value)
      }

  }

  private def load(path: Path): LoadResult = {
    val s = FileUtils.readFileToString(path.toFile, StandardCharsets.UTF_8)
    val compilationUnit = CompilationUnit(s, name = path.toString, mish = false)
    val settings = CompilationSettings(inferTypes = false, bareWords = false)
    val program = Compiler.compile(compilationUnit, globalVariables.immutableFields, settings) match {
      case Right(program)                  ⇒ program
      case Left(ParseError(msg, location)) ⇒
        errorPrinter.printError("Syntax error", msg, compilationUnit, Seq(StackTraceItem(SourceLocation(compilationUnit.provenance, location))))
        ???
    }
    val context = new ExecutionContext(Thread.currentThread)

    Singletons.environment = globalVariables.get(StandardEnvironment.Env) match {
      case Some(obj: MashObject) ⇒ obj
      case _                     ⇒ MashObject.empty()
    }
    Singletons.setExecutionContext(context)
    ExecutionContext.set(context)
    val unitScope = MashObject.empty
    val scopeStack = ScopeStack(globalVariables).withFullScope(unitScope)
    Evaluator.evaluate(program.body)(EvaluationContext(scopeStack))
    val namespace = Namespace(program.namespaceOpt.get.segments)
    LoadResult(namespace, unitScope)
  }

}
