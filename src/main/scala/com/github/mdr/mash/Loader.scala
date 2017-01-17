package com.github.mdr.mash

import java.io.PrintStream
import java.nio.charset.StandardCharsets
import java.nio.file.Path
import java.util.UUID

import com.github.mdr.mash.commands.ErrorPrinter
import com.github.mdr.mash.compiler.{ CompilationSettings, CompilationUnit, Compiler }
import com.github.mdr.mash.evaluator.{ EvaluationContext, ScopeStack, _ }
import com.github.mdr.mash.os.linux.LinuxFileSystem
import com.github.mdr.mash.parser.{ AbstractSyntax, ParseError }
import com.github.mdr.mash.runtime.{ MashObject, MashValue }
import com.github.mdr.mash.terminal.Terminal
import org.apache.commons.io.FileUtils
import org.fusesource.jansi.Ansi

case class Namespace(segments: Seq[String])

case class LoadResult(namespace: Namespace, loadScope: MashObject)

class Loader(terminal: Terminal,
             output: PrintStream,
             sessionId: UUID,
             globalVariables: MashObject) {

  private val fileSystem = LinuxFileSystem
  private val errorPrinter = new ErrorPrinter(output, terminal.info)
  private val debugLogger = new DebugLogger(sessionId.toString)

  def load() {
    val libPath = Mash.MashDir.resolve("lib")
    val mashFileGlob = libPath.toString + "/**.mash"
    val mashFilePaths = fileSystem.glob(mashFileGlob).map(_.path)
    for {
      path ← mashFilePaths
      LoadResult(namespace, loadScope) ← load(path)
      (name, value) ← loadScope.immutableFields
    } populate(globalVariables, namespace.segments, name, value)
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

  private def load(path: Path): Option[LoadResult] = {
    val s = FileUtils.readFileToString(path.toFile, StandardCharsets.UTF_8)
    val compilationUnit = CompilationUnit(s, name = path.toString, mish = false)
    val programOpt = safeCompile(compilationUnit, bareWords = false)
    programOpt.flatMap { program ⇒
      runProgram(program, compilationUnit).map { unitScope ⇒
        val namespace = Namespace(program.namespaceOpt.get.segments)
        LoadResult(namespace, unitScope)
      }
    }
  }

  private def runProgram(program: AbstractSyntax.Program, unit: CompilationUnit): Option[MashObject] =
    try {
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
      Some(unitScope)
    } catch {
      case e@EvaluatorException(msg, stack, cause) ⇒
        if (true)
          errorPrinter.printError("Error", msg, unit, stack.reverse)
        debugLogger.logException(e)
        None
      case _: EvaluationInterruptedException       ⇒
        output.println(Ansi.ansi().fg(Ansi.Color.YELLOW).bold.a("Interrupted:").boldOff.a(" command cancelled by user").reset())
        None
    }


  private def safeCompile(unit: CompilationUnit, bareWords: Boolean): Option[AbstractSyntax.Program] = {
    val settings = CompilationSettings(inferTypes = false, bareWords = bareWords)
    Compiler.compile(unit, globalVariables.immutableFields, settings) match {
      case Left(ParseError(msg, location)) ⇒
        if (true)
          errorPrinter.printError("Syntax error", msg, unit, Seq(StackTraceItem(SourceLocation(unit.provenance, location))))
        None
      case Right(program)                  ⇒
        Some(program)
    }
  }

}
