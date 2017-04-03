package com.github.mdr.mash

import java.io.PrintStream
import java.nio.charset.StandardCharsets
import java.nio.file.Path
import java.util.UUID

import com.github.mdr.mash.commands.ErrorPrinter
import com.github.mdr.mash.compiler.{ CompilationSettings, CompilationUnit, Compiler }
import com.github.mdr.mash.evaluator.{ EvaluationContext, ScopeStack, _ }
import com.github.mdr.mash.functions.Namespace
import com.github.mdr.mash.os.linux.LinuxFileSystem
import com.github.mdr.mash.parser.AbstractSyntax.NamespaceDeclaration
import com.github.mdr.mash.parser.{ AbstractSyntax, ParseError }
import com.github.mdr.mash.runtime.{ MashObject, MashValue }
import com.github.mdr.mash.terminal.Terminal
import org.apache.commons.io.FileUtils
import org.fusesource.jansi.Ansi

case class LoadResult(namespace: Namespace, loadScope: MashObject)

class Loader(terminal: Terminal,
             output: PrintStream,
             sessionId: UUID,
             globals: MashObject,
             ns: MashObject) {

  private val fileSystem = LinuxFileSystem
  private val errorPrinter = new ErrorPrinter(output, terminal.info)
  private val debugLogger = new DebugLogger(sessionId.toString)

  def load() {
    for {
      path ← getMashFilePaths
      LoadResult(namespace, loadScope) ← load(path)
      (name, value) ← loadScope.immutableFields
    } populate(ns, namespace.segments.toList, name, value)
    for ((name, value) <- ns.immutableFields)
      globals.set(name, value)
  }

  private def getMashFilePaths: Seq[Path] = {
    val libPath = Mash.MashDir.resolve("lib")
    val mashFileGlob = libPath.toString + "/**.mash"
    fileSystem.glob(mashFileGlob).map(_.path)
  }

  private def populate(obj: MashObject, path: List[String], name: String, value: MashValue): Unit =
    path match {
      case Nil           ⇒
        obj.set(name, value)
      case first :: rest ⇒
        obj.get(first) match {
          case Some(subObject: MashObject) ⇒
            populate(subObject, rest, name, value)
          case _                           ⇒
            obj.set(first, MashObject.empty)
            populate(obj, path, name, value)
        }
    }

  private def load(path: Path): Option[LoadResult] = {
    val s = FileUtils.readFileToString(path.toFile, StandardCharsets.UTF_8)
    val compilationUnit = CompilationUnit(s, name = path.toString, mish = false)
    val programOpt = safeCompile(compilationUnit, bareWords = false)
    for {
      program ← programOpt
      namespaceDeclaration ← program.namespaceOpt
      unitScope ← runProgram(program, compilationUnit)
      namespace = getNamespace(namespaceDeclaration)
    } yield LoadResult(namespace, unitScope)
  }

  private def getNamespace(namespaceDeclaration: NamespaceDeclaration): Namespace =
    Namespace(namespaceDeclaration.segments)

  private def runProgram(program: AbstractSyntax.Program, unit: CompilationUnit): Option[MashObject] =
    try {
      val context = new ExecutionContext(Thread.currentThread)
      Singletons.environment = globals.get(StandardEnvironment.Env) match {
        case Some(obj: MashObject) ⇒ obj
        case _                     ⇒ MashObject.empty
      }
      Singletons.setExecutionContext(context)
      ExecutionContext.set(context)
      val unitScope = MashObject.empty
      val scopeStack = ScopeStack(globals).withFullScope(unitScope)
      val namespaceOpt = program.namespaceOpt.map(getNamespace)
      Evaluator.evaluate(program.body)(EvaluationContext(scopeStack, namespaceOpt))
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

  private def safeCompile(unit: CompilationUnit, bareWords: Boolean, printErrors: Boolean = true): Option[AbstractSyntax.Program] = {
    val settings = CompilationSettings(bareWords = bareWords)
    Compiler.compile(unit, globals.immutableFields, settings) match {
      case Left(ParseError(msg, location)) ⇒
        if (printErrors)
          errorPrinter.printError("Syntax error", msg, unit, Seq(StackTraceItem(SourceLocation(unit.provenance, location))))
        None
      case Right(program)                  ⇒
        Some(program)
    }
  }

}
