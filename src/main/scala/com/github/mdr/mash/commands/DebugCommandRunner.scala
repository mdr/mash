package com.github.mdr.mash.commands

import java.io.PrintStream

import com.github.mdr.mash.compiler.CompilationUnit
import com.github.mdr.mash.compiler.Compiler
import com.github.mdr.mash.lexer.MashLexer
import com.github.mdr.mash.parser.MashParserException
import com.github.mdr.mash.parser.PrettyPrinter
import com.github.mdr.mash.parser.TreePrettyPrinter
import com.github.mdr.mash.runtime.MashObject

class DebugCommandRunner(output: PrintStream, globals: MashObject) {

  def runDebugCommand(keyword: String, args: String, bareWords: Boolean) {
    (keyword, args) match {
      case ("p" | "pretty", actualCmd) ⇒
        for (expr ← Compiler.compile(CompilationUnit(actualCmd), globals.immutableFields, forgiving = true, inferTypes = true, bareWords = bareWords))
          TreePrettyPrinter.printTree(expr)
      case ("e" | "expression", actualCmd) ⇒
        for (expr ← Compiler.compile(CompilationUnit(actualCmd), globals.immutableFields, forgiving = true, bareWords = bareWords))
          output.println(PrettyPrinter.pretty(expr))
      case ("t" | "type", actualCmd) ⇒
        for (expr ← Compiler.compile(CompilationUnit(actualCmd), globals.immutableFields, forgiving = true, inferTypes = true, bareWords = bareWords))
          output.println(expr.typeOpt.getOrElse("Could not infer type"))
      case ("tokens", actualCmd) ⇒
        MashLexer.tokenise(actualCmd, forgiving = true, includeCommentsAndWhitespace = true).foreach(output.println)
    }
  }

}