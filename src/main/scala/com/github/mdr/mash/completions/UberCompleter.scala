package com.github.mdr.mash.completions

import java.io.IOException
import java.nio.file.Files
import java.nio.file.LinkOption
import java.nio.file.Paths
import java.nio.file.attribute.PosixFileAttributeView
import scala.PartialFunction.condOpt
import com.github.mdr.mash.compiler.Compiler
import com.github.mdr.mash.evaluator.BoundMethod
import com.github.mdr.mash.evaluator.Environment
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.evaluator.TildeExpander
import com.github.mdr.mash.inference.AnnotatedExpr
import com.github.mdr.mash.inference.SimpleTypedArguments
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.inference.TypedArgument
import com.github.mdr.mash.inference.TypedArguments
import com.github.mdr.mash.lexer.MashLexer
import com.github.mdr.mash.lexer.Token
import com.github.mdr.mash.lexer.TokenType
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.os.EnvironmentInteractions
import com.github.mdr.mash.os.FileSystem
import com.github.mdr.mash.parser.AbstractSyntax.Argument
import com.github.mdr.mash.parser.AbstractSyntax.InvocationExpr
import com.github.mdr.mash.parser.MashParserException
import com.github.mdr.mash.utils.Region
import com.github.mdr.mash.utils.StringUtils
import com.github.mdr.mash.parser.Abstractifier
import com.github.mdr.mash.parser.StringEscapes

object UberCompleter {

  /**
   *  @return true if the given pos is either inside the given token or immediately after it
   */
  def isNearby(pos: Int, token: Token) = token.region.contains(pos) || pos == token.region.posAfter

}

class UberCompleter(fileSystem: FileSystem, envInteractions: EnvironmentInteractions) {

  import UberCompleter._

  def complete(s: String, pos: Int, env: Environment, mish: Boolean): Option[CompletionResult] = {
    val tokens = MashLexer.tokenise(s, forgiving = true, includeCommentsAndWhitespace = true, mish = mish)
    tokens.find(isNearby(pos, _)).flatMap { nearbyToken ⇒
      nearbyToken.tokenType match {
        case TokenType.STRING_LITERAL | TokenType.TILDE | TokenType.DIVIDE ⇒
          completeAsString(s, nearbyToken.region, env, mish)._2
        case TokenType.LONG_FLAG ⇒
          val exprOpt = Compiler.compile(s, env, forgiving = true, inferTypes = true, mish = mish)
          for {
            expr ← exprOpt
            sourceInfo ← expr.sourceInfoOpt
            InvocationInfo(invocationExpr, _) ← InvocationFinder.findInvocationWithFlagArg(expr, nearbyToken)
            functionType ← invocationExpr.function.typeOpt
            flags ← FlagCompleter.getFlags(functionType)
            completions = FlagCompleter.completeLongFlag(flags, nearbyToken)
            if completions.nonEmpty
          } yield CompletionResult(completions, nearbyToken.region)
        case TokenType.MINUS ⇒
          val replaced = StringUtils.replace(s, nearbyToken.region, "--dummyFlag")
          val exprOpt = Compiler.compile(replaced, env, forgiving = true, inferTypes = true, mish = mish)
          for {
            expr ← exprOpt
            sourceInfo ← expr.sourceInfoOpt
            tokens = sourceInfo.expr.tokens
            flagToken ← tokens.find(t ⇒ isNearby(pos, t) && t.isFlag)
            InvocationInfo(invocationExpr, _) ← InvocationFinder.findInvocationWithFlagArg(expr, flagToken)
            functionType ← invocationExpr.function.typeOpt
            flags ← FlagCompleter.getFlags(functionType)
            completions = FlagCompleter.completeAllFlags(flags)
            if completions.nonEmpty
          } yield CompletionResult(completions, nearbyToken.region)
        case TokenType.IDENTIFIER ⇒
          val (isPathCompletion, asStringResultOpt) = completeAsString(s, nearbyToken.region, env, mish = mish)
          val (isMemberExpr, memberResultOpt) = MemberCompleter.completeMember(s, nearbyToken, env, mish = mish)
          val bindingResultOpt =
            if (isMemberExpr)
              None // It would be misleading to try and complete other things in member position
            else
              completeBindingsAndFiles(env, nearbyToken.text, nearbyToken.region)
          if (isPathCompletion)
            merge(asStringResultOpt, bindingResultOpt)
          else
            asStringResultOpt orElse memberResultOpt orElse bindingResultOpt
        case TokenType.DOT | TokenType.DOT_NULL_SAFE ⇒
          val posBeforeDot = nearbyToken.offset - 1
          val isMemberDot = (posBeforeDot >= 0 && !s(posBeforeDot).isWhitespace)

          val replaced = StringUtils.replace(s, nearbyToken.region, ".dummy")
          val expr = Compiler.compile(replaced, env, forgiving = true, inferTypes = true, mish = mish)
          val memberCompletionResultOpt = MemberCompleter.complete(s, nearbyToken, env, pos, mish = mish)
          if (isMemberDot)
            memberCompletionResultOpt orElse completeAsString(s, nearbyToken.region, env, mish = mish)._2
          else
            completeAsString(s, nearbyToken.region, env, mish = mish)._2 orElse memberCompletionResultOpt
        case t ⇒
          val asStringRegion = if (t.isWhitespace) Region(pos, 0) else nearbyToken.region
          val (isPathCompletion, asStringResultOpt) = completeAsString(s, asStringRegion, env, mish = mish)
          val bindingResultOpt = completeBindingsAndFiles(env, "", Region(pos, 0))
          if (isPathCompletion)
            merge(asStringResultOpt, bindingResultOpt)
          else
            asStringResultOpt orElse bindingResultOpt
      }
    }.map(_.sorted)
  }

  private def merge(res1Opt: Option[CompletionResult], res2Opt: Option[CompletionResult]): Option[CompletionResult] = {
    (res1Opt, res2Opt) match {
      case (Some(CompletionResult(completions1, location1)), Some(CompletionResult(completions2, location2))) if location1 == location2 ⇒
        Some(CompletionResult((completions1 ++ completions2).distinct.sortBy(_.displayText), location1))
      case _ ⇒ res1Opt orElse res2Opt
    }
  }

  private def completeBindingsAndFiles(env: Environment, prefix: String, region: Region): Option[CompletionResult] = {
    val bindingCompletions =
      for {
        (name, value) ← env.valuesMap.toSeq
        if name startsWith prefix
        (completionType, description) = getBindingTypeAndDescription(value)
      } yield Completion(name, typeOpt = Some(completionType), descriptionOpt = Some(description))
    val fileCompletions = completeFiles(prefix)
    val completions = bindingCompletions ++ fileCompletions
    if (completions.isEmpty)
      None
    else
      Some(CompletionResult(completions, region))
  }

  private def getBindingTypeAndDescription(value: Any) = value match {
    case mf: MashFunction ⇒ (CompletionType.Function, mf.summary)
    case bf: BoundMethod  ⇒ (CompletionType.Method, bf.method.summary)
    case x                ⇒ (CompletionType.Binding, x + "")
  }

  /**
   * Reinterpret nearby characters as a String and attempt completion
   *
   * First return value is true iff the completion was a path completion
   */
  private def completeAsString(s: String, initialRegion: Region, env: Environment, mish: Boolean): (Boolean, Option[CompletionResult]) = {
    val region = ContiguousRegionFinder.getContiguousRegion(s, initialRegion, mish = mish)
    val replacement = "\"" + region.of(s).filterNot('"' == _) + "\""
    val replaced = StringUtils.replace(s, region, replacement)
    val exprOpt = Compiler.compile(replaced, env, forgiving = true, inferTypes = true, mish = mish)
    def isNearbyStringToken(token: Token) = token.isString && isNearby(initialRegion.offset, token)
    val argCompletionOpt =
      for {
        expr ← exprOpt
        sourceInfo ← expr.sourceInfoOpt
        tokens = sourceInfo.expr.tokens
        literalToken ← tokens.find(isNearbyStringToken)
        InvocationInfo(invocationExpr, argPos) ← InvocationFinder.findInvocationWithLiteralArg(expr, literalToken)
        completionSpecs ← getCompletionSpecs(invocationExpr, argPos)
        completions = completeFromSpecs(completionSpecs, literalToken)
        if completions.nonEmpty
      } yield CompletionResult(completions, region)
    val equalityCompletionOpt =
      for {
        expr ← exprOpt
        sourceInfo ← expr.sourceInfoOpt
        tokens = sourceInfo.expr.tokens
        literalToken ← tokens.find(isNearbyStringToken)
        equalityType ← EqualityFinder.findEqualityExprWithLiteralArg(expr, literalToken)
        completions ← getEqualityCompletions(equalityType, literalToken)
        if completions.nonEmpty
      } yield CompletionResult(completions, region)
    val filesCompletionOpt = {
      val withoutQuotes = replacement.filterNot(_ == '"')
      val completions = completeFiles(withoutQuotes)
      if (completions.nonEmpty)
        Some(CompletionResult(completions, region))
      else
        None
    }
    val isPathCompletion = argCompletionOpt.isEmpty && equalityCompletionOpt.isEmpty && filesCompletionOpt.isDefined
    val completionResultOpt = argCompletionOpt orElse equalityCompletionOpt orElse filesCompletionOpt
    (isPathCompletion, completionResultOpt)
  }

  private def getEqualityCompletions(t: Type, literalToken: Token): Option[Seq[Completion]] = condOpt(t) {
    case Type.Tagged(baseClass, tagClass) ⇒
      val withoutQuotes = literalToken.text.filterNot(_ == '"')
      tagClass.enumerationValues.toSeq.flatten.filter(_.startsWith(withoutQuotes)).map(Completion(_, isQuoted = baseClass == StringClass))
  }

  private def getCompletionSpecs(invocationExpr: InvocationExpr, argPos: Int): Option[Seq[CompletionSpec]] =
    invocationExpr.function.typeOpt.flatMap { functionType ⇒
      condOpt(functionType) {
        case Type.DefinedFunction(f) ⇒
          f.getCompletionSpecs(argPos, typedArguments(invocationExpr))
        case Type.BoundMethod(targetType, m) ⇒
          m.getCompletionSpecs(argPos, Some(targetType), typedArguments(invocationExpr))
      }
    }

  private def typedArguments(invocationExpr: InvocationExpr): TypedArguments = {
    def annotateArg(arg: Argument): TypedArgument =
      arg match {
        case Argument.PositionArg(e, _)           ⇒ TypedArgument.PositionArg(AnnotatedExpr(Some(e), e.typeOpt))
        case Argument.ShortFlag(flags, _)         ⇒ TypedArgument.ShortFlag(flags)
        case Argument.LongFlag(flag, valueOpt, _) ⇒ TypedArgument.LongFlag(flag, valueOpt.map(e ⇒ AnnotatedExpr(Some(e), e.typeOpt)))
      }
    SimpleTypedArguments(invocationExpr.arguments.map(annotateArg))
  }

  private def getFileCompletionType(pathString: String): Option[CompletionType] = {
    val p = Paths.get(pathString)
    val attrs = try
      Files.getFileAttributeView(p, classOf[PosixFileAttributeView], LinkOption.NOFOLLOW_LINKS).readAttributes()
    catch {
      case e: IOException ⇒
        return None
    }
    if (attrs.isRegularFile())
      Some(CompletionType.File)
    else if (attrs.isDirectory())
      Some(CompletionType.Directory)
    else
      None
  }

  private def filePathCompleter = new FilePathCompleter(fileSystem)

  private def completeFiles(s: String, directoriesOnly: Boolean = false): Seq[Completion] = {
    val tildeExpander = new TildeExpander(envInteractions)
    val tildeExpandedOpt = tildeExpander.expandOpt(s)
    val prefix = StringEscapes.unescape(tildeExpandedOpt.getOrElse(s))
    for {
      path ← filePathCompleter.getCompletions(prefix, directoriesOnly = directoriesOnly)
      tildeExpanded = if (tildeExpandedOpt.isDefined) tildeExpander.retilde(path) else path
      escaped = StringEscapes.escapeChars(tildeExpanded)
      typeOpt = getFileCompletionType(path)
    } yield Completion(tildeExpanded, Some(escaped), isQuoted = true, typeOpt = typeOpt, descriptionOpt = Some(path))
  }

  private def completeFromSpecs(completionSpecs: Seq[CompletionSpec], literalToken: Token): Seq[Completion] =
    completionSpecs.flatMap(spec ⇒ completeFromSpec(spec, literalToken))

  private def completeFromSpec(spec: CompletionSpec, literalToken: Token): Seq[Completion] = {
    import CompletionSpec._
    val withoutQuotes = literalToken.text.filterNot(_ == '"')
    spec match {
      case Directory | File ⇒
        completeFiles(withoutQuotes, directoriesOnly = spec == Directory)
      case Members(targetType) ⇒
        MemberCompleter.complete(targetType, withoutQuotes)
    }
  }

}