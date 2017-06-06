package com.github.mdr.mash.compiler

import com.github.mdr.mash.parser.AbstractSyntax._

object AddHolesToHeadlessMembers {

  def addHoles(program: Program): Program = program.transform {
    case HeadlessMemberExpr(member, isSafe, sourceInfoOpt) ⇒ MemberExpr(Hole(1, None), member, isSafe, sourceInfoOpt)
  }.asInstanceOf[Program]

}