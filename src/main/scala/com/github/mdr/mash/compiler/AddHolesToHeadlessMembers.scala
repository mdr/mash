package com.github.mdr.mash.compiler

import com.github.mdr.mash.parser.AbstractSyntax._

object AddHolesToHeadlessMembers {

  def addHoles(program: Program): Program = program.transform {
    case HeadlessMemberExpr(member, isNullSafe, sourceInfoOpt) ⇒ MemberExpr(Hole(None), member, isNullSafe, sourceInfoOpt)
  }.asInstanceOf[Program]

}