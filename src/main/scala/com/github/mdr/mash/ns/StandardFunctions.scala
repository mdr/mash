package com.github.mdr.mash.ns

import com.github.mdr.mash.ns.collections._
import com.github.mdr.mash.ns.core._
import com.github.mdr.mash.ns.core.help._
import com.github.mdr.mash.ns.os._
import com.github.mdr.mash.ns.time._
import com.github.mdr.mash.ns.core.HistoryFunction
import com.github.mdr.mash.ns.git.LogFunction

object StandardFunctions {

  lazy val Functions = CoreFunctions ++ OsFunctions ++ CollectionsFunctions

  private val CoreFunctions = Seq(
    ExitFunction,
    IdentityFunction,
    IsNullFunction,
    HelpFunction,
    HistoryFunction,
    NotFunction,
    NowFunction,
    ParseNumberFunction,
    PrintFunction,
    RunFunction)

  private val OsFunctions = Seq(
    CdFunction,
    ChildrenFunction,
    CopyFunction,
    HomeFunction,
    GlobFunction,
    KillFunction,
    LsFunction,
    MkdirFunction,
    MvFunction,
    OldDirsFunction,
    PwdFunction,
    ReadLinesFunction,
    DeleteFunction,
    UpFunction,
    WriteFunction,
    GroupsFunction,
    UserFunction,
    UsersFunction,
    ProcessesFunction)

  private val CollectionsFunctions = Seq(
    AllFunction,
    AnyFunction,
    ContainsFunction,
    CountMatchesFunction,
    EachFunction,
    FindFunction,
    FirstFunction,
    GroupByFunction,
    IsEmptyFunction,
    JoinFunction,
    LastFunction,
    LengthFunction,
    MapFunction,
    MaxByFunction,
    MaxFunction,
    MinByFunction,
    MinFunction,
    ReverseFunction,
    SelectFunction,
    SkipFunction,
    SkipUntilFunction,
    SkipWhileFunction,
    SortByFunction,
    SortFunction,
    SumByFunction,
    SumFunction,
    TakeWhileFunction,
    UniqueFunction,
    WhereFunction,
    WhereNotFunction)

  val Aliases = Map(
    "ps" -> ProcessesFunction,
    "cp" -> CopyFunction,
    "drop" -> SkipFunction,
    "dropWhile" -> SkipWhileFunction,
    "count" -> LengthFunction,
    "keepIf" -> WhereFunction,
    "filter" -> WhereFunction,
    "filterNot" -> WhereNotFunction,
    "discardIf" -> WhereNotFunction,
    "dropIf" -> WhereNotFunction,
    "cat" -> ReadLinesFunction,
    "rm" -> DeleteFunction,
    "man" -> HelpFunction)

}