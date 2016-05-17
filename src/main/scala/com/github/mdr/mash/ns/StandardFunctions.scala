package com.github.mdr.mash.ns

import com.github.mdr.mash.ns.collections._
import com.github.mdr.mash.ns.core._
import com.github.mdr.mash.ns.core.help._
import com.github.mdr.mash.ns.os.{ GroupClass ⇒ _, _ }
import com.github.mdr.mash.ns.time._
import com.github.mdr.mash.ns.core.HistoryFunction
import com.github.mdr.mash.ns.git._
import com.github.mdr.mash.evaluator.MashClass
import com.github.mdr.mash.ns.view.ViewClass
import com.github.mdr.mash.ns.json.FromFileFunction

object StandardFunctions {

  /**
   * Functions imported into the default namespace
   */
  lazy val StandardFunctions = CoreFunctions ++ OsFunctions ++ CollectionsFunctions

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
    CreateDirectoryFunction,
    HomeFunction,
    GlobFunction,
    KillFunction,
    LsFunction,
    MoveFunction,
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
    NonEmptyFunction,
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
    "mv" -> MoveFunction,
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
    "man" -> HelpFunction,
    "mkdir" -> CreateDirectoryFunction)

  val AllClasses: Seq[MashClass] = Seq(
    GroupClass,
    SeqClass,
    BooleanClass,
    BoundMethodClass,
    BytesClass,
    ClassClass,
    FunctionClass,
    ClassHelpClass,
    FieldHelpClass,
    FunctionHelpClass,
    ParameterHelpClass,
    HistoryClass,
    NullClass,
    NumberClass,
    ObjectClass,
    RegexClass,
    StringClass,
    UnitClass,
    BranchClass,
    CommitClass,
    CommitHashClass,
    IdentityClass,
    FileTypeClass,
    GidClass,
    com.github.mdr.mash.ns.os.GroupClass,
    GroupInfoClass,
    PathClass,
    PathSummaryClass,
    PermissionsClass,
    PermissionsSectionClass,
    PidClass,
    ProcessClass,
    ProcessResultClass,
    SignalClass,
    UidClass,
    UsernameClass,
    UserSummaryClass,
    DateTimeClass,
    LocalDateClass,
    ViewClass)
}