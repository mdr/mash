package com.github.mdr.mash.ns

import com.github.mdr.mash.ns.collections._
import com.github.mdr.mash.ns.core._
import com.github.mdr.mash.ns.core.help._
import com.github.mdr.mash.ns.os.{ GroupClass ⇒ _, _ }
import com.github.mdr.mash.ns.time._
import com.github.mdr.mash.ns.git._
import com.github.mdr.mash.evaluator.MashClass
import com.github.mdr.mash.ns.view.ViewClass
import com.github.mdr.mash.ns.git.branch.{ DeleteFunction ⇒ _, _ }
import com.github.mdr.mash.ns.view._
import org.apache.commons.lang3.SystemUtils
import com.github.mdr.mash.ns.os.WithinFunction

object MashRoot {

  lazy val AllFunctions = StandardFunctions ++ OtherFunctions

  /**
   * Functions to be imported into the default namespace
   */
  lazy val StandardFunctions = CoreFunctions ++ OsFunctions ++ CollectionsFunctions

  lazy val StandardClasses = Seq(
    ListClass,
    BooleanClass,
    BoundMethodClass,
    ClassClass,
    FunctionClass,
    NullClass,
    NumberClass,
    ObjectClass,
    AnyClass,
    StringClass,
    UnitClass)

  lazy private val OtherFunctions =
    GitNamespace.GitFunctions ++
      Seq(BrowserFunction, RawFunction) ++
      Seq(json.FromFileFunction, json.FromStringFunction, json.AsJsonFunction) ++
      Seq(http.GetFunction)

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
    ChangeDirectoryFunction,
    ChildrenFunction,
    CopyFunction,
    CreateDirectoryFunction,
    HomeFunction,
    GlobFunction,
    KillFunction,
    ListFilesFunction,
    MoveFunction,
    OldDirsFunction,
    CurrentDirectoryFunction,
    ReadLinesFunction,
    DeleteFunction,
    UpFunction,
    WithinFunction,
    WriteFunction,
    ProcessesFunction) ++ (
      if (SystemUtils.IS_OS_MAC_OSX)
        Seq()
      else
        Seq(
          GroupsFunction,
          UserFunction,
          UsersFunction))

  private val CollectionsFunctions = Seq(
    AllFunction,
    AnyFunction,
    ContainsFunction,
    CountMatchesFunction,
    EachFunction,
    FindFunction,
    FlatMapFunction,
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
    ReduceFunction,
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
    "cd" -> ChangeDirectoryFunction,
    "cp" -> CopyFunction,
    "drop" -> SkipFunction,
    "dropWhile" -> SkipWhileFunction,
    "count" -> LengthFunction,
    "keepIf" -> WhereFunction,
    "ls" -> ListFilesFunction,
    "filter" -> WhereFunction,
    "filterNot" -> WhereNotFunction,
    "discardIf" -> WhereNotFunction,
    "dropIf" -> WhereNotFunction,
    "cat" -> ReadLinesFunction,
    "rm" -> DeleteFunction,
    "man" -> HelpFunction,
    "mkdir" -> CreateDirectoryFunction,
    "pwd" -> CurrentDirectoryFunction)

  val AllClasses: Seq[MashClass] = Seq(
    GroupClass,
    ListClass,
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
    AnyClass,
    RegexClass,
    StringClass,
    UnitClass,
    // git
    CommitClass,
    CommitHashClass,
    IdentityClass,
    FetchBranchUpdateClass,
    BranchClass,
    LocalBranchNameClass,
    RemoteBranchClass,
    RemoteBranchNameClass,
    RemoteNameClass,
    //
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
    MillisecondsClass,
    SecondsClass,
    MinutesClass,
    HoursClass,
    DaysClass,
    WeeksClass,
    MonthsClass,
    DateTimeClass,
    LocalDateClass,
    ViewClass,
    http.ResponseClass)
}