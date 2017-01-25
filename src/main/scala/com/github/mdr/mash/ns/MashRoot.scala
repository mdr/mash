package com.github.mdr.mash.ns

import com.github.mdr.mash.evaluator.MashClass
import com.github.mdr.mash.ns.collections._
import com.github.mdr.mash.ns.core._
import com.github.mdr.mash.ns.core.help._
import com.github.mdr.mash.ns.core.thread.{ InBackgroundFunction, ParallelMapFunction }
import com.github.mdr.mash.ns.git._
import com.github.mdr.mash.ns.git.branch.{ DeleteFunction ⇒ _, _ }
import com.github.mdr.mash.ns.git.tag
import com.github.mdr.mash.ns.git.remote
import com.github.mdr.mash.ns.maths.StatsClass
import com.github.mdr.mash.ns.os.{ WithinFunction, GroupClass ⇒ _, _ }
import com.github.mdr.mash.ns.time._
import com.github.mdr.mash.ns.view.{ ViewClass, _ }
import org.apache.commons.lang3.SystemUtils

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
      MathsFunctions ++
      Seq(BrowserFunction, RawFunction, TreeFunction) ++
      Seq(json.FromFileFunction, json.FromStringFunction, json.PrettyPrintFunction) ++
      Seq(
        dns.LookupFunction,
        http.DeleteFunction,
        http.GetFunction,
        http.PostFunction,
        http.PutFunction,
        net.UrlEncodeFunction,
        net.UrlFunction) ++
      Seq(random.UuidFunction) ++
      Seq(
        mash.EvalFunction,
        mash.SourceFunction,
        mash.VersionFunction)

  private val CoreFunctions = Seq(
    ExitFunction,
    IdentityFunction,
    InBackgroundFunction,
    IsNullFunction,
    HelpFunction,
    HistoryFunction,
    NotFunction,
    NowFunction,
    ParallelMapFunction,
    ParseIso8601Function,
    ParseNumberFunction,
    PrintFunction,
    RunFunction,
    SleepFunction,
    TapFunction,
    TimeTakenFunction,
    TodayFunction,
    TryFunction,
    WhileFunction,
    type_.HintFunction)

  private val MathsFunctions = Seq(
    maths.LogFunction,
    maths.StatsFunction)

  private val OsFunctions = Seq(
    ChangeDirectoryFunction,
    ChildrenFunction,
    CopyFunction,
    CreateDirectoryFunction,
    CreateTempDirectoryFunction,
    CreateTempFileFunction,
    CurrentDirectoryFunction,
    DeleteFunction,
    GlobFunction,
    HomeFunction,
    KillFunction,
    ListFilesFunction,
    MoveFunction,
    OldDirsFunction,
    ReadLinesFunction,
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
    DeselectFunction,
    EachFunction,
    FindFunction,
    FlatMapFunction,
    FirstFunction,
    FlattenFunction,
    GrepFunction,
    GroupByFunction,
    IndexOfFunction,
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
    SlidingFunction,
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
    StatsClass,
    TimedResultClass,
    // git
    CommitClass,
    CommitHashClass,
    IdentityClass,
    FetchBranchUpdateClass,
    BranchClass,
    LocalBranchNameClass,
    RemoteBranchClass,
    RemoteBranchNameClass,
    tag.TagClass,
    tag.TagNameClass,
    remote.RemoteClass,
    remote.RemoteNameClass,
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
    DateClass,
    ViewClass,
    http.CookieClass,
    http.HeaderClass,
    http.ResponseClass,
    net.HostClass,
    net.UrlClass)
}