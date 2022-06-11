import com.typesafe.tools.mima.core.{
  ProblemFilters,
  IncompatibleMethTypeProblem,
  IncompatibleResultTypeProblem,
  DirectMissingMethodProblem,
  MissingClassProblem
}

object MimaExclusionRules {

  private val parserImplIncompatibleMethTypeProblem =
    Seq(
      "cats.parse.Parser#Impl.mergeCharIn",
      "cats.parse.Parser#Impl.mergeStrIn",
      "cats.parse.Parser#Impl#CharIn.copy",
      "cats.parse.Parser#Impl#CharIn.apply",
      "cats.parse.Parser#Impl#CharIn.this"
    ).map(ProblemFilters.exclude[IncompatibleMethTypeProblem](_))

  private val parserImplIncompatibleResultTypeProblem =
    Seq(
      "cats.parse.Parser#Impl#StringIn.parseMut",
      "cats.parse.Parser#Impl.stringIn",
      "cats.parse.Parser#Impl#CharIn.copy$default$2",
      "cats.parse.Parser#Impl#CharIn.bitSet",
      "cats.parse.Parser#Impl#CharIn._2"
    ).map(ProblemFilters.exclude[IncompatibleResultTypeProblem](_))

  private val parserImplDirectMissingMethodProblem =
    Seq(
      "cats.parse.Parser#Impl.mergeCharIn",
      "cats.parse.Parser#Impl.mergeStrIn"
    ).map(ProblemFilters.exclude[DirectMissingMethodProblem](_))

  private val parserImplMissingClassProblem =
    Seq(
      "cats.parse.Parser$Impl$Rep0",
      "cats.parse.Parser$Impl$Rep0$"
    ).map(ProblemFilters.exclude[MissingClassProblem](_))

  val parserImpl =
    parserImplIncompatibleMethTypeProblem ++
      parserImplIncompatibleResultTypeProblem ++
      parserImplDirectMissingMethodProblem ++
      parserImplMissingClassProblem

  // TODO: Remove these rules in future release.
  private val bitSetUtilIncompatibleMethType =
    Seq(
      "cats.parse.BitSetUtil.isSingleton",
      "cats.parse.BitSetUtil.isSet"
    ).map(ProblemFilters.exclude[IncompatibleMethTypeProblem](_))

  private val bitSetUtilIncompatibleResultType =
    Seq(
      "cats.parse.BitSetUtil.bitSetFor"
    ).map(ProblemFilters.exclude[IncompatibleResultTypeProblem](_))

  val bitSetUtil =
    bitSetUtilIncompatibleMethType ++
      bitSetUtilIncompatibleResultType
}
