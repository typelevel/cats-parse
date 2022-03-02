import com.typesafe.tools.mima.core.ProblemFilters
import com.typesafe.tools.mima.core.IncompatibleMethTypeProblem
import com.typesafe.tools.mima.core.IncompatibleResultTypeProblem
object MimaExclusionRules {
  val ParserImpl = Seq(
    "cats.parse.Parser#Impl#CharIn.copy",
    "cats.parse.Parser#Impl#CharIn.apply",
    "cats.parse.Parser#Impl#CharIn.this"
  ).map(ProblemFilters.exclude[IncompatibleMethTypeProblem](_)) ++ Seq(
    "cats.parse.Parser#Impl#CharIn.copy$default$2",
    "cats.parse.Parser#Impl#CharIn.bitSet"
  ).map(ProblemFilters.exclude[IncompatibleResultTypeProblem](_))
}
