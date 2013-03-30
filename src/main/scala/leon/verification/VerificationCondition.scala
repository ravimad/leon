package leon.verification

import leon.purescala.Trees._
import leon.purescala.Definitions._
import leon.purescala.Common._

import leon.solvers.Solver

/** This is just to hold some history information. */
class VerificationCondition(val condition: Expr, val funDef: FunDef, val kind: VCKind.Value, val tactic: Tactic, val info: String = "") extends ScalacPositional {
  // None = still unknown
  // Some(true) = valid
  // Some(false) = valid
  var value : Option[Boolean] = None
  var solvedWith : Option[Solver] = None
  var time : Option[Double] = None

  def status : String = value match {
    case None => "unknown"
    case Some(true) => "valid"
    case Some(false) => "invalid"
  }

  def tacticStr = tactic.shortDescription match {
    case "default" => ""
    case s => s
  }

  def solverStr = solvedWith match {
    case Some(s) => s.name
    case None => ""
  }

}

/** extended vc */
class ExtendedVC(val body: Expr, val post: Expr, val funDef1: FunDef, val kind1: VCKind.Value, val tactic1: Tactic, val info1: String = "") 
	extends VerificationCondition(Implies(body,post),funDef1,kind1,tactic1,info1) {
 }

object VCKind extends Enumeration {
  val Precondition = Value("precond.")
  val Postcondition = Value("postcond.")
  val ExhaustiveMatch = Value("match.")
  val MapAccess = Value("map acc.")
  val ArrayAccess = Value("arr. acc.")
  val InvariantInit = Value("inv init.")
  val InvariantInd = Value("inv ind.")
  val InvariantPost = Value("inv post.")
  val InvariantPre = Value("inv pre.")
}
	