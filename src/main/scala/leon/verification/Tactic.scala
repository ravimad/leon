package leon
package verification

import purescala.Definitions._

abstract class Tactic(reporter: Reporter) {
  val description : String
  val shortDescription : String

  def setProgram(program: Program) : Unit = {}
  def generatePostconditions(function: FunDef) : Seq[VerificationCondition]
  def generateExtendedVCs(function: FunDef) : Seq[ExtendedVC] = {
    throw NotImplementedException("generate Extended VCs not implemented")
  }
  def generatePreconditions(function: FunDef) : Seq[VerificationCondition]
  def generatePatternMatchingExhaustivenessChecks(function: FunDef) : Seq[VerificationCondition]
  def generateMiscCorrectnessConditions(function: FunDef) : Seq[VerificationCondition]
  def generateArrayAccessChecks(function: FunDef) : Seq[VerificationCondition]
}
