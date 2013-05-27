package leon
package solvers

import purescala.Common._
import purescala.Definitions._
import purescala.TreeOps._
import purescala.Trees._
import leon.NotImplementedException

abstract class Solver(val context : LeonContext) extends IncrementalSolverBuilder with InterruptibleSolver with LeonComponent {
  // This can be used by solvers to "see" the programs from which the
  // formulas come. (e.g. to set up some datastructures for the defined
  // ADTs, etc.) 
  // Ideally, we would pass it at construction time and not change it later.
  def setProgram(program: Program) : Unit = {}
  
  def SetModelListener(listener: (Map[Identifier,Expr] => Unit)) {   
    throw NotImplementedException("SetModelListener not implemented")
  }
  
  /*def SetClauseListener(listener: ((Seq[Expr],Seq[Expr],Seq[Expr]) => Unit)) {
    throw NotImplementedException("SetClauseListener not implemented")
  }*/
  
  def setInferenceEngine(inferEngine: () => Boolean) {
    throw NotImplementedException("setInferenceEngine not implemented: "+this.getClass())
  }
  
  def solve(body: Expr,post: Expr) : (Option[Boolean], Map[Identifier, Expr]) = {
    throw NotImplementedException("solve with two arguments is not implemented")
  }

  // Returns Some(true) if valid, Some(false) if invalid,
  // None if unknown.
  // should halt as soon as possible with any result (Unknown is ok) as soon as forceStop is true
  def solve(expression: Expr) : Option[Boolean]

  def solveSAT(expression: Expr): (Option[Boolean], Map[Identifier, Expr]) = {
    solve(Not(expression)) match {
      case Some(true) =>
        (Some(false), Map())
      case Some(false) =>
        (Some(true), Map())
      case None =>
        (None, Map())
    }
  }

  def solveSATWithCores(expression: Expr, assumptions: Set[Expr]): (Option[Boolean], Map[Identifier, Expr], Set[Expr]) = {
    solveSAT(And(expression +: assumptions.toSeq)) match {
      case (Some(false), _) =>
        (Some(false), Map(), assumptions)
      case (r, m) =>
        (r, m, Set())
    }
  }   

  def superseeds : Seq[String] = Nil

  private var _forceStop = false

  def halt() : Unit = {
    _forceStop = true
  }

  def init() : Unit = {
    _forceStop = false
  }

  protected def forceStop = _forceStop
}

