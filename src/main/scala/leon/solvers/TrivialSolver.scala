package leon
package solvers

import purescala.Common._
import purescala.Definitions._
import purescala.Trees._
import purescala.TypeTrees._

class TrivialSolver(context: LeonContext) extends Solver(context) with NaiveIncrementalSolver {
  val name = "trivial"
  val description = "Solver for syntactically trivial formulas"

  def solve(expression: Expr) : Option[Boolean] = expression match {
    case BooleanLiteral(v) => Some(v)
    case Not(BooleanLiteral(v)) => Some(!v)
    case Or(exs) if exs.contains(BooleanLiteral(true)) => Some(true)
    case And(exs) if exs.contains(BooleanLiteral(false)) => Some(false)
    case Equals(l,r) if l == r => Some(true)
    case _ => None
  }
  
  override def SetModelListener(listener: (Map[Identifier,Expr] => Unit)) {       
  }
  
  /*override def SetClauseListener(listener: (Seq[Expr],Seq[Expr],Seq[Expr]) => Unit) {    
  }*/
  
  override def setInferenceEngine(infEngine: () => Boolean) = {       
  }
  
  override def solve(body: Expr,post: Expr) : (Option[Boolean], Map[Identifier, Expr]) = {
    solve(And(body,Not(post))) match {
      case Some(true) =>
        (Some(false), Map())
      case Some(false) =>
        (Some(true), Map())
      case None =>
        (None, Map())
    }
  }

}
