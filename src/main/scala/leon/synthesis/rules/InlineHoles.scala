/* Copyright 2009-2014 EPFL, Lausanne */

package leon
package synthesis
package rules

import scala.annotation.tailrec

import leon.utils._

import solvers._

import purescala.Common._
import purescala.Trees._
import purescala.TreeOps._
import purescala.TypeTrees._
import purescala.Extractors._

case object InlineHoles extends Rule("Inline-Holes") {
  override val priority = RulePriorityHoles

  def instantiateOn(sctx: SynthesisContext, p: Problem): Traversable[RuleInstantiation] = {

    @tailrec
    def inlineUntilHoles(e: Expr): Expr = {
      if (containsHoles(e)) {
        // Holes are exposed, no need to inline (yet)
        e
      } else {
        // Inline all functions "once" that contain holes
        val newE = postMap {
          case fi @ FunctionInvocation(tfd, args) if usesHoles(fi) && tfd.hasBody =>
            val inlined = replaceFromIDs((tfd.params.map(_.id) zip args).toMap, tfd.body.get)
            Some(inlined)

          case _ => None
        }(e)

        inlineUntilHoles(newE)
      }
    }

    def inlineHoles(phi: Expr): (List[Identifier], Expr) = {
      var newXs = List[Identifier]()

      val res = preMap {
        case h @ Hole(o) =>
          val tpe = h.getType
          val x = FreshIdentifier("h", true).setType(tpe)
          newXs ::= x

          Some(x.toVariable)

        case _ => None
      }(phi)

      (newXs.reverse, res)
    }

    if (usesHoles(p.phi)) {
      val pathsToCalls = CollectorWithPaths({
          case fi: FunctionInvocation if usesHoles(fi) => fi
      }).traverse(p.phi).map(_._2)

      val pc = if (pathsToCalls.nonEmpty) {
        Not(Or(pathsToCalls))
      } else {
        BooleanLiteral(false)
      }

      // Creates two alternative branches:
      // 1) a version with holes unreachable, on which this rule won't re-apply
      val sfact  = new TimeoutSolverFactory(sctx.solverFactory, 500L)
      val solver = SimpleSolverAPI(new TimeoutSolverFactory(sctx.solverFactory, 2000L))

      val(holesAvoidable, _) = solver.solveSAT(And(p.pc, pc))

      val avoid = if (holesAvoidable != Some(false)) {
        val newPhi = simplifyPaths(sfact)(And(pc, p.phi))
        val newProblem1 = p.copy(phi = newPhi)

        Some(RuleInstantiation.immediateDecomp(p, this, List(newProblem1), {
          case List(s) if (s.pre != BooleanLiteral(false)) => Some(s)
          case _ => None
        }, "Avoid Holes"))
      } else {
        None
      }


      // 2) a version with holes reachable to continue applying itself
      val newPhi                 = inlineUntilHoles(p.phi)
      val (newXs, newPhiInlined) = inlineHoles(newPhi)

      val newProblem2 = p.copy(phi = newPhiInlined, xs = p.xs ::: newXs)
      val rec = Some(RuleInstantiation.immediateDecomp(p, this, List(newProblem2), project(p.xs.size), "Inline Holes"))

      List(rec, avoid).flatten
    } else {
      Nil
    }
  }
}
