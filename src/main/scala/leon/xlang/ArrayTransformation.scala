/* Copyright 2009-2014 EPFL, Lausanne */

package leon.xlang

import leon.TransformationPhase
import leon.LeonContext
import leon.purescala.Common._
import leon.purescala.Definitions._
import leon.purescala.Trees._
import leon.xlang.Trees._
import leon.purescala.Extractors._
import leon.purescala.TypeTrees._

object ArrayTransformation extends TransformationPhase {

  val name = "Array Transformation"
  val description = "Add bound checking for array access and remove array update with side effect"

  private var id2FreshId = Map[Identifier, Identifier]()

  def apply(ctx: LeonContext, pgm: Program): Program = {

    id2FreshId = Map()
    val allFuns = pgm.definedFunctions
    allFuns.foreach(fd => {
      id2FreshId = Map()
      fd.precondition = fd.precondition.map(transform)
      fd.body = fd.body.map(transform)
      fd.postcondition = fd.postcondition.map { case (id, post) => (id, transform(post)) }
    })
    pgm
  }


  def transform(expr: Expr): Expr = (expr match {
    case up@ArrayUpdate(a, i, v) => {
      val ra = transform(a)
      val ri = transform(i)
      val rv = transform(v)
      val Variable(id) = ra
      Assignment(id, ArrayUpdated(ra, ri, rv).setType(ra.getType).setPos(up))
    }
    case ArrayClone(a) => {
      val ra = transform(a)
      ra
    }
    case Let(i, v, b) => {
      v.getType match {
        case ArrayType(_) => {
          val freshIdentifier = FreshIdentifier("t").setType(i.getType)
          id2FreshId += (i -> freshIdentifier)
          LetVar(freshIdentifier, transform(v), transform(b))
        }
        case _ => Let(i, transform(v), transform(b))
      }
    }
    case Variable(i) => {
      val freshId = id2FreshId.get(i).getOrElse(i)
      Variable(freshId)
    }

    case LetVar(id, e, b) => {
      val er = transform(e)
      val br = transform(b)
      LetVar(id, er, br)
    }
    case wh@While(c, e) => {
      val newWh = While(transform(c), transform(e))
      newWh.invariant = wh.invariant.map(i => transform(i))
      newWh.setPos(wh)
      newWh
    }

    case ite@IfExpr(c, t, e) => {
      val rc = transform(c)
      val rt = transform(t)
      val re = transform(e)
      IfExpr(rc, rt, re).setType(rt.getType)
    }

    case m @ MatchExpr(scrut, cses) => {
      val scrutRec = transform(scrut)
      val csesRec = cses.map{
        case SimpleCase(pat, rhs) => SimpleCase(pat, transform(rhs))
        case GuardedCase(pat, guard, rhs) => GuardedCase(pat, transform(guard), transform(rhs))
      }
      val tpe = csesRec.head.rhs.getType
      MatchExpr(scrutRec, csesRec).setType(tpe).setPos(m)
    }
    case LetDef(fd, b) => {
      fd.precondition = fd.precondition.map(transform)
      fd.body = fd.body.map(transform)
      fd.postcondition = fd.postcondition.map { case (id, post) => (id, transform(post)) }
      val rb = transform(b)
      LetDef(fd, rb)
    }
    case n @ NAryOperator(args, recons) => recons(args.map(transform)).setType(n.getType)
    case b @ BinaryOperator(a1, a2, recons) => recons(transform(a1), transform(a2)).setType(b.getType)
    case u @ UnaryOperator(a, recons) => recons(transform(a)).setType(u.getType)

    case (t: Terminal) => t
    case unhandled => scala.sys.error("Non-terminal case should be handled in ArrayTransformation: " + unhandled)
  }).setPos(expr)

}
