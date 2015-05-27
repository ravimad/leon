import leon.lang._
import leon.annotation._
import leon.collection._
import leon._
import leon.lang.invariantLang._


// object List {
//   sealed abstract class List[T]
//   case class Cons[T](h: T, t: List[T]) extends List[T]
//   case class Nil[T]() extends List[T]

//   def apply[T](elements: Seq[T]): List[T] = {
//     elements.foldLeft(Nil[T](): List[T])((acc: List[T], element: T) => Cons[T](element, acc))
//   }
// }

object IntLattice {
  import List._

  abstract class Element
  case class Bot() extends Element
  case class Top() extends Element
  case class IntVal(x: Int) extends Element

  def height = {
    /**
     * A number that depends on the lattice definition.
     * In simplest case it has height 3 (_|_ (bot) <= Ints <= T (top))
     */
    3
  }

  def join(oldVal: Element, newVal: Element) = (oldVal, newVal) match {
    case (Bot(), any) => any // bot is the identity for join
    case (any, Bot()) => any
    case (Top(), _) => Top() // top joined with anything is top
    case (_, Top()) => Top()
    case (IntVal(x), IntVal(y)) if (x == y) => IntVal(y)
    case _ =>
      //here old and new vals are different integers
      Top()
  }

  def add(a: Element, b: Element): Element = {
    (a, b) match {
      case (Bot(), _) => Bot()
      case (_, Bot()) => Bot()
      case (Top(), _) => Top()
      case (_, Top()) => Top()
      case (IntVal(x), IntVal(y)) => IntVal(x + y)
    }
  }

  def multiply(a: Element, b: Element): Element = {
    (a, b) match {
      case (_, IntVal(x)) if x == 0 => IntVal(0)
      case (IntVal(x), _) if x == 0 => IntVal(0)
      case (Bot(), _) => Bot()
      case (_, Bot()) => Bot()
      case (Top(), _) => Top()
      case (_, Top()) => Top()
      case (IntVal(x), IntVal(y)) => IntVal(x * y)
    }
  }
}

object ConstantPropagation {
  import IntLattice._
  import List._

  // def printTabs(n: Int): Unit = {
  //     if (n == 1) print(" ")
  //     else {
  //       print(" ")
  //       printTabs(n - 1)
  //     }
  //   }

  // def printExprList(l: List[Expr], tabs: Int): Unit = {
  //   l match {
  //     case Nil() => print("")
  //     case Cons(e, tail) => {
  //       e.printExpr(tabs)
  //       printExprList(tail, tabs)
  //     }
  //   }
  // }

  abstract class Expr
  case class Times(lhs: Expr, rhs: Expr) extends Expr
  case class Plus(lhs: Expr, rhs: Expr) extends Expr
  case class IntLiteral(v: Int) extends Expr
  case class FunctionCall(calleeName: Identifier, args: List[Expr]) extends Expr
  case class IfThenElse(cond: Expr, thenExpr: Expr, elseExpr: Expr) extends Expr
  case class Identifier(id: Int) extends Expr
  // can you take care of this in size computation etc.

  /**
   * Definition of a function AST
   */
  case class Function(name: Identifier, params: List[Expr], body: Expr)

  /**
   * Assuming that the functions are ordered from callee to
   * caller and there is no mutual recursion
   */
  case class Program(funcs: List[Function])

  def size(l: List[Function]): Int = {
    l match {
      case Cons(_, t) => 1 + size(t)
      case Nil() => 0
    }
  }

  def sizeExprList(exprs: List[Expr]): Int = {
    exprs match {
      case Nil() => 0
      case Cons(currExpr, otherExprs) => sizeExpr(currExpr) + sizeExprList(otherExprs)
    }
  }

  def sizeExpr(e: Expr): Int = {
    e match {
      case Plus(l, r) => 1 + sizeExpr(l) + sizeExpr(r)
      case Times(l, r) => 1 + sizeExpr(l) + sizeExpr(r)
      case FunctionCall(c, args) => {
        1 + sizeExprList(args)
      }
      case IfThenElse(c, th, el) =>
        1 + sizeExpr(c) + sizeExpr(th) + sizeExpr(el)
      case _ => 1
    }
  }

  def sizeFuncList(funcs: List[Function]): Int = {
    funcs match {
      case Nil() => 0
      case Cons(currFunc, otherFuncs) =>
        1 + sizeExpr(currFunc.body) + sizeFuncList(otherFuncs)
    }
  }

  def initToBot(l: List[Function]): List[(Int /*function id*/ , Element)] = {
    l match {
      case Nil() => Nil[(Int /*function id*/ , Element)]()
      case Cons(fun, tail) => Cons((fun.name.id, Bot()), initToBot(tail))
    }
  } ensuring (res => true template ((a, b) => time <= a * size(l) + b))

  def foldConstants(p: Program): Program = {
    val fvals = computeSummaries(p, initToBot(p.funcs), height)
    val newfuns = transformFuns(p.funcs, fvals)
    Program(newfuns)
  } // ensuring(res => true template((a, b, c) => time <= (height*sizeFuncList(p.funcs))*a + height*b + c))

  /**
   * The initVals is the initial values for the
   * values of the functions
   */
  def computeSummaries(p: Program, initVals: List[(Int /*function id*/ , Element)], noIters: Int): List[(Int /*function id*/ , Element)] = {
    require(noIters >= 0)
    if (noIters <= 0) {
      initVals
    } else
      computeSummaries(p, analyzeFuns(p.funcs, initVals, initVals), noIters - 1)
  } ensuring (res => true template ((a, b, d) => time <= a * (sizeFuncList(p.funcs) * noIters) + b * noIters + d))

  /**
   * Initial fvals and oldVals are the same
   * but as the function progresses, fvals will only have the olds values
   * of the functions that are yet to be processed, whereas oldVals will remain the same.
   */
  def analyzeFuns(funcs: List[Function], fvals: List[(Int, Element)], oldVals: List[(Int, Element)]): List[(Int, Element)] = {
    (funcs, fvals) match {
      case (Cons(f, otherFuns), Cons((fid, fval), otherVals)) =>
        val newval = analyzeFunction(f, oldVals)
        val approxVal = join(fval, newval) //creates an approximation of newVal to ensure convergence
        Cons((fid, approxVal), analyzeFuns (otherFuns, otherVals, oldVals))
      case _ =>
        Nil[(Int, Element)]() //this also handles precondition violations e.g. lists aren't of same size etc.
    }
  } ensuring (res => true template ((a, b) => time <= a * sizeFuncList(funcs) + b))

  // this is not necesary as the summaries (i.e, funVals) hold for any arguments
  // not necessarily constant arguments e.g. f(x,y) = { 0 } is a constant irrespective
  // of x and y
  /*def isConstantCall(args: List[Expr]): Boolean = {
    args match {
      case Nil() => true
      case Cons(IntLiteral(_), otherExprs) => isConstantCall(otherExprs)
      case _ => false
    }
  }*/

  @constantTime
  def getFunctionVal(funcId: Int, funcVals: List[(Int, Element)]): Element = {
    funcVals match {
      case Nil() => Bot()
      case Cons((currFuncId, currFuncVal), otherFuncVals) if (currFuncId == funcId) => currFuncVal
      case Cons(_, otherFuncVals) =>
        getFunctionVal(funcId, otherFuncVals)
    }
  }

  def analyzeExprList(l: List[Expr], funcVals: List[(Int, Element)]): List[Element] = {
    l match {
      case Nil() => Nil[Element]()
      case Cons(expr, otherExprs) => Cons(analyzeExpr(expr, funcVals), analyzeExprList(otherExprs, funcVals))
    }
  } ensuring (res => true template ((a, b) => time <= a * sizeExprList(l) + b))

  /**
   * Returns the value of the expression when "abstractly interpreted"
   * using the lattice.
   */
  def analyzeExpr(e: Expr, funcVals: List[(Int, Element)]): Element = {
    e match {
      case Times(lhs: Expr, rhs: Expr) => {
        val lval = analyzeExpr(lhs, funcVals)
        val rval = analyzeExpr(rhs, funcVals)
        multiply(lval, rval)
      }
      case Plus(lhs: Expr, rhs: Expr) => {
        val lval = analyzeExpr(lhs, funcVals)
        val rval = analyzeExpr(rhs, funcVals)
        add(lval, rval)
      }
      case FunctionCall(calleeName, args: List[Expr]) => {
        getFunctionVal(calleeName.id, funcVals)
      }
      case IfThenElse(c, th, el) => {
        //analyze then and else branches and join their values
        //TODO: this can be made more precise e.g. if 'c' is
        //a non-zero value it can only execute the then branch.
        val v1 = analyzeExpr(th, funcVals)
        val v2 = analyzeExpr(el, funcVals)
        join(v1, v2)
      }
      case lit @ IntLiteral(v) =>
        IntVal(v)
    }
  } ensuring (res => true template ((a, b) => time <= a * sizeExpr(e) + b))

  def analyzeFunction(f: Function, oldVals: List[(Int, Element)]): Element = {
    // traverse the body of the function and simplify constants
    // for function calls assume the value given by oldVals
    // also for if-then-else statments, take a join of the values along if and else branches
    // assume that bot op any = bot and top op any = top (but this can be made more precise).
    analyzeExpr(f.body, oldVals)

  } // ensuring (res => true template ((a, b) => time <= a * sizeExpr(f.body) + b))

  def transformExprList(l: List[Expr], funcVals: List[(Int, Element)]): List[Expr] = {
    l match {
      case Nil() => Nil[Expr]()
      case Cons(expr, otherExprs) => Cons(transformExpr(expr, funcVals),
        transformExprList(otherExprs, funcVals))
    }
  } ensuring (res => true template ((a, b) => time <= a * sizeExprList(l) + b))

  /**
   * Returns the folded expression
   */
  def transformExpr(e: Expr, funcVals: List[(Int, Element)]): Expr = {
    e match {
      case Times(lhs: Expr, rhs: Expr) => {
        val foldedLHS = transformExpr(lhs, funcVals)
        val foldedRHS = transformExpr(rhs, funcVals)
        (foldedLHS, foldedRHS) match {
          case (IntLiteral(x), IntLiteral(y)) =>
            IntLiteral(x * y)
          case _ =>
            Times(foldedLHS, foldedRHS)
        }
      }
      case Plus(lhs: Expr, rhs: Expr) => {
        val foldedLHS = transformExpr(lhs, funcVals)
        val foldedRHS = transformExpr(rhs, funcVals)
        (foldedLHS, foldedRHS) match {
          case (IntLiteral(x), IntLiteral(y)) =>
            IntLiteral(x + y)
          case _ =>
            Plus(foldedLHS, foldedRHS)
        }
      }
      case FunctionCall(calleeName, args: List[Expr]) => {
        getFunctionVal(calleeName.id, funcVals) match {
          case IntVal(x) =>
            IntLiteral(x)
          case _ =>
            val foldedArgs = transformExprList(args, funcVals)
            FunctionCall(calleeName, foldedArgs)
        }
      }
      case IfThenElse(c, th, el) => {
        val foldedCond = transformExpr(c, funcVals)
        val foldedTh = transformExpr(th, funcVals)
        val foldedEl = transformExpr(el, funcVals)
        foldedCond match {
          case IntLiteral(x) => {
            if (x != 0) foldedTh
            else foldedEl
          }
          case _ => IfThenElse(foldedCond, foldedTh, foldedEl)
        }
      }
      case lit @ IntLiteral(v) =>
        lit
    }
  } ensuring (res => true template ((a, b) => time <= a * sizeExpr(e) + b))

  def transformFuns(funcs: List[Function], fvals: List[(Int, Element)]): List[Function] = {
    funcs match {
      case Cons(f, otherFuns) =>
        val newfun = Function(f.name, f.params, transformExpr(f.body, fvals))
        Cons(newfun, transformFuns(otherFuns, fvals))
      case _ =>
        Nil[Function]()
    }
  } ensuring (res => true template ((a, b) => time <= a * sizeFuncList(funcs) + b))

  // def main(args: Array[String]): Unit = {
  //   // val f100 =
  //   //   Function(
  //   //     Identifier(100),
  //   //     List(Seq(Identifier(1), Identifier(2), Identifier(3))),

  //   //     // Body
  //   //     Plus(Times(IntLiteral(23), IntLiteral(4)), IntLiteral(23))
  //   //   )

  //   // val f100 =
  //   //   Function(
  //   //     Identifier(100),
  //   //     List(Seq(Identifier(1))),

  //   //     // Body
  //   //     IfThenElse(
  //   //       Plus(IntLiteral(1), IntLiteral(2)),
  //   //       IntLiteral(10),
  //   //       FunctionCall(Identifier(100), List(Seq(IntLiteral(10))))
  //   //     )
  //   //   )

  //   val f100 =
  //     Function (
  //       Identifier(100),
  //       List(Seq(Identifier(1))),

  //       // Body
  //       IfThenElse(
  //         Plus(IntLiteral(1), IntLiteral(-1)),
  //         IntLiteral(20),
  //         FunctionCall(Identifier(200), List(Seq(IntLiteral(10))))
  //       )
  //     )

  //   val f200 =
  //     Function(
  //       Identifier(200),
  //       List(Seq(Identifier(1))),

  //       // Body
  //       IfThenElse(
  //         Plus(IntLiteral(1), IntLiteral(2)),
  //         IntLiteral(10),
  //         FunctionCall(Identifier(100), List(Seq(IntLiteral(10))))
  //       )
  //     )

  //   val prog = Program(List(Seq(f100, f200)))
  //   val newProg = foldConstants(prog)
  //   println("Old Program: \n" + prog)
  //   println("\nNew Program: \n" + newProg)

  //   prog.printProg()
  //   newProg.printProg()
  // }
}
