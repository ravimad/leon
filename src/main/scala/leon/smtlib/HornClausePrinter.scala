package leon
package smtlib

import purescala._
import Common._
import Trees._
import Extractors._
import TreeOps._
import TypeTrees._
import Definitions._

import SExprs._

/** This pretty-printer prints an SMTLIB2 representation of the Purescala program */
object HornClausePrinter {

  private var errorConstants: Set[(SSymbol, SExpr)] = Set()

  def apply(pgm: Program): String = {
    errorConstants = Set()

    val defs: Seq[ClassTypeDef] = pgm.definedClasses

    val partition: Seq[(AbstractClassDef, Seq[CaseClassDef])] = {
      val parents: Seq[AbstractClassDef] = defs.filter(!_.hasParent).asInstanceOf[Seq[AbstractClassDef]]
      parents.map(p => (p, defs.filter(c => c.parent match {
        case Some(p2) => p == p2
        case None => false
      }).asInstanceOf[Seq[CaseClassDef]]))
    }

    val sorts: Seq[SExpr] = partition.map{ case (parent, children) => {
      val name = id2sym(parent.id)
      val constructors: List[SExpr] = children.map(child => {
        val fields: List[SExpr] = child.fields.map{case VarDecl(id, tpe) => SList(id2sym(id), tpe2sort(tpe)) }.toList
        if(fields.isEmpty) id2sym(child.id) else SList(id2sym(child.id) :: fields)
      }).toList

      SList(name :: constructors)
    }}
    val sortDecls = SList(SSymbol("declare-datatypes"), SList(), SList(sorts.toList))

    val convertedFunDefs = pgm.definedFunctions.map(fd2sexp)
    
    val sortErrors: List[SExpr] = errorConstants.map(p => SList(SSymbol("declare-const"), p._1, p._2)).toList

    (
      SComment("! THEORY=1") +:
      SComment("Generated by Leon") +:
      sortDecls +: (
        sortErrors ++
        convertedFunDefs.flatMap(_._1) ++
        convertedFunDefs.map(_._2) ++
        convertedFunDefs.flatMap(_._3) ++
        Seq(SList(SSymbol("check-sat")))
      )
    ).map(SExprs.toString(_)).mkString("\n")

  }


  private def id2sym(id: Identifier) = SSymbol(id.uniqueName)

  //return a series of declarations, an expression that defines the function, 
  //and the seq of asserts for pre/post-conditions
  private def fd2sexp(funDef: FunDef): (Seq[SExpr], SExpr, Seq[SExpr]) = {
    val name = id2sym(funDef.id)
    val returnSort = tpe2sort(funDef.returnType)
    val varDecls: List[(SSymbol, SExpr)] = funDef.args.map(vd => (id2sym(vd.id), tpe2sort(vd.tpe))).toList

    var declarations: List[SExpr] = List(SList(SSymbol("declare-fun"), name, SList(varDecls.map(_._2) ::: List(returnSort)), SSymbol("Bool")))
    
    val cleanBody = expandLets(matchToIfThenElse(funDef.body.get))

    //val bodyWithoutPre: SExpr = SList(SSymbol("="),
    //  SList(name :: varDecls.map(_._1)),
    //  exp2sexp(cleanBody))
    //val body = if(funDef.hasPrecondition) 
    //    SList(SSymbol("=>"), exp2sexp(matchToIfThenElse(funDef.getPrecondition)), bodyWithoutPre)
    //  else 
    //    bodyWithoutPre

    var fi2fresh: Map[FunctionInvocation, Identifier] = Map()
    val cleanBodyWithoutRec = searchAndReplaceDFS((exp: Expr) => exp match {
      case fi@FunctionInvocation(fd, args) => fi2fresh.get(fi) match {
        case Some(id) => Some(id.toVariable)
        case None => {
          val id = FreshIdentifier("res").setType(fd.returnType)
          fi2fresh += (fi -> id)
          Some(id.toVariable)
        }
      }
      case _ => None
    })(cleanBody)

    val resSymbols: List[SExpr] = fi2fresh.map((p: (FunctionInvocation, Identifier)) => SList(id2sym(p._2), tpe2sort(p._2.getType))).toList
    val inductiveConds: List[SExpr] = fi2fresh.map{
      case (FunctionInvocation(fd, args), id) => SList(id2sym(fd.id) :: (args.map(exp2sexp) :+ id2sym(id)).toList)
    }.toList

    val functionIsBody = SList(name :: (varDecls.map(p => p._1) :+ exp2sexp(cleanBodyWithoutRec)))

    val body = 
      if(resSymbols.isEmpty) { 
        if(inductiveConds.isEmpty)
          functionIsBody
        else
          SList(SSymbol("=>"), SList(SSymbol("and") :: inductiveConds), functionIsBody)
      } else {
        if(inductiveConds.isEmpty)
          SList(SSymbol("forall"), SList(resSymbols), functionIsBody)
        else
          SList(SSymbol("forall"), SList(resSymbols), SList(SSymbol("=>"), SList(SSymbol("and") :: inductiveConds), functionIsBody))
      }


    val definition = SList(SSymbol("forall"),
          SList(varDecls.map(p => SList(p._1, p._2)).toList),
          body)

    val vcs: Seq[SExpr] = {

      val post: Seq[SExpr] = if(funDef.hasPostcondition) {
        val (resvar, postExpr) = funDef.postcondition.get
        //why need fresh Ids
        val freshIds = funDef.args.map(vd => FreshIdentifier(vd.id.name).setType(vd.tpe))
        //val freshRes = FreshIdentifier("res").setType(funDef.returnType)
        //val freshPost = replace(Map(ResultVariable() -> freshRes.toVariable), funDef.getPostcondition)
        val freshPost = postExpr
        val invoc = SList(name :: (freshIds.map(id2sym) :+ id2sym(resvar)).toList)
        Seq(SList(SSymbol("forall"),
            SList(SList(id2sym(resvar), tpe2sort(resvar.getType)) :: freshIds.map(id => SList(id2sym(id), tpe2sort(id.getType))).toList),
            SList(SSymbol("=>"), invoc, exp2sexp(freshPost))))
      } else Seq()

      //val precs: Seq[SExpr] = {

      //  def withPrecIfDefined(path: Seq[Expr], shouldHold: Expr) : Expr = if(funDef.hasPrecondition) {
      //    Not(And(And(matchToIfThenElse(funDef.precondition.get) +: path), Not(shouldHold)))
      //  } else {
      //    Not(And(And(path), Not(shouldHold)))
      //  }

      //  val allPathConds = collectWithPathCondition((t => t match {
      //    case FunctionInvocation(funDef, _) if(funDef.hasPrecondition) => true
      //    case _ => false
      //  }), cleanBody)

      //  allPathConds.map(pc => {
      //    val path : Seq[Expr] = pc._1
      //    val fi = pc._2.asInstanceOf[FunctionInvocation]
      //    val FunctionInvocation(funDef, args) = fi
      //    val prec: Expr = freshenLocals(matchToIfThenElse(funDef.precondition.get))
      //    val newLetIDs = funDef.args.map(a => FreshIdentifier("arg_" + a.id.name, true).setType(a.tpe))
      //    val substMap = Map[Expr,Expr]((funDef.args.map(_.toVariable) zip newLetIDs.map(Variable(_))) : _*)
      //    val newBody : Expr = replace(substMap, prec)
      //    val newCall : Expr = (newLetIDs zip args).foldRight(newBody)((iap, e) => Let(iap._1, iap._2, e))
      //    exp2sexp(Not(withPrecIfDefined(path, newCall)))
      //  }).toList
      //}

      post//++ precs
    }

    (declarations, 
     SList(SSymbol("assert"), definition), 
     vcs.map(SList(SSymbol("assert"), _))
    )
  }


  def tpe2sort(tpe: TypeTree): SExpr = tpe match {
    case Int32Type => SSymbol("Int")
    case BooleanType => SSymbol("Bool")
    case ArrayType(baseTpe) => SList(SSymbol("Array"), SSymbol("Int"), tpe2sort(baseTpe))
    case AbstractClassType(abs) => id2sym(abs.id)
    case _ => sys.error("TODO tpe2sort: " + tpe)
  }

  def exp2sexp(tree: Expr): SExpr = tree match {
    case Variable(id) => id2sym(id)
    //case LetTuple(ids,d,e) => SList(List(
    //  SSymbol("let"),
    //  SList(List(ids))
    case Let(b,d,e) => SList(
      SSymbol("let"),
      SList(
        SList(id2sym(b), exp2sexp(d))
      ),
      exp2sexp(e)
    )
    case And(exprs) => SList(SSymbol("and") :: exprs.map(exp2sexp).toList)
    case Or(exprs) => SList(SSymbol("or") :: exprs.map(exp2sexp).toList)
    case Not(expr) => SList(SSymbol("not"), exp2sexp(expr))
    case Equals(l,r) => SList(SSymbol("="), exp2sexp(l), exp2sexp(r))
    case IntLiteral(v) => SInt(v)
    case BooleanLiteral(v) => SSymbol(v.toString) //TODO: maybe need some builtin type here
    case StringLiteral(s) => SString(s)

    case Implies(l,r) => SList(SSymbol("=>"), exp2sexp(l), exp2sexp(r))
    case Iff(l,r) => SList(SSymbol("="), exp2sexp(l), exp2sexp(r))

    case Plus(l,r) => SList(SSymbol("+"), exp2sexp(l), exp2sexp(r))
    case UMinus(expr) => SList(SSymbol("-"), exp2sexp(expr))
    case Minus(l,r) => SList(SSymbol("-"), exp2sexp(l), exp2sexp(r))
    case Times(l,r) => SList(SSymbol("*"), exp2sexp(l), exp2sexp(r))
    case Division(l,r) => SList(SSymbol("div"), exp2sexp(l), exp2sexp(r))
    case Modulo(l,r) => SList(SSymbol("mod"), exp2sexp(l), exp2sexp(r))
    case LessThan(l,r) => SList(SSymbol("<"), exp2sexp(l), exp2sexp(r))
    case LessEquals(l,r) => SList(SSymbol("<="), exp2sexp(l), exp2sexp(r))
    case GreaterThan(l,r) => SList(SSymbol(">"), exp2sexp(l), exp2sexp(r))
    case GreaterEquals(l,r) => SList(SSymbol(">="), exp2sexp(l), exp2sexp(r))

    case IfExpr(c, t, e) => SList(SSymbol("ite"), exp2sexp(c), exp2sexp(t), exp2sexp(e))

    case FunctionInvocation(fd, args) => SList(id2sym(fd.id) :: args.map(exp2sexp).toList)

    case ArrayFill(length, defaultValue) => SList(
      SList(SSymbol("as"), SSymbol("const"), tpe2sort(tree.getType)),
      exp2sexp(defaultValue)
    )
    case ArrayMake(defaultValue) => SList(
      SList(SSymbol("as"), SSymbol("const"), tpe2sort(tree.getType)),
      exp2sexp(defaultValue)
    )
    case ArraySelect(array, index) => SList(SSymbol("select"), exp2sexp(array), exp2sexp(index))
    case ArrayUpdated(array, index, newValue) => SList(SSymbol("store"), exp2sexp(array), exp2sexp(index), exp2sexp(newValue))

    case CaseClass(ccd, args) => SList(id2sym(ccd.id) :: args.map(exp2sexp(_)).toList)
    case CaseClassSelector(_, arg, field) => SList(id2sym(field), exp2sexp(arg))

    case CaseClassInstanceOf(ccd, arg) => {
      val name = id2sym(ccd.id)
      val testerName = SSymbol("is-" + name.s)
      SList(testerName, exp2sexp(arg))
    }

    case er@Error(_) => {
      val id = id2sym(FreshIdentifier("error_value").setType(er.getType))
      errorConstants += ((id, tpe2sort(er.getType)))
      id
    }

    case o => sys.error("TODO converting to smtlib: " + o)
  }

  // prec: there should be no lets and no pattern-matching in this expression
  def collectWithPathCondition(matcher: Expr=>Boolean, expression: Expr) : Set[(Seq[Expr],Expr)] = {
    var collected : Set[(Seq[Expr],Expr)] = Set.empty

      def rec(expr: Expr, path: List[Expr]) : Unit = {
        if(matcher(expr)) {
          collected = collected + ((path.reverse, expr))
        }

        expr match {
          case Let(i,e,b) => {
            rec(e, path)
              rec(b, Equals(Variable(i), e) :: path)
          }
          case IfExpr(cond, then, elze) => {
            rec(cond, path)
              rec(then, cond :: path)
              rec(elze, Not(cond) :: path)
          }
          case NAryOperator(args, _) => args.foreach(rec(_, path))
            case BinaryOperator(t1, t2, _) => rec(t1, path); rec(t2, path)
            case UnaryOperator(t, _) => rec(t, path)
          case t : Terminal => ;
          case _ => scala.sys.error("Unhandled tree in collectWithPathCondition : " + expr)
        }
      }

    rec(expression, Nil)
      collected
  }
}
