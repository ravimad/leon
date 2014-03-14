package leon
package invariant

import z3.scala._
import purescala._
import purescala.Common._
import purescala.Definitions._
import purescala.Trees._
import purescala.TreeOps._
import purescala.Extractors._
import purescala.TypeTrees._
import solvers.{ Solver, TimeoutSolver }
import solvers.z3.FairZ3Solver
import leon.evaluators._
import java.io._
import leon.solvers.z3.UninterpretedZ3Solver
import leon.LeonContext
import leon.LeonOptionDef
import leon.LeonPhase
import leon.LeonValueOption
import leon.ListValue
import leon.Reporter
import leon.verification.DefaultTactic
import leon.verification.ExtendedVC
import leon.verification.Tactic
import leon.verification.VerificationReport

//A set of implications
//'initexpr' is required to be in negation normal form and And/Ors have been pulled up
//TODO: optimize the representation so that we use fewer guards.
class Formula(initexpr: Expr) {
  
  val fls = BooleanLiteral(false)
  val tru = BooleanLiteral(true)
  val useImplies = true
  
  val combiningOp = if(useImplies) Implies.apply _ else Equals.apply _  
  protected var disjuncts = Map[Variable, Seq[Constraint]]() //a mapping from guards to conjunction of atoms
  protected var conjuncts = Map[Variable, Expr]() //a mapping from guards to disjunction of atoms  
  val firstRoot : Variable = addConstraints(initexpr)._1  
  protected var roots : Seq[Variable] = Seq(firstRoot) //a list of roots, the formula is a conjunction of formula of each root
    
  def disjunctsInFormula = disjuncts   
  
  //return the root variable and the sequence of disjunct guards added 
  //(which includes the root variable incase it respresents a disjunct)
  def addConstraints(ine: Expr) : (Variable, Seq[Variable]) = {
    
    var newDisjGuards = Seq[Variable]()    
    
    def getCtrsFromExprs(exprs: Seq[Expr]) : Seq[Constraint] = {
      var break = false
      exprs.foldLeft(Seq[Constraint]())((acc, e) => {
        if (break) acc
        else {
          val ctr = ConstraintUtil.createConstriant(e)
          ctr match {
            case BoolConstraint(BooleanLiteral(true)) => acc
            case BoolConstraint(BooleanLiteral(false)) => {
              break = true
              Seq(ctr)
            }
            case _ => acc :+ ctr
          }
        }
      })
    }
    
    val f1 = simplePostTransform((e: Expr) => e match {
      case Or(args) => {
        val newargs = args.map(arg => arg match {
          case v: Variable if (disjuncts.contains(v)) => arg
          case v: Variable if (conjuncts.contains(v)) => throw IllegalStateException("or gaurd inside conjunct: "+e+" or-guard: "+v)
          case _ => {
            val atoms = arg  match {
              case And(atms) => atms
              case _ => Seq(arg)
            }              
            val g = TVarFactory.createTemp("b").setType(BooleanType).toVariable
            newDisjGuards :+= g
            //println("atoms: "+atoms)
            val ctrs = getCtrsFromExprs(atoms)            
            disjuncts += (g -> ctrs)                
            g
          }
        })
        //create a temporary for Or
        val gor = TVarFactory.createTemp("b").setType(BooleanType).toVariable
        val newor = Or(newargs)        
        conjuncts += (gor -> newor)
        gor
      }
      case And(args) => {
        val newargs = args.map(arg => if (InvariantUtil.getTemplateVars(e).isEmpty) {
          arg
        } else {
          //if the expression has template variables then we separate it using guards
          val g = TVarFactory.createTemp("b").setType(BooleanType).toVariable
          newDisjGuards :+= g
          val ctrs = getCtrsFromExprs(Seq(arg))
          disjuncts += (g -> ctrs)
          g
        })
        And(newargs)
      }       
      case _ => e
    })(ExpressionTransformer.simplify(simplifyArithmetic(ine)))
    
    val rootvar = f1 match {      
      case v: Variable if(conjuncts.contains(v)) => v
      case v: Variable if(disjuncts.contains(v)) => throw IllegalStateException("f1 is a disjunct guard: "+v)
      case _ => {
        val atoms = f1 match {
          case And(atms) => atms
          case _ => Seq(f1)
        }
        val g = TVarFactory.createTemp("b").setType(BooleanType).toVariable
        val ctrs = getCtrsFromExprs(atoms)
        newDisjGuards :+= g
        disjuncts += (g -> ctrs)        
        g
      }
    }
    (rootvar, newDisjGuards)
  }
  
  //'satGuard' is required to a guard variable
  def pickSatDisjunct(startGaurd : Variable, model: Map[Identifier, Expr]): Seq[Constraint] = {
        
    def traverseOrs(gd: Variable, model: Map[Identifier, Expr]): Seq[Variable] = {
      val e @ Or(guards) = conjuncts(gd)
      //pick one guard that is true
      val guard = guards.collectFirst { case g @ Variable(id) if (model(id) == tru) => g }
      if (!guard.isDefined)
        throw IllegalStateException("No satisfiable guard found: " + e)
      guard.get +: traverseAnds(guard.get, model)
    }

    def traverseAnds(gd: Variable, model: Map[Identifier, Expr]): Seq[Variable] = {
      val ctrs = disjuncts(gd)
      val guards = ctrs.collect {
        case BoolConstraint(v @ Variable(_)) if (conjuncts.contains(v) || disjuncts.contains(v)) => v
      }
      if (guards.isEmpty) Seq()
      else {
        guards.foldLeft(Seq[Variable]())((acc, g) => {
          if (model(g.id) != tru)
            throw IllegalStateException("Not a satisfiable guard: " + g)

          if (conjuncts.contains(g))
            acc ++ traverseOrs(g, model)
          else {
            acc ++ (g +: traverseAnds(g, model))
          }
        })
      }
    }
    //if startGuard is unsat return empty
    if (model(startGaurd.id) == fls) Seq()
    else {
      val satGuards = if (conjuncts.contains(startGaurd)) traverseOrs(startGaurd, model)
      else (startGaurd +: traverseAnds(startGaurd, model))
      satGuards.flatMap(g => disjuncts(g))
    }
  }
  
  //'neweexpr' is required to be in negation normal form and And/Ors have been pulled up  
  def conjoinWithDisjunct(guard: Variable, newexpr: Expr) : (Variable, Seq[Variable]) = {
     val (exprRoot, newGaurds) = addConstraints(newexpr)
     //add 'newguard' in conjunction with 'disjuncts(guard)'
     val ctrs = disjuncts(guard)
     disjuncts -= guard
     disjuncts += (guard -> (BoolConstraint(exprRoot) +: ctrs))
     (exprRoot, newGaurds)
  }

  def conjoinWithRoot(newexpr: Expr): (Variable, Seq[Variable]) = {
    val (exprRoot, newGaurds) = addConstraints(newexpr)       
    roots :+= exprRoot
    (exprRoot, newGaurds)
  }
  
  /**
   * The first return value is param part and the second one is the 
   * non-parametric part
   */
  def splitParamPart : (Expr, Expr) = {    
    var paramPart = Seq[Expr]()
    var rest = Seq[Expr]()
    disjuncts.foreach(entry => {
      val (g,ctrs) = entry
      val ctrExpr = combiningOp(g,And(ctrs.map(_.toExpr)))
      if(InvariantUtil.getTemplateVars(ctrExpr).isEmpty) 
        rest :+= ctrExpr
      else
        paramPart :+= ctrExpr
        
    })    
    val conjs = conjuncts.map((entry) => combiningOp(entry._1, entry._2)).toSeq ++ roots    
    (And(paramPart), And(rest ++ conjs ++ roots))
  }  
  
  def toExpr : Expr={
    val disjs = disjuncts.map((entry) => {
      val (g,ctrs) = entry
      combiningOp(g, And(ctrs.map(_.toExpr)))
    }).toSeq
    val conjs = conjuncts.map((entry) => combiningOp(entry._1, entry._2)).toSeq
    And(disjs ++ conjs ++ roots)
  } 
  
  //unpack the disjunct and conjuncts by removing all guards
  def unpackedExpr : Expr = {
    //replace all conjunct guards in disjuncts by their mapping
    val disjs : Map[Expr,Expr] = disjuncts.map((entry) => {
      val (g,ctrs) = entry
      val newctrs = ctrs.map(_ match {
        case BoolConstraint(g@Variable(_)) if conjuncts.contains(g) => conjuncts(g)
        case ctr@_ => ctr.toExpr 
      })
      (g, And(newctrs))
    })
    val rootexprs = roots.map(_ match {
        case g@Variable(_) if conjuncts.contains(g) => conjuncts(g)
        case e@_ => e 
      })    
    //replace every guard in the 'disjs' by its disjunct. DO this as long as every guard is replaced in every disjunct
    var unpackedDisjs = disjs
    var replacedGuard = true
    //var removeGuards = Seq[Variable]()
    while(replacedGuard) {      
      replacedGuard = false
      
      val newDisjs = unpackedDisjs.map(entry => {
        val (g,d) = entry
        val guards = variablesOf(d).collect{ case id@_ if disjuncts.contains(id.toVariable) => id.toVariable }
        if (guards.isEmpty) entry
        else {
          /*println("Disunct: "+d)
          println("guard replaced: "+guards)*/
          replacedGuard = true
          //removeGuards ++= guards
          (g, replace(unpackedDisjs, d))
        }           
      })
      unpackedDisjs = newDisjs
    }   
    //replace all the 'guards' in root using 'unpackedDisjs'
    replace(unpackedDisjs, And(rootexprs))
  }
  
  override def toString : String = {
    val disjStrs = disjuncts.map((entry) => {
      val (g,ctrs) = entry
      simplifyArithmetic(combiningOp(g, And(ctrs.map(_.toExpr)))).toString
    }).toSeq
    val conjStrs = conjuncts.map((entry) => combiningOp(entry._1, entry._2).toString).toSeq
    val rootStrs = roots.map(_.toString)
    (disjStrs ++ conjStrs ++ rootStrs).foldLeft("")((acc,str) => acc + "\n" + str)
  } 
}