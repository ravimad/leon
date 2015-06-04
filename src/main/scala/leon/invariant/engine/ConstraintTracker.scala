package leon
package invariant.engine

import z3.scala._
import purescala._
import purescala.Common._
import purescala.Definitions._
import purescala.Trees._
import purescala.TreeOps._
import purescala.Extractors._
import purescala.TypeTrees._
import evaluators._
import java.io._

import invariant.factories._
import invariant.util._
import invariant.structure._
 
class ConstraintTracker(ctx : InferenceContext, rootFun : FunDef, temFactory: TemplateFactory) {
    
  //a mapping from functions to its VCs represented as a CNF formula
  protected var funcVCs = Map[FunDef,Formula]()
  
  val vcRefiner = new RefinementEngine(ctx, this, temFactory)
  val specInstantiator = getSpecInstantiator
  
  def getSpecInstantiator = {
    new SpecInstantiator(ctx, this, temFactory)
  }
  
  def getFuncs : Seq[FunDef] = funcVCs.keys.toSeq
  def hasVC(fdef: FunDef) = funcVCs.contains(fdef)  
  def getVC(fd: FunDef) : Formula = funcVCs(fd)
  
  def addVC(fd: FunDef, vc: Expr) = {       
    funcVCs += (fd -> new Formula(fd, vc, ctx))     
  }
  
  def initialize = {    
    //assume specifications 
    specInstantiator.instantiate    
  }

  def refineVCs(toUnrollCalls: Option[Set[Call]]) : Set[Call]  = {
    val unrolledCalls = vcRefiner.refineAbstraction(toUnrollCalls)        
    specInstantiator.instantiate   
    unrolledCalls
  }  
}

/**
 * The class is exclusively for generation Sygus constraints
 */
/*class SygusContraintTracker(ctx : InferenceContext, 
    rootFun : FunDef, 
    temFactory: TemplateFactory) // synthesis functions corresponding to the templates of every function
		extends ConstraintTracker(ctx, rootFun, temFactory) {  
  
  override def getSpecInstantiator = {
    new SpecInstantiator(ctx, this, temFactory) {
      override def templateForCall(call: Call): Expr = {
        val argmap = Util.formalToAcutal(call)
        val callee = call.fi.tfd.fd
        val synthfun = temFactory.constructSynthFunction(argmap, callee) //assuming that every function has a template
        val template = if (callee.hasPrecondition) {
          val freshPre = replace(Util.formalToAcutal(call), 
              freshenLocals(matchToIfThenElse(callee.precondition.get)))
          Implies(freshPre, synthfun)
        } else {
          synthfun
        }
        //flatten functions       
        ExpressionTransformer.normalizeExpr(template, ctx.multOp)
      }
    }
  }  
}*/