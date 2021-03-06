package leon
package invariant.factories

import z3.scala._
import purescala.Common._
import purescala.Definitions._
import purescala.Trees._
import purescala.TreeOps._
import purescala.Extractors._
import purescala.TypeTrees._
import java.io._
import scala.collection.mutable.{ Map => MutableMap }
import invariant._
import scala.collection.mutable.{Map => MutableMap}

import invariant.engine._
import invariant.util._
import invariant.structure._

/*object UserTemplates {
  private var userTemplates = Map[FunDef, Expr]()
  
  def setTemplate(fd:FunDef, tempExpr :Expr) = {
    userTemplates += (fd -> tempExpr) 
  }
  
  def templates = userTemplates  
}*/

object TemplateIdFactory {
  //a set of template ids
  private var ids = Set[Identifier]()
  
  def getTemplateIds : Set[Identifier] = ids 
  
  def freshIdentifier(name : String = "", idType: TypeTree = RealType) : Identifier = {
    val idname = if(name.isEmpty()) "a?"
    			 else name + "?"
    val freshid = FreshIdentifier(idname, true).setType(idType)
    ids += freshid
    freshid
  }
  
  def copyIdentifier(id: Identifier) : Identifier = {   
    val freshid = FreshIdentifier(id.name, false).setType(RealType)
    ids += freshid
    freshid
  }
  
   /**
   * Template variables have real type
   */
  def IsTemplateIdentifier(id : Identifier) : Boolean = {
    ids.contains(id)
  }
  
  def IsTemplateVar(v : Variable) : Boolean = {
    IsTemplateIdentifier(v.id)
  }
   
  def freshTemplateVar(name : String= "") : Variable = {
    Variable(freshIdentifier(name))
  }
}

trait TemplateGenerator {
  def getNextTemplate(fd : FunDef): Expr
}

/**
 * Templates are expressions with template variables.
 * The program variables that can be free in the templates are only the arguments and
 * the result variable
 */
class TemplateFactory(tempGen : Option[TemplateGenerator], reporter : Reporter) {

  //a mapping from function definition to the template
  private var templateMap = {
    
    //initialize the template map with predefined user maps
    var muMap = MutableMap[FunDef, Expr]()
    muMap ++= FunctionInfoFactory.templateMap
    muMap
  }
  
  def setTemplate(fd:FunDef, tempExpr :Expr) = {
    templateMap += (fd -> tempExpr) 
  }

  /**    
   * This is the default template generator.
   *  
   */  
  def getDefaultTemplate(fd : FunDef): Expr = {

    //just consider all the arguments, return values that are integers
    val baseTerms = fd.params.filter((vardecl) => Util.isNumericType(vardecl.tpe)).map(_.toVariable) ++ 
    					(if(Util.isNumericType(fd.returnType)) Seq(Util.getFunctionReturnVariable(fd)) 
    					 else Seq())        
    					
    val lhs = baseTerms.foldLeft(TemplateIdFactory.freshTemplateVar() : Expr)((acc, t)=> {       
       Plus(Times(TemplateIdFactory.freshTemplateVar(),t),acc)
    })
    val tempExpr = LessEquals(lhs,IntLiteral(0))
    tempExpr
  }
  
  
  /**
   * Constructs a template using a mapping from the formals to actuals.
   * Uses default template if a template does not exist for the function and no template generator is provided.
   * Otherwise, use the provided template generator
   */
  var refinementSet = Set[FunDef]()
  def constructTemplate(argmap: Map[Expr,Expr], fd: FunDef): Expr = {
    
    //initialize the template for the function
    if (!templateMap.contains(fd)) {      
      if(!tempGen.isDefined) templateMap += (fd -> getDefaultTemplate(fd))
      else {
    	templateMap += (fd -> tempGen.get.getNextTemplate(fd))
    	refinementSet += fd
    	//for information 
      	reporter.info("- Template generated for function "+fd.id+" : "+templateMap(fd))      	
      }
    }        
    replace(argmap,templateMap(fd))
  }
    
  
  /**
   * Refines the templates of the functions that were assigned templates using the template generator.    
   */
  def refineTemplates(): Boolean = {
    
    if(tempGen.isDefined) {
      var modifiedTemplate = false
      refinementSet.foreach((fd) => {
        val oldTemp = templateMap(fd)
        val newTemp = tempGen.get.getNextTemplate(fd)

        if (oldTemp != newTemp) {
          modifiedTemplate = true
          templateMap.update(fd, newTemp)
          reporter.info("- New template for function " + fd.id + " : " + newTemp)
        }
      })
      modifiedTemplate
    } else false    
  }
  
  def getTemplate(fd : FunDef) : Option[Expr] = {
    templateMap.get(fd)
  }   

  def getFunctionsWithTemplate : Seq[FunDef] = templateMap.keys.toSeq

  /**
   * For creating synthesis benchmarks
   */
  /*private var synthFuns = Map[FunDef,(FunDef,Expr)]() //a mapping from functions to their synthesis functions and templates 
  def constructSynthFunction(argmap: Map[Expr,Expr], fd: FunDef) : FunctionInvocation = {
    val (synfun, _) = synthFuns.getOrElse(fd, {
    	val temp = templateMap.getOrElse(fd, { getDefaultTemplate(fd) })
    	val freevars = variablesOf(temp).filterNot(TemplateIdFactory.IsTemplateIdentifier _).toList
    	val synfun = new FunDef(FreshIdentifier(fd.id.name + "-template"), List(), BooleanType, 
    	    freevars.map(id => ValDef(id, id.getType)))
    	synthFuns += (fd -> (synfun, temp))
    	(synfun, temp)
    })
    //map the arguments of synthFuns using the given mapping and construct a function invocation
    FunctionInvocation(TypedFunDef(synfun, List()), synfun.params.map(p => argmap(p.id.toVariable)))    
  }
  
  def getSynthFunctions(fd: FunDef) : Option[(FunDef, Expr)] = synthFuns.get(fd)*/
}