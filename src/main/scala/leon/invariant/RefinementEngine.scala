package leon
package invariant

import purescala.Common._
import purescala.Definitions._
import purescala.Trees._
import purescala.TreeOps._
import purescala.Extractors._
import purescala.TypeTrees._
import scala.collection.mutable.{ Set => MutableSet }
import scala.collection.mutable.{ Map => MutableMap }
import leon.evaluators._
import java.io._
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
import leon.invariant._

class CallData(val node : CtrNode, val cnt: Int) {
  val ctrnode = node
  val unrollCnt = cnt
}

//TODO: the parts of the code that collect the new head functions is ugly. Fix this.
class RefinementEngine(prog: Program, ctrTracker: ConstraintTracker, tempFactory : TemplateFactory, reporter : Reporter) {
  
  private val MAX_UNROLLS = 2 
  //a mapping from a call to the node containing the call (plus some additional data like the unroll depth etc.)    
  private var headCallPtrs : Map[Call, CallData] = _

  //a set of calls for which its templates have been assumed
  //TODO: Ideally this info should stored in a distributed way inside the nodes of the constraint tree
  private var templatedCalls = Set[Call]()

  /**
  * This creates an initial abstraction 
  **/
  def initialize() : Unit = {

    //this procedure has side-effects on 'templatedCalls'
    //we are consciously ignoring the return value of the procedure
    assumePostConditions()

    headCallPtrs = findAllHeads(ctrTracker)
    //for debugging
    //println("Head-Calls: "+headCallPtrs.keys.toSeq)
    //System.exit(0)
  }

  private def findAllHeads(ctrTracker: ConstraintTracker) : Map[Call,CallData] ={  
    var heads = Map[Call,CallData]()
    
    ctrTracker.getFuncs.foreach((fd) => {
      val (btree,ptree) = ctrTracker.getVC(fd)      
      heads ++= (findHeads(btree, MAX_UNROLLS) ++ findHeads(ptree, MAX_UNROLLS))
    })        
    heads
    
  }  
  
  /**
   * Heads are procedure calls whose target definitions have not been unrolled
   */
  private def findHeads(ctrTree: CtrTree, unrollCnt : Int) : Map[Call,CallData] ={      
    var heads = Map[Call,CallData]()

    if(unrollCnt > 0) {
      def visitor : (CtrNode => Unit) = 
        (node: CtrNode) => {
          val calls = node.uifs
          calls.foreach((call) => { heads += (call -> new CallData(node, unrollCnt)) })           
        }  
      TreeUtil.preorderVisit(ctrTree,visitor)      
    }
    heads
  }    

  /**
   * This procedure refines the existing abstraction.
   * Currently, the refinement happens by unrolling the head functions.   
   */
  def refineAbstraction(): Set[Call] = {
    var newheads = Map[Call,CallData]()
    //unroll each call in the head pointers    
    val unrolls = headCallPtrs.foldLeft(Set[Call]())((acc, entry) => {      
      val (call, calldata) = entry            

      //the following creates new constraint trees and hence has side effects
      if(calldata.unrollCnt > 0) {        
        newheads ++= unrollCall(call, calldata.ctrnode, calldata.unrollCnt - 1)
      }

      acc + call
    })
    
    //assume the post-conditions for the calls in the VCs of this function
    newheads ++= assumePostConditions()

    //update the head functions
    headCallPtrs = newheads 

    //For debugging: print the post and body root of all the functions
    /*ctrTracker.getFuncs.foreach((fd) => {
        val (btree,ptree) = ctrTracker.getVC(fd)
        println("Function: "+fd.id)
        println("\t Body Tree: "+btree.toString)
        println("\t Post Tree: "+ptree.toString)
      })*/

    unrolls
  }
  

  /**
   * Returns a set of unrolled calls and a set of new head functions   
   * here we unroll the methods in the current abstraction by one step.
   * This procedure has side-effects on 'headCallPtrs'
   */  
  //private var unrollCounts = MutableMap[Call,Int]()
  def unrollCall(call : Call, ctrnode : CtrNode, unrollCnt : Int): Map[Call,CallData] = {                

    //println("Processing: "+call)
    val fi = call.fi
    if (fi.funDef.hasBody) {

      //freshen the body and the post           
      val isRecursive = prog.isRecursive(fi.funDef)        
      if(isRecursive) {                        
        var newheads = Map[Call,CallData]()
        val recFun = fi.funDef
        if (!ctrTracker.hasCtrTree(recFun)) { //check if a constraint tree does not exist for the call's target

          println("Creating VC for "+fi.funDef.id)          
          /**
           * create a new verification condition for this recursive function
           */          
          val freshBody = matchToIfThenElse(freshenLocals(recFun.body.get))
          val resvar = if (recFun.hasPostcondition) {
            recFun.postcondition.get._1.toVariable
          } else {
            //create a new resvar
            Variable(FreshIdentifier("res", true).setType(recFun.returnType))
          }          
          val plainBody = Equals(resvar,freshBody)
          val bodyExpr = ExpressionTransformer.normalizeExpr(if (recFun.hasPrecondition) {
            And(matchToIfThenElse(recFun.precondition.get), plainBody)
          } else plainBody)
          
          //add body constraints
          ctrTracker.addBodyConstraints(recFun, bodyExpr)

          //add (negated) post condition template for the function                              
          val argmap = InvariantUtil.formalToAcutal(Call(resvar, FunctionInvocation(recFun, recFun.args.map(_.toVariable))))

          val postTemp = tempFactory.constructTemplate(argmap, recFun)
          val npostTemp = ExpressionTransformer.normalizeExpr(Not(postTemp))
          //print the negated post
          //println("Negated Post: "+npostTemp)
          ctrTracker.addPostConstraints(recFun,npostTemp)

          //Find new heads
          val (btree,ptree) = ctrTracker.getVC(recFun)
          newheads ++= (findHeads(btree, MAX_UNROLLS) ++ findHeads(ptree, MAX_UNROLLS))          
        }
        //unroll the body some fixed number of times                       
        /*val ucount = if(unrollCounts.contains(call)) {
                          val count = unrollCounts(call)
                          unrollCounts.update(call,count + 1)
                          count
                      } else {
                        unrollCounts += (call -> 0)
                        0
                      }*/
        //TODO: unroll always ??                   
        println("Unrolling " + Equals(call.retexpr,call.fi))
        newheads ++= inilineCall(call, ctrnode, unrollCnt)          
        newheads
      }
      else {        
        //here we are unrolling a non-recursive function, so set the unrollCnt to maximum
        println("Inlining "+fi.funDef.id)               
        inilineCall(call, ctrnode, MAX_UNROLLS)        
      }                
    } else Map()    
  }

  /**
  * The parameter 'resVar' is the result variable of the body
  **/
  def inilineCall(call : Call, ctrnode: CtrNode, unrollCnt : Int) : Map[Call,CallData] = {    
    //here inline the body && Post and add it to the tree of the rec caller
    val callee = call.fi.funDef
    val post = callee.postcondition
    
    //Important: make sure we use a fresh body expression here
    val freshBody = matchToIfThenElse(freshenLocals(callee.body.get))    
    val calleeSummary = if (post.isDefined) {
      val (resvar, poste) = post.get
      val freshPost = matchToIfThenElse(freshenLocals(poste))
      val bodyRel = Equals(resvar.toVariable, freshBody)
      And(bodyRel, freshPost)
   } else {
      Equals(ResultVariable().setType(callee.returnType), freshBody)
    }

    val argmap1 = InvariantUtil.formalToAcutal(call)
    val inlinedSummary = ExpressionTransformer.normalizeExpr(replace(argmap1, calleeSummary))
          
    //println("calleeSummary: "+inlinedSummary)        
    //create a constraint tree for the summary
    val summaryTree = CtrNode()      
    ctrTracker.addConstraintRecur(inlinedSummary, summaryTree)          

    //Find new heads (note: should not insert summaryTree and then call findheads)
    val newheads = findHeads(summaryTree, unrollCnt)

    //insert the tree
    TreeUtil.insertTree(ctrnode, summaryTree)        

    newheads
  }

 /*
  * This function refines the constraint tree by assuming the post conditions/templates for calls in
  * the body and post tree.
  */
  def assumePostConditions() : Map[Call,CallData] = {
    
    ctrTracker.getFuncs.foldLeft(Map[Call,CallData]())((acc, fd) => {

      val (btree,ptree) = ctrTracker.getVC(fd)      
      val hds = assumePostConditionsForTree(btree, ptree, fd)            
      (acc ++ hds)
    })
  }
  
  def assumePostConditionsForTree(bodyRoot: CtrNode, postRoot : CtrNode, fd: FunDef) : Map[Call,CallData] = {
    
    /**
     * A helper function that creates templates for a call
     */
    var templateMap = Map[Call, Expr]()
    def templateForCall(call: Call): Expr = {

      templateMap.getOrElse(call, {
        val argmap = InvariantUtil.formalToAcutal(call)
        val tempExpr = tempFactory.constructTemplate(argmap, call.fi.funDef)
        templateMap += (call -> tempExpr)
        tempExpr
      })
    }

    var visited = Set[CtrNode]()
    var newheads = Map[Call,CallData]()

    //this does a post order traversal
    def assumeTemplates(root: CtrTree) : Unit = root match {

      case n @ CtrNode(_) => {

        if(!visited.contains(n)){
          visited += n

          //first recurse into the children  
          n.Children.foreach(assumeTemplates(_))

          //For debugging      
          /*if (fd.id.name.equals("size")) {
            println("Node UIFs: " + n.uifs)            
          }*/

          //now process this node
          val newCalls = n.uifs.toSeq.filter((call) => {                       
            val accept = !templatedCalls.contains(call) && ctrTracker.hasCtrTree(call.fi.funDef)
            /*if(call.fi.funDef.id.name == "size" && call.retexpr.asInstanceOf[Variable].id.name == "r14"){
              println("******** Filter value for: "+call.expr+" is "+accept)
            }*/
            accept
          })          
          
          //println("New calls: "+processedCalls)
          //update processed calls
          templatedCalls ++= newCalls 

          val templates = newCalls.map(templateForCall(_))

          if (!templates.isEmpty) {
          
            val ctr = And(templates)          
            //flatten functions
            val flatExpr = ExpressionTransformer.normalizeExpr(ctr)
            //create the root of a new  tree          
            val templateTree = CtrNode()          
            ctrTracker.addConstraintRecur(flatExpr, templateTree)

            //find new heads
            newheads ++= findHeads(templateTree, MAX_UNROLLS)

            //insert the templateTree after this node
            TreeUtil.insertTree(n,templateTree)                        
          } 
        }                
      }      
      case CtrLeaf() => ;
    }

    /*if (fd.id.name.equals("size")) {
      println("Templated calls: "+templatedCalls)
    }*/
    assumeTemplates(bodyRoot) 
    assumeTemplates(postRoot)

    newheads
  }
}
