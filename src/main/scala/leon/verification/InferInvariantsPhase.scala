package leon
package verification

import z3.scala._
import purescala.Common._
import purescala.Definitions._
import purescala.Trees._
import purescala.TreeOps._
import purescala.Extractors._
import purescala.TypeTrees._
import solvers.{ Solver, TrivialSolver, TimeoutSolver }
import solvers.z3.FairZ3Solver
import scala.collection.mutable.{ Set => MutableSet }
import leon.evaluators._
import java.io._
import scala.tools.nsc.io.File
import leon.solvers.z3.UninterpretedZ3Solver

/**
 * @author ravi
 * This phase performs automatic invariant inference. 
 */
object InferInvariantsPhase extends LeonPhase[Program, VerificationReport] {
  val name = "InferInv"
  val description = "Invariant Inference"
  private var reporter: Reporter = _
  private var context : LeonContext = _
  private var program : Program = _
  private var uiSolver : UninterpretedZ3Solver = _

  override val definedOptions: Set[LeonOptionDef] = Set(
    LeonValueOptionDef("functions", "--functions=f1:f2", "Limit verification to f1,f2,..."),
    LeonValueOptionDef("timeout", "--timeout=T", "Timeout after T seconds when trying to prove a verification condition."))

  
  //this is a template for linear constraints
  //A linear constraint is something a expression of the form a1*v1 + a2*v2 + .. + an*vn + b <= 0 or = 0
  case class LinearTemplate(val template: Expr, val coeffTemplate: Map[Expr, Expr with Terminal], val constTemplate: Option[Expr with Terminal])
  {
    override def toString() : String = {      
      val constStr = coeffTemplate.foldLeft("")((str,pair) => { 
        val (e,i) = pair
        val termStr = if(i != IntLiteral(1)) i + "*" + e.toString 
        			  else e.toString 
        (str + termStr + " + ")        
        }) + 
        (if(constTemplate.isDefined) constTemplate.get.toString
        else "0") +
        (template match {
          case t : Equals => " = "
          case t : LessThan => " < "
          case t: GreaterThan => " > "
          case t: LessEquals => " <= "
          case t: GreaterEquals => " >= "          
        }) + "0"
        
       constStr //+ " ActualExpr: "+ expr
    }
  }
  
  //each constraint is a mapping from constraint variable to its coefficient (integers)
  //a constraint variable can be leon variables or function invocations or case classes etc.
  case class LinearConstraint(val expr: Expr, val coeffMap: Map[Expr,IntLiteral], val constant: Option[IntLiteral]) extends
  	LinearTemplate(expr,coeffMap,constant)
  {
  }
  
  //this is in reality a DAG
  //TODO: Fix this entire portion of code that manipulates the tree
  abstract class CtrTree
  case class CtrNode(val blockingId: Identifier) extends CtrTree {

    var constraints = Set[LinearConstraint]()    
    private var children = Set[CtrTree](CtrLeaf())
    
    def Children : Set[CtrTree] = children  
    
    def copyChildren(newnode : CtrNode) = {
      newnode.children = this.children      
    }
    
    def removeAllChildren() = {
      this.children = Set(CtrLeaf())
    }
    
    def addChildren(child : CtrNode) = {
      if(children.size == 1 && children.first == CtrLeaf())
        children = Set[CtrTree](child)
      else 
        children += child
    }
    
    override def toString(): String = {
      var str = "Id: " + blockingId + " Constriants: " + constraints + " children: \n"
      children.foldLeft(str)((g: String, node: CtrTree) => { g + node.toString })
    }
  }
  case class CtrLeaf() extends CtrTree
  //this is a mutable map (a little nasty)
  private var treeNodeMap = collection.mutable.Map[Identifier, CtrNode]()

  //root of the tree. This would be set while constraints are added
  var bodyRoot: CtrTree = CtrLeaf()
  var postRoot: CtrTree = CtrLeaf()

  def isBlockingId(id: Identifier): Boolean = {
    if (id.name.startsWith("b")) true else false
  }

  def isStartId(id: Identifier): Boolean = {
    if (id.name.contains("start")) true else false
  }

  //TODO: cleanup this code and do not use flags
  def addConstraint(e: Expr, isBody: Boolean) = {
    val (id, innerExpr) = parseGuardedExpr(e)

    //get the node corresponding to the id
    val ctrnode = treeNodeMap.getOrElse(id, {
      val node = CtrNode(id)
      treeNodeMap += (id -> node)

      //set the root of the tree (this code is ugly and does string matching)
      //TODO: fix this
      if (isStartId(id)) {
        val root = if (isBody) bodyRoot else postRoot
        if (root.isInstanceOf[CtrNode])
          throw IllegalStateException("Different start symbol: " + id)
        else {
          if (isBody) bodyRoot = node else postRoot = node
        }
      }

      node
    })

    //returns the children nodes classified into real and dummy children. The first set is the real set and the second is the dummy set
    def addCtrOrBlkLiteral(ie: Expr, newChild: Boolean): (Set[CtrNode],Set[CtrNode]) = {
      ie match {
        case Or(subexprs) => {
          val validSubExprs = subexprs.collect((sube) => sube match {
            case _ if (sube match {
              //cases to be ignored              
              case Not(Variable(childId)) => false //need not take this into consideration now
              case _ => true
            }) => sube
          })
          if (!validSubExprs.isEmpty) {           
            val createChild = if (validSubExprs.size > 1) true else false            
            validSubExprs.foldLeft((Set[CtrNode](),Set[CtrNode]()))((acc,sube) => {
              val (real,dummy) = acc
              val (real2,dummy2) = addCtrOrBlkLiteral(sube, createChild)
              (real ++ real2, dummy ++ dummy2)             
            })
          }
          else (Set(),Set())
        }
        case And(subexprs) => {          
          subexprs.foldLeft((Set[CtrNode](),Set[CtrNode]()))((acc,sube) => {
              val (real,dummy) = acc
              val (real2,dummy2) = addCtrOrBlkLiteral(sube, false)
              (real ++ real2, dummy ++ dummy2)             
            })          
        }           
        
        case Variable(childId) => {
          //checking for blocking literal
          if (isBlockingId(childId)) {
            (Set(createOrLookupId(ctrnode, childId)),Set())
          }
          else
            throw IllegalStateException("Encountered a variable that is not a blocking id: " + childId)
        }
        case Iff(lhs, rhs) => {
          //lhs corresponds to a blocking id in this case
          lhs match {
            case Variable(childId) if (isBlockingId(childId)) => {
              val childNode = createOrLookupId(ctrnode, childId)
              val ctr = exprToConstraint(rhs)
              childNode.constraints += ctr    
              (Set(childNode),Set())
            }
            case _ => throw IllegalStateException("Iff block --- encountered something that is not a blocking id: " + lhs)
          }

        }
        case _ => {          
          val node = if (newChild) createOrLookupId(ctrnode, FreshIdentifier("dummy", true))
        		  	 else ctrnode
          val ctr = exprToConstraint(ie)
          node.constraints += ctr       
          if(newChild) (Set(),Set(node)) else (Set(),Set())             
        }
      }
    }
    //important: calling makelinear may result in disjunctions and also potentially conjunctions
    val flatExpr = FlattenFunction(innerExpr)
    val nnfExpr = TransformNot(flatExpr)    
    val (realChildren,dummyChildren) = addCtrOrBlkLiteral(nnfExpr, false)
    
    //now merge dummy children with the ctrnode itself.
    //the following code is slightly nasty and with side effects
    val parentNode = if(!dummyChildren.isEmpty) {
      val newnode = CtrNode(ctrnode.blockingId)
      ctrnode.copyChildren(newnode)
      ctrnode.removeAllChildren()
      dummyChildren.foreach((child) => {child.addChildren(newnode); ctrnode.addChildren(child)})
      treeNodeMap.update(ctrnode.blockingId, newnode)
      newnode
    }  else ctrnode
    
    realChildren.foreach(parentNode.addChildren(_))    
  }

  def createOrLookupId(parentNode: CtrNode, childId: Identifier): CtrNode = {
    var childNode = treeNodeMap.getOrElse(childId, {
      val node = CtrNode(childId)
      treeNodeMap += (childId -> node)
      node
    })    
    childNode
  }  
      
  def parseGuardedExpr(e: Expr): (Identifier, Expr) = {
    e match {
      case Or(Not(Variable(id)) :: tail) => {
        tail match {
          case expr :: Nil => (id, expr)
          case expr :: tail2 => {
            //this corresponds to a disjunction
            (id, Or(tail))
          }
          case _ => throw IllegalStateException("Not in expected format: " + tail)
        }
      }
      case _ => throw IllegalStateException("Not a guarded expression: " + e)
    }
  }

  
  //the expr is required to be linear, if not, an exception would be thrown
  //for now some of the constructs are not handled
  def exprToConstraint(expr: Expr): LinearConstraint = {
    var coeffMap = Map[Expr, IntLiteral]()
    var constant: Option[IntLiteral] = None

    val oneLit = IntLiteral(1)
    def genConstraint(e: Expr): Option[Expr] = {
      e match {
        case IntLiteral(v) => {
              constant = Some(IntLiteral(v))
              None
        }
        case Plus(e1, e2) => {
          if (e1.isInstanceOf[IntLiteral] && e2.isInstanceOf[IntLiteral])
            throw IllegalStateException("sum of two constants, not in canonical form: " + e)

          val r1 = genConstraint(e1)
          if (r1.isDefined) {
            //here the coefficient is 1
            coeffMap += (r1.get -> oneLit)
          }
          val r2 = genConstraint(e2)
          if (r2.isDefined)
            coeffMap += (r2.get -> oneLit)

          None
        }
        case Times(e1, e2) => {
          if (e1.isInstanceOf[IntLiteral] && e2.isInstanceOf[IntLiteral])
            throw IllegalStateException("product of two constants, not in canonical form: " + e)

          /*else if (!e1.isInstanceOf[IntLiteral] && !e2.isInstanceOf[IntLiteral])
            throw IllegalStateException("nonlinear expression: " + e)*/
          /*else {
            val (coeff, cvar) = e1 match {
              case IntLiteral(v) => (v, e2)
              case _ => {
                val IntLiteral(v) = e2
                (v, e1)
              }
            }*/
          val IntLiteral(v) = e1
          val (coeff, cvar) = (v, e2)

          val r = genConstraint(cvar)
          if (!r.isDefined)
            throw IllegalStateException("Multiplicand not a constraint variable: " + cvar)
          else {
            //add to mapping
            coeffMap += (r.get -> IntLiteral(coeff))
          }
          None          
        }
        case Variable(id) => Some(e)
        case FunctionInvocation(fdef, args) => Some(e)
        case BinaryOperator(e1, e2, op) => {

          /*if (!e.isInstanceOf[Equals] && !e.isInstanceOf[LessThan] && !e.isInstanceOf[LessEquals]
            && !e.isInstanceOf[GreaterThan] && !e.isInstanceOf[GreaterEquals])
            throw IllegalStateException("Relation is not linear: " + e)
          else {*/
          if (e1.isInstanceOf[IntLiteral] && e2.isInstanceOf[IntLiteral])
            throw IllegalStateException("relation on two integers, not in canonical form: " + e)

          e2 match {
            case IntLiteral(0) => {
              val r = genConstraint(e1)
              if (r.isDefined) {
                //here the coefficient is 1
                coeffMap += (r.get -> oneLit)
              }
              None
            }
            case _ => throw IllegalStateException("Not in canonical form: " + e)
          }
        }
        case _ => {
          throw IllegalStateException("Ecountered unhandled term in the expression: " + e)
        }
      } //end of match e
    } //end of genConstraint      
    
    val linearExpr = MakeLinear(expr)
    if (!genConstraint(linearExpr).isDefined) {
      LinearConstraint(linearExpr, coeffMap, constant)
    } else
      throw IllegalStateException("Expression not a linear relation: " + expr)
  }

  
  /**
   * This method may have to do all sorts of transformation to make the expressions linear constraints.   
   * This assumes that the input expression is an atomic predicate (i.e, without and, or and nots)
   * This is subjected to constant modification.
   */
  def MakeLinear(atom: Expr): Expr = {
    
    //pushes the minus inside the arithmetic terms
    def PushMinus(inExpr : Expr) : Expr = {
      require(inExpr.getType == Int32Type)
      
      inExpr match {
        case IntLiteral(v) => IntLiteral(-v)
        case t : Terminal => Times(IntLiteral(-1),t)
        case fi@FunctionInvocation(fdef,args) => Times(IntLiteral(-1),fi)        
        case UMinus(e1) => e1
        case Minus(e1,e2) => Plus(PushMinus(e1),e2)
        case Plus(e1,e2) => Plus(PushMinus(e1),PushMinus(e2))
        case Times(e1,e2) => {
          //here push the minus in to the coefficient if possible
			e1 match {
              case IntLiteral(v) => Times(PushMinus(e1),e2)
              case _ => Times(e1,PushMinus(e2))
            }          
        }                
        case _ => throw NotImplementedException("PushMinus -- Operators not yet handled: "+inExpr)         
      }
    }
    
    //we assume that PushMinus has already been invoke on the expression
    def PushTimes(c : Int, ine : Expr) : Expr = {
      require(ine.getType == Int32Type)
      
      ine match {
        case IntLiteral(v) => IntLiteral(c * v)
        case t : Terminal => Times(IntLiteral(c),t)
        case fi@FunctionInvocation(fdef,args) => Times(IntLiteral(c),fi)                       
        case Plus(e1,e2) => Plus(PushTimes(c,e1),PushTimes(c,e2))
        case Times(e1,e2) => {
          //here push the times into the coefficient (which should be the first expression)
        	Times(PushTimes(c,e1),e2)
        }                
        case _ => throw NotImplementedException("PushTimes -- Operators not yet handled: "+ine)         
      }
    }

    //collect all the constants and simplify them
    //we assume that PushTimes and PushMinus have been invoked on the expression
    def simplifyConsts(ine: Expr): (Option[Expr], Int) = {
      require(ine.getType == Int32Type)

      ine match {         
        case IntLiteral(v) => (None,v)
        case t: Terminal => (Some(t),0)
        case fi: FunctionInvocation => (Some(fi), 0)       
        case Plus(e1, e2) => {
          val (r1, c1) = simplifyConsts(e1)
          val (r2, c2) = simplifyConsts(e2)

          val newe = (r1, r2) match {
            case (None, None) => None
            case (Some(t), None) => Some(t)
            case (None, Some(t)) => Some(t)
            case (Some(t1), Some(t2)) => Some(Plus(t1, t2))
          }
          (newe, c1 + c2)
        }
        case Times(e1, e2) => {
          //because of the pushTimes assumption, we can simplify this case
          (Some(ine),0)
        }
        case _ => throw NotImplementedException("collectConstants -- Operators not yet handled: "+ine)
      }
    }
   	    
    def mkLinearRecur(inExpr: Expr): Expr = {
      inExpr match {        
        case e @ BinaryOperator(e1, e2, op) if (e1.getType == Int32Type &&
            (e.isInstanceOf[Equals] || e.isInstanceOf[LessThan] 
        	|| e.isInstanceOf[LessEquals]|| e.isInstanceOf[GreaterThan] 
        	|| e.isInstanceOf[GreaterEquals])) => {

          e2 match {
            case IntLiteral(0) => e
            case _ => {
               val (newe,newop) = e match {
                 case t : Equals => (Minus(e1, e2),op)
                 case t : LessEquals => (Minus(e1, e2),LessEquals)
                 case t: LessThan => (Plus(Minus(e1,e2),IntLiteral(1)), LessEquals)
                 case t: GreaterEquals => (Minus(e2,e1), LessEquals)
                 case t : GreaterThan => (Plus(Minus(e2,e1),IntLiteral(1)), LessEquals) 
              }
              val r = mkLinearRecur(newe)
              //simplify the resulting constants
              val (Some(r2),const) = simplifyConsts(r)
              val finale = if(const != 0) Plus(r2,IntLiteral(const)) else r2
              //println(r + " simplifies to "+finale)
              newop(finale, IntLiteral(0))
            }
          }
        }
        case Minus(e1,e2) => Plus(mkLinearRecur(e1),PushMinus(mkLinearRecur(e2)))
        case UMinus(e1) => PushMinus(mkLinearRecur(e1))
        case Times(e1,e2) => {
          val (r1,r2) = (mkLinearRecur(e1),mkLinearRecur(e2))
          (r1,r2) match {
            case (IntLiteral(v),_) => PushTimes(v,r2)
            case (_,IntLiteral(v)) => PushTimes(v,r1)
            case _ => throw IllegalStateException("Expression not linear: "+Times(r1,r2))
          }         
        }        
        case Plus(e1,e2) => Plus(mkLinearRecur(e1),mkLinearRecur(e2))
        case t : Terminal => t
        case fi : FunctionInvocation => fi        
        /*case UnaryOperator(e,op) => op(mkLinearRecur(e))
        case BinaryOperator(e1,e2,op) => op(mkLinearRecur(e1),mkLinearRecur(e2))
        case NAryOperator(args,op) => op(args.map(mkLinearRecur(_)))*/
        case _ => throw IllegalStateException("Expression not linear: "+inExpr)
      }
    }	 
   	val rese = mkLinearRecur(atom)
   	//println("Unnormalized Linearized expression: "+unnormLinear)
   	rese
  } 
  
  //TODO: change instanceOf to match
  //It also necessary to convert the formula to negated normal form by pushing all not's inside
  def TransformNot(expr: Expr): Expr = {
    def nnf(inExpr: Expr): Expr = {
      inExpr match {
        //matches integer binary relation
        case Not(e @ BinaryOperator(e1, e2, op)) if (e1.getType == Int32Type) => {
          e match {
            case e: Equals => Or(nnf(LessEquals(e1, Minus(e2, IntLiteral(1)))), nnf(GreaterEquals(e1, Plus(e2, IntLiteral(1)))))
            case e: LessThan => GreaterEquals(nnf(e1), nnf(e2))
            case e: LessEquals => GreaterThan(nnf(e1), nnf(e2))
            case e: GreaterThan => LessEquals(nnf(e1), nnf(e2))
            case e: GreaterEquals => LessThan(nnf(e1), nnf(e2))
            case _ => throw IllegalStateException("Unknown integer predicate: " + e)
          }
        }                
        //TODO: "Not" is not recognized as an unary operator, need to find out why
        case e@Not(t: Terminal) => e
        case Not(And(args)) => Or(args.map(arg => nnf(Not(arg))))
        case Not(Or(args)) => And(args.map(arg => nnf(Not(arg))))
        case Not(Not(e1)) => nnf(e1)
        case Not(Implies(e1, e2)) => And(nnf(e1), nnf(Not(e2)))
        case Not(Iff(e1,e2)) => Or(nnf(Implies(e1,e2)),nnf(Implies(e2,e1)))        
        
        case t : Terminal => t
        case u@UnaryOperator(e1,op) => op(nnf(e1))
        case b@BinaryOperator(e1,e2,op) => op(nnf(e1),nnf(e2))
        case n@NAryOperator(args,op) => op(args.map(nnf(_))) 
        
        case _ => throw IllegalStateException("Impossible event: expr did not match any case: "+inExpr)        
      }
    }
    nnf(expr)
  }
  
  def FlattenFunction(inExpr : Expr) : Expr = {
    
    var conjuncts = Set[Expr]()    
    def flattenFunc(e : Expr) : Expr = {
      e match {
        case fi@FunctionInvocation(fd,args) => {
          //flatten the  arguments
          val newargs = args.foldLeft(List[Expr with Terminal]())((acc,arg) => 
            arg match {
              case t: Terminal => (acc :+ t)
              case arge@_ => {
                val freshvar = Variable(FreshIdentifier("arg",true).setType(arge.getType))
                conjuncts += Equals(freshvar,arge) 
                (acc :+ freshvar)
              } 
            }
          )
          FunctionInvocation(fd,newargs)
        }
        case _ => e
      }
    }
    var newExpr = simplePostTransform(flattenFunc)(inExpr)
    And((conjuncts + newExpr).toSeq)
  } 
  
  private var paramCoeff = Map[FunDef,List[Variable]]()
  
  def getInvariantTemplate() : (FunctionInvocation => Set[LinearTemplate]) ={
        
    /**
     * The ordering of the expessions in the List[Expr] is very important.
     * TODO: in the future use more sophisticated ways of constructing terms
     */  
    def getWellTypedTerms(args : Seq[Expr], fd : FunDef) : List[Expr] = {
      
      //just consider all the arguments and res (which is the function itself) that are
      //integer valued as only possible terms
     val terms= (args :+ FunctionInvocation(fd,args)).collect((e : Expr) => e match { case _ if(e.getType == Int32Type) => e })
     terms.toList    	
    }
    
    (fi: FunctionInvocation) =>  {
      val fd = fi.funDef            
      
      if(!paramCoeff.contains(fd))
      {
        //bind function to (unknown) coefficients
        val paramTerms = getWellTypedTerms(fd.args.map(_.toVariable), fd)
        val newCoeffs = List.range(0, paramTerms.size+1).map((i)=> Variable(FreshIdentifier("a"+i+"a",true).setType(Int32Type)))        
        paramCoeff += (fd -> newCoeffs)
      }
      
      //get the arguments of the function invocation
      val args = fi.args
      //here check if the args are all Terminals
      args.foreach((arg) => if(!arg.isInstanceOf[Terminal]) throw IllegalStateException("Argument is not a variable"))
      
      //get all teh terms constructible using args
      val argTerms = getWellTypedTerms(args,fd)
            
      //parse the existing coefficient map
      val (constPart :: coeffsPart) = paramCoeff(fd) 
      val coeffmap = argTerms.zip(coeffsPart)
      
      //create a linear expression
      val linearExpr = LessEquals(coeffmap.foldLeft(constPart : Expr)((acc,param)=>{
        val (term,coeff) = param
        Plus(acc,Times(coeff,term))
      }),IntLiteral(0))
      
      Set(LinearTemplate(linearExpr, coeffmap.toMap, Some(constPart)))
    }
  }
    
  //some utility methods
  def getFIs(ctr: LinearConstraint) : Set[FunctionInvocation] = {
    val fis = ctr.coeffMap.keys.collect((e) => e match {
        case fi : FunctionInvocation => fi        
    })
    fis.toSet
  }
  
  /**
   * This function computes invariants belonging to the template.
   * The result is a mapping from function definitions to the corresponding invariants.
   * Note that the invariants are context specific and may not be context independent invariants for the functions (except for startFun)   
   */  
  def SolveForTemplates(inFun : FunDef, bodyTree : CtrTree, postTree: CtrTree, invTemplate: FunctionInvocation => Set[LinearTemplate]) 
  	: Option[Map[FunDef,Expr]] = {
    //this is a mapping from node ids of the trees to the templates induced by the constraints of the node  
    //val templateMap = Map[Identifier,Set[LinearConstraint]]()
    //get the template for the inFun 
    val inTemplates = invTemplate(FunctionInvocation(inFun,inFun.args.map(_.toVariable)))
    
    //first traverse the body and collect all the antecedents
    var antSet = List[(List[LinearConstraint],List[LinearTemplate])]()        
    
    def traverseBodyTree(tree: CtrTree, currentCtrs : List[LinearConstraint], currentTemps : List[LinearTemplate]): Unit = {
      tree match{
        case n@CtrNode(blkid) => {
          val ctrs = n.constraints          
          val newCtrs = currentCtrs ++ ctrs 
          //find function invocations in ctrs
          val fis = ctrs.foldLeft(Set[FunctionInvocation]())((set,ctr) =>  set ++ getFIs(ctr))
          //generate a template for each function invocation and add it to the antecedents or consequent.
          //For now we consider on the function invocations of the input procedure only
          //TODO: Extend this to function invocations of other procedures
          val newTemps = fis.filter(_.funDef.equals(inFun)).foldLeft(currentTemps)((acc,fi) => {
             val invt = invTemplate(fi)
             acc ++ invt
          })                                             
          //recurse into children
          for(child <- n.Children)
            traverseBodyTree(child,newCtrs,newTemps)
        }
        case CtrLeaf() => {
          //add the currnetCtrs only if it is not unsat
          val pathExpr = And(currentCtrs.foldLeft(Seq[Expr]())((acc,ctr)=> (acc :+ ctr.expr)))
          val (res,model) = this.uiSolver.solveSATWithFunctionCalls(pathExpr)
          
          if(!res.isDefined || res.get == true) antSet +:= (currentCtrs,currentTemps)
          else{
            println("Found uninterpreted path: "+pathExpr)
            antSet
          } 
        }
      }
    }
    
    def traversePostTree(tree: CtrTree, postAnts : List[LinearTemplate], conseqs : List[LinearTemplate]): Unit = {
      tree match{
        case n@CtrNode(blkid) => {
          val ctrs = n.constraints          
          var newcons = conseqs ++ ctrs
          var newants = postAnts
          //find function invocations in ctrs
          val fis = ctrs.foldLeft(Set[FunctionInvocation]())((set,ctr) =>  set ++ getFIs(ctr))
          //generate a template for each function invocation and add it to the antecedents.          
          for (fi <- fis.filter(_.funDef.equals(inFun))) {           
            val invt = invTemplate(fi)
            newants ++= invt            
          }                                        
          //recurse into children
          for(child <- n.Children)
          {
            traversePostTree(child,newants,newcons)
          }
        }
        case CtrLeaf() => {
          //here we need to check if the every antecedent in antSet implies the conseqs of this path 
          val nonLinearCtrs = antSet.foldLeft(Set[Expr]())((acc,ants)=> {
            val allAnts = (ants._1 ++ ants._2 ++ postAnts)
            val allConseqs = (conseqs ++ inTemplates)
            //for debugging
        	//println("Antecedents : "+allAnts+" Consequents: "+allConseqs)
        	
        	//here we know that the antecedents are satisfiable 
            acc ++ genNonLinearCtrsFromImplications(allAnts,allConseqs)
          })          
          //look for a solution of non-linear constraints
          //println("Non linear constraints for this branch: " +nonLinearCtrs)
          val nlictr = And(nonLinearCtrs.toSeq)
          val (res,model) = this.uiSolver.solveSATWithFunctionCalls(nlictr)
          if(res.isDefined && res.get == true)
          {
            //construct an invariant (and print the model)
            val invs = inTemplates.map((inTemp) => {
              val coeff = inTemp.coeffTemplate.map((entry) => {
                val (k,v) = entry 
                v match {
                  case Variable(id) => (k,model(id))
                  case _ => (k,v)
                }                
              })
              val const = inTemp.constTemplate match {
                case Some(Variable(id)) => model(id)
                case Some(t) => t
                case None => IntLiteral(0)                  
              } 
              val expr = coeff.foldLeft(const)((acc,entry)=> Plus(acc,Times(entry._1,entry._2)))
              expr
            })
            println("Invariants: "+invs) 
          }
          else
          {
            println("Constriaint was not satisfiable")
          }
          
          //if found a solution return true and break
        }
      }
    }    

    //traverse the bodyTree and postTree 
    traverseBodyTree(bodyTree,List[LinearConstraint](),List[LinearTemplate]())
    traversePostTree(postTree,List[LinearTemplate](),List[LinearTemplate]())
        
    //solve the generated constraints using z3
    //println("Non-linear constraints: "+ctrNonLinear)
    None
  }

  /**
   * This procedure uses Farka's lemma to generate a set of non-linear constraints for the input implication.
   */
  def genNonLinearCtrsFromImplications(ants: Seq[LinearTemplate], conseqs: Seq[LinearTemplate]): Set[Expr] = {

    //compute the set of all constraint variables in ants
    val antCVars = ants.foldLeft(Set[Expr]())((acc, ant) => acc ++ ant.coeffTemplate.keySet)
    var nonLinearCtrs = Set[Expr]()
        
    for (conseq <- conseqs) {
      //create a set of identifiers one for each ants      
      //TODO: may need to alter the type 
      val lambdas = ants.map((ant) => (ant -> Variable(FreshIdentifier("l", true).setType(Int32Type)))).toMap

      //add a bunch of constraints on lambdas
      nonLinearCtrs ++= ants.collect((ant) => ant.template match {
        case t: LessEquals => GreaterEquals(lambdas(ant), IntLiteral(0))
      })
      
      val cvars = conseq.coeffTemplate.keys ++ antCVars
      //println("CVars: "+cvars.size)
      
      //compute the linear combination of all the coeffs of antCVars
      var sum: Expr = null
      //total number of constraint vars
      for (cvar <- cvars) {
        for (ant <- ants) {
          if (ant.coeffTemplate.contains(cvar)) {
            val addend = Times(lambdas(ant), ant.coeffTemplate.get(cvar).get)
            if (sum == null)
              sum = addend
            else
              sum = Plus(sum, addend)
          }
        }
        //make the sum equal to the coeff. of cvar in conseq
        if (conseq.coeffTemplate.contains(cvar))
          nonLinearCtrs += Equals(conseq.coeffTemplate.get(cvar).get, sum)
        else
          nonLinearCtrs += Equals(IntLiteral(0), sum)
      }
    }
    nonLinearCtrs
  }
  
  
  def getClauseListener(fundef: FunDef): ((Seq[Expr], Seq[Expr], Seq[Expr]) => Unit) = {
    var counter = 0;
    val listener = (body: Seq[Expr], post: Seq[Expr], newClauses: Seq[Expr]) => {
      //reconstructs the linear constraints corresponding to each path in the programs
      //A tree is used for efficiently representing the set of constraints corresponding to each path

      //initialize the goal clauses
      if (!post.isEmpty) {
        //println("Goal clauses: " + post)
        post.map(addConstraint(_, false))
        println("Goal Tree: " + postRoot.toString)
      }

      if (!body.isEmpty) {
        //println("Body clauses: " + body)
        body.map(addConstraint(_, true))
        println("Body Tree: " + bodyRoot.toString)
      }      
      
      //new clauses are considered as a part of the body
      if(!newClauses.isEmpty) {      
    	//println("new clauses: " + newClauses)
        newClauses.map(addConstraint(_, true))
        println("Body Tree: " + bodyRoot.toString)
        
        //solve for the templates at this unroll step
        val res = SolveForTemplates(fundef,bodyRoot,postRoot,getInvariantTemplate())
        System.exit(0)
      }            
    }
    listener
  }
  
  
  def run(ctx: LeonContext)(program: Program): VerificationReport = {

    val functionsToAnalyse: MutableSet[String] = MutableSet.empty
    var timeout: Option[Int] = None

    for (opt <- ctx.options) opt match {
      case LeonValueOption("functions", ListValue(fs)) =>
        functionsToAnalyse ++= fs

      case v @ LeonValueOption("timeout", _) =>
        timeout = v.asInt(ctx)

      case _ =>
    }

    this.uiSolver = new UninterpretedZ3Solver(ctx)
    this.uiSolver.setProgram(program)
    val reporter = ctx.reporter

    val trivialSolver = new TrivialSolver(ctx)
    val fairZ3 = new FairZ3Solver(ctx)

    val solvers0: Seq[Solver] = trivialSolver :: fairZ3 :: Nil
    val solvers: Seq[Solver] = timeout match {
      case Some(t) => solvers0.map(s => new TimeoutSolver(s, 1000L * t))
      case None => solvers0
    }

    solvers.foreach(_.setProgram(program))

    val defaultTactic = new DefaultTactic(reporter)
    defaultTactic.setProgram(program)
    /*val inductionTactic = new InductionTactic(reporter)
    inductionTactic.setProgram(program)*/

    def generateVerificationConditions: List[ExtendedVC] = {
      var allVCs: Seq[ExtendedVC] = Seq.empty
      val analysedFunctions: MutableSet[String] = MutableSet.empty

      for (funDef <- program.definedFunctions.toList.sortWith((fd1, fd2) => fd1 < fd2) 
          if (functionsToAnalyse.isEmpty || functionsToAnalyse.contains(funDef.id.name))) {
        analysedFunctions += funDef.id.name

        val tactic: Tactic = defaultTactic

        //add the template as a post-condition to all the methods

        /*allVCs ++= tactic.generatePreconditions(funDef)
          allVCs ++= tactic.generatePatternMatchingExhaustivenessChecks(funDef)*/
        allVCs ++= tactic.generateExtendedVCs(funDef)
        /*allVCs ++= tactic.generateMiscCorrectnessConditions(funDef)
          allVCs ++= tactic.generateArrayAccessChecks(funDef)*/

        allVCs = allVCs.sortWith((vc1, vc2) => {
          val id1 = vc1.funDef.id.name
          val id2 = vc2.funDef.id.name
          if (id1 != id2) id1 < id2 else vc1 < vc2
        })
      }

      val notFound = functionsToAnalyse -- analysedFunctions
      notFound.foreach(fn => reporter.error("Did not find function \"" + fn + "\" though it was marked for analysis."))

      allVCs.toList
    }

    def checkVerificationConditions(vcs: Seq[ExtendedVC]): VerificationReport = {

      for (vcInfo <- vcs) {
        val funDef = vcInfo.funDef
        val body = TransformNot(vcInfo.body)
        val post = TransformNot(vcInfo.post)

        reporter.info("Now considering '" + vcInfo.kind + "' VC for " + funDef.id + "...")
        reporter.info("Verification condition (" + vcInfo.kind + ") for ==== " + funDef.id + " ====")
        reporter.info("Body: " + simplifyLets(body))
        reporter.info("Post: " + simplifyLets(post))

        // try all solvers until one returns a meaningful answer
        var superseeded: Set[String] = Set.empty[String]
        solvers.find(se => {
          reporter.info("Trying with solver: " + se.name)
          if (superseeded(se.name) || superseeded(se.description)) {
            reporter.info("Solver was superseeded. Skipping.")
            false
          } else {
            superseeded = superseeded ++ Set(se.superseeds: _*)

            //set listeners        	  
            //se.SetModelListener(getModelListener(funDef))
            se.SetClauseListener(getClauseListener(funDef))

            val t1 = System.nanoTime
            se.init()
            //invoke the solver with separate body and post-condition
            //val (satResult, counterexample) = se.solveSAT(Not(vc))
            val (satResult, counterexample) = se.solve(body, post)
            val solverResult = satResult.map(!_)

            val t2 = System.nanoTime
            val dt = ((t2 - t1) / 1000000) / 1000.0

            solverResult match {
              case None => false
              case Some(true) => {
                reporter.info("==== VALID ====")

                vcInfo.value = Some(true)
                vcInfo.solvedWith = Some(se)
                vcInfo.time = Some(dt)

                true
              }
              case Some(false) => {
                reporter.error("Found counter-example : ")
                reporter.error(counterexample.toSeq.sortBy(_._1.name).map(p => p._1 + " -> " + p._2).mkString("\n"))
                reporter.error("==== INVALID ====")
                vcInfo.value = Some(false)
                vcInfo.solvedWith = Some(se)
                vcInfo.time = Some(dt)

                true
              }
            }
          }
        }) match {
          case None => {
            reporter.warning("No solver could prove or disprove the verification condition.")
          }
          case _ =>
        }

      }

      val report = new VerificationReport(vcs)
      report
    }

    reporter.info("Running Invariant Inference Phase...")

    val report = if (solvers.size > 1) {
      reporter.info("Running verification condition generation...")
      checkVerificationConditions(generateVerificationConditions)
    } else {
      reporter.warning("No solver specified. Cannot test verification conditions.")
      VerificationReport.emptyReport
    }

    report
  }

  /**
   * Dumps an input formula in princess format
   */
  /*var filecount :Int = 0  
  def DumpInPrincessFormat(parts: List[(FunDef,List[Expr])], guard: List[Expr])
  {   
	 val file = new java.io.File("princess-output"+filecount+".txt")
	 filecount += 1
	 file.createNewFile()	 
	 val writer = new java.io.BufferedWriter(new java.io.FileWriter(file))
	 
	  //declare the list of free variables (consider only integers for now)
	  val freevars = variablesOf(And(guard))
	  writer.write("\\functions {\n")
	  freevars.foreach((id) => id.getType match {
	    case Int32Type => writer.write("int "+id.toString+";") 
	    case BooleanType => ;//reporter.warning("Boolean types present not handling them for now ")
	    case _ => ;
	  })
	  writer.write("\n}")
	  writer.newLine()
	  
	  //considering only binary operators
	  def appendInfix(lhs: String,rhs: String,op: String) : String = {
	    lhs  + (if(rhs.isEmpty()) "" 
	    	  else if(lhs.isEmpty()) rhs
	    	  else (op +rhs))
	  }
	  
	  //considering only unary operators
	  def appendPrefix(opd: String,op: String) : String = {
	    if(opd.isEmpty()) opd
	    else op + "("+opd+")"
	  }
	  
	  //create a function to convert leon expressions into princess formulas (not all operations are supported)
	  //note: princess does not support boolean type. Hence, we need to replace boolean variables by a predicate
	  // which may introduce disjunctions
	  def PrinForm(formula: Expr) : String = formula match {
	    case And(args) => args.foldLeft(new String())((str,x) => {
	    	appendInfix(str,PrinForm(x)," & ")	    		    	
	    })
	    case Or(args) => args.foldLeft(new String())((str,x) => appendInfix(str,PrinForm(x)," | "))
	    
	    case Variable(id) => id.getType match {
	    							case Int32Type => id.toString	    							
	    							case _ => ""
	    						}
	    case IntLiteral(v) => v.toString
	    case BooleanLiteral(v) => v.toString	    
	    
	    case t@Not(Variable(id)) => reporter.warning("Encountered negation of a variable: " + t); ""
	    case Not(t) => appendPrefix(PrinForm(t),"!")	    
	    case UMinus(t) => appendPrefix(PrinForm(t),"-")
	    	    	   
	    case t@Iff(t1,t2) => {
	     //appendInfix(PrinForm(Implies(t1,t2)),PrinForm(Implies(t2,t1))," & ")
	     //this is a temporary hack to handle the post-condition
	      val (lhs,rhs) = (PrinForm(t1),PrinForm(t2))
	      if(lhs.isEmpty() && rhs.isEmpty()) ""
	      else if(lhs.isEmpty()) PrinForm(t2)
	      else if(rhs.isEmpty()) PrinForm(t1)
	      else {
	       reporter.warning("Both LHS and RHS are bool expressions: "+t);
	       "" 
	      }
	    }
	      					
	    case Implies(t1,t2) => PrinForm(Or(Not(t1),t2))
	    
	    case Equals(t1,t2) => appendInfix(PrinForm(t1),PrinForm(t2),"=")
	    case Plus(t1,t2) => appendInfix(PrinForm(t1),PrinForm(t2),"+")
	    case Minus(t1,t2) => appendInfix(PrinForm(t1),PrinForm(t2),"-")
	    case Times(t1,t2) => appendInfix(PrinForm(t1),PrinForm(t2),"*")
	    case LessThan(t1,t2) => appendInfix(PrinForm(t1),PrinForm(t2),"<")
	    case GreaterThan(t1,t2) => appendInfix(PrinForm(t1),PrinForm(t2),">")
	    case LessEquals(t1,t2) => appendInfix(PrinForm(t1),PrinForm(t2),"<=")
	    case GreaterEquals(t1,t2) => appendInfix(PrinForm(t1),PrinForm(t2),">=")	    
	    case _ => "" //empty string in other cases
	  }
	  
	  //create formula parts
	  writer.write("\\problem{\n")	  
	  
	  var partcount = 0
	  var processedFormulas = List[Expr]()
	  var partnames = List[String]()
	  	  
	  parts.foreach((elem) => {
	    val (func,list) = elem	    
	    
	    val formulas = list -- processedFormulas
	    val partstr = func.id.toString() + partcount
	    writer.write("\\part["+ partstr  +"]"+"\t")
	    writer.write("(" + PrinForm(And(formulas)) +")")
	    
	    if(partcount < parts.length - 1)
	      writer.write(" &\n")
	    else writer.write("\n")
	    
	    //update mutable state variables
	    processedFormulas = processedFormulas ++ formulas
	    partnames = partnames :+ partstr
	    partcount = partcount + 1
	  })
	  
	  //add the implies block
	  writer.write("->\n") 	  
	  
	  //add the final part
	   val leftFormula = guard -- processedFormulas	   
	   writer.write("\\part[assert]"+"\t")
	   writer.write("(" + PrinForm(And(leftFormula)) +")")
	   writer.write("}\n")
	   
	   //add assert to partnames
	   partnames = partnames :+ "assert"
	   
	   //add interpolant blocks	   
	   for( i <- 1 to partnames.length - 1 )  {
	      val (phrase,index) = partnames.foldLeft((new String(),1))(
	      (g,x) => {	      
	    	  	val (ipstr,index) = g
	    	  	if(index == i + 1 && index > 1) (ipstr + ";" + x, index + 1)
	    	  	else if(index > 1) (ipstr + "," + x, index + 1)
	    	  	else (x, index + 1)
	      	})
	      writer.write("\\interpolant {"+phrase+"}\n")	     
	   }
	  writer.flush()
	  writer.close()	  
  }

*/

  /*def getModelListener(funDef: FunDef) : (Map[Identifier, Expr]) => Unit = {
      
      //create an interpolation solver
      val interpolationSolver = new PrincessSolver(ctx)
      val pre = if (funDef.precondition.isEmpty) BooleanLiteral(true) else matchToIfThenElse(funDef.precondition.get)
      val body = matchToIfThenElse(funDef.body.get)
      val resFresh = FreshIdentifier("result", true).setType(body.getType)
      val post = replace(Map(ResultVariable() -> Variable(resFresh)), matchToIfThenElse(funDef.postcondition.get))

      */
  /**
   * This function will be called back by the solver on discovering an input
   */ /*
      val processNewInput = (input: Map[Identifier, Expr]) => {
        //create a symbolic trace for pre and body
        var symtraceBody = input.foldLeft(List[Expr]())((g, x) => { g :+ Equals(Variable(x._1), x._2) })
        var parts = List[(FunDef, List[Expr])]()

        //compute the symbolic trace induced by the input
        val (tracePre, partsPre) =
          if (funDef.precondition.isDefined) {
            val resPre = new TraceCollectingEvaluator(ctx, program).eval(pre, input)
            resPre match {
              case EvaluationWithPartitions(BooleanLiteral(true), SymVal(guardPre, valuePre), partsPre) => {
                ((guardPre :+ valuePre), partsPre)
              }
              case _ =>
                reporter.warning("Error in colleting traces for Precondition: " + resPre + " For input: " + input)
                (List[Expr](), List[(FunDef, List[Expr])]())
            }
          } else (List[Expr](), List[(FunDef, List[Expr])]())
        symtraceBody ++= tracePre
        parts ++= partsPre

        //collect traces for body
        val resBody = new TraceCollectingEvaluator(ctx, program).eval(body, input)
        resBody match {
          case EvaluationWithPartitions(cval, SymVal(guardBody, valueBody), partsBody) => {
            //collect traces for the post-condition
            val postInput = input ++ Map(resFresh -> cval)
            val resPost = new TraceCollectingEvaluator(ctx, program).eval(post, postInput)
            resPost match {
              case EvaluationWithPartitions(BooleanLiteral(true), SymVal(guardPost, valuePost), partsPost) => {
                //create a symbolic trace for pre and body
                symtraceBody ++= (guardBody :+ Equals(Variable(resFresh), valueBody))

                //create a set of parts for interpolating
                parts ++= partsBody ++ partsPost :+ (funDef, symtraceBody)

                //print each part for debugging
                //parts.foreach((x) => { println("Method: " + x._1.id + " Trace: " + x._2) })

                //create a symbolic trace including the post condition
                val pathcond = symtraceBody ++ (guardPost :+ valuePost)
                //println("Final Trace: " + pathcond)

                //convert the guards to princess input
                //DumpInPrincessFormat(parts, pathcond)         
                val interpolants = interpolationSolver.getInterpolants(parts,pathcond)
              }
              case EvaluationWithPartitions(BooleanLiteral(true), symval, parts) => {
                reporter.warning("Found counter example for the post-condition: " + postInput)
              }
              case _ => reporter.warning("Error in colleting traces for post: " + resPost + " For input: " + postInput)
            }
          }
          case _ => reporter.warning("Error in colleting traces for body: " + resBody + " For input: " + input)
        }
      }
      
      processNewInput
    }
*/

}
