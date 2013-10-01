package leon
package invariant

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
import leon.invariant._
import scala.collection.mutable.{Set => MutableSet}

/**
 * @author ravi
 * This phase performs automatic invariant inference. 
 * TODO: handle options
 */
object InferInvariantsPhase extends LeonPhase[Program, VerificationReport] {
  val name = "InferInv"
  val description = "Invariant Inference"
  
  override val definedOptions: Set[LeonOptionDef] = Set(
    LeonValueOptionDef("functions", "--functions=f1:f2", "Limit verification to f1,f2,..."),
    LeonValueOptionDef("timeout", "--timeout=T", "Timeout after T seconds when trying to prove a verification condition."))

  //TODO: handle direct equality and inequality on ADTs
  class InferenceEngineGenerator(reporter: Reporter, program: Program, context: LeonContext,
    uisolver: UninterpretedZ3Solver) {

    def getInferenceEngine(funDef: FunDef, tempGen : TemplateGenerator): (() => Option[Boolean]) = {

      //Create and initialize a constraint tracker
      val constTracker = new ConstraintTracker(funDef)
      
      //create a body and post of the function
      val body = funDef.body.get
      val post = funDef.postcondition.get 
      
      val resFresh = Variable(FreshIdentifier("result", true).setType(body.getType))      
      val simpBody = matchToIfThenElse(body)
      val bodyExpr = Equals(resFresh, simpBody)
      val postExpr = replace(Map(ResultVariable() -> resFresh), matchToIfThenElse(post))
      
      //flatten the functions in the body      
      val flatBody = ExpressionTransformer.normalizeExpr(bodyExpr)      
      //println("falttened Body: " + flatBody)

      //create a postcondition template 
      val postTemp = if (program.isRecursive(funDef)) {
        
        val argmap = InvariantUtil.formalToAcutal(
          Call(resFresh, FunctionInvocation(funDef, funDef.args.map(_.toVariable))),
          ResultVariable())

        //Some(TemplateFactory.constructTemplate(argmap, vc.funDef))
        val temp = TemplateFactory.constructTemplate(argmap, funDef, Some(tempGen))
        Some(temp)
      } else None
      
      val npost = ExpressionTransformer.normalizeExpr(Not(postExpr))

      //add the negation of the post-condition "or" the template
      //note that we need to use Or as we are using the negation of the disjunction
      val fullPost = if (postTemp.isDefined)
        Or(npost, ExpressionTransformer.normalizeExpr(Not(postTemp.get)))
      else npost
      //println("Full Post: "+fullPost)
      constTracker.addPostConstraints(funDef, fullPost)
      constTracker.addBodyConstraints(funDef, flatBody)

      val (btree, ptree) = constTracker.getVC(funDef)
      //println("Body Constraint Tree: "+btree)      

      //create entities that uses the constraint tracker
      val lsAnalyzer = new LinearSystemAnalyzer(constTracker, reporter)
      val vcRefiner = new RefinementEngine(program, constTracker, Some(tempGen))
      vcRefiner.initialize()

      var refinementStep: Int = 0
      val inferenceEngine = () => {

        val refined =
          if (refinementStep >= 1) {

            reporter.info("- More unrollings for invariant inference")

            val unrolledCalls = vcRefiner.refineAbstraction()
            if (unrolledCalls.isEmpty) {
              reporter.info("- Cannot do more unrollings, reached unroll bound")
              false
            }
            else true            
          } else true

        refinementStep += 1

        if (!refined) Some(false)
        else {
          //solve for the templates at this unroll step
          //val res = lsAnalyzer.solveForTemplates(uisolver)
          val res = lsAnalyzer.solveForTemplatesIncr(uisolver)

          if (res.isDefined) {

            res.get.foreach((pair) => {
              val (fd, inv) = pair
              reporter.info("- Found inductive invariant: " + fd.id + " --> " + inv)
            })
            reporter.info("- Verifying Invariants... ")

            verifyInvariant(program, context, reporter, res.get, funDef)
            System.exit(0)
            Some(true)
          } else {            
            //here, we do not know if the template is solvable or not, we need to do more unrollings.
            None
          }
        }
      }
      inferenceEngine
    }
  }

  /**
   * This function creates a new program with each functions postcondition strengthened by
   * the inferred postcondition
   */
  def verifyInvariant(program: Program, ctx: LeonContext, reporter: Reporter,
    newposts: Map[FunDef, Expr], rootfd: FunDef): Boolean = {

    //create a fundef for each function in the program
    val newFundefs = program.mainObject.definedFunctions.map((fd) => {
      val newfd = new FunDef(FreshIdentifier(fd.id.name, true), fd.returnType, fd.args)
      (fd, newfd)
    }).toMap

    val replaceFun = (e: Expr) => e match {
      case fi @ FunctionInvocation(fd1, args) if newFundefs.contains(fd1) =>
        Some(FunctionInvocation(newFundefs(fd1), args))
      case _ => None
    }

    //create a body, pre, post for each newfundef
    newFundefs.foreach((entry) => {
      val (fd, newfd) = entry

      //add a new precondition
      newfd.precondition =
        if (fd.precondition.isDefined)
          Some(searchAndReplaceDFS(replaceFun)(fd.precondition.get))
        else None

      //add a new body
      newfd.body = if (fd.body.isDefined)
        Some(searchAndReplaceDFS(replaceFun)(fd.body.get))
      else None

      //add a new postcondition                  
      val newpost = if (newposts.contains(fd)) {
        val inv = newposts(fd)
        if (fd.postcondition.isDefined)
          Some(And(fd.postcondition.get, inv))
        else Some(inv)
      } else fd.postcondition

      newfd.postcondition = if (newpost.isDefined)
        Some(searchAndReplaceDFS(replaceFun)(newpost.get))
      else None
    })

    val newfuncs = newFundefs.values.toSeq
    val otherDefs = program.mainObject.defs.diff(program.mainObject.definedFunctions)

    val newObjDef = ObjectDef(program.mainObject.id.freshen, 
      newfuncs ++ otherDefs, program.mainObject.invariants)

    val newprog = Program(program.id.freshen, newObjDef)
    //println("Program: "+newprog)

    val defaultTactic = new DefaultTactic(reporter)
    defaultTactic.setProgram(newprog)
    val vc = defaultTactic.generatePostconditions(newFundefs(rootfd)).first

    val fairZ3 = new FairZ3Solver(ctx)
    fairZ3.setProgram(newprog)
    //println("Func : "+ vc.funDef + " new vc: "+vc.condition)            
    val sat = fairZ3.solveSAT(Not(vc.condition))
    sat._1 match {
      case Some(false) => {
        reporter.info("- Invariant verified")
        true
      }
      case Some(true) => {
        reporter.error("- Invalid invariant, model: " + sat._2)
        System.exit(-1)
        false
      }
      case _ => {
        reporter.error("- Unable to prove or disprove invariant")
        System.exit(-1)
        false
      }
    }
  }

  def run(ctx: LeonContext)(program: Program): VerificationReport = {

    val reporter = ctx.reporter
    reporter.info("Running Invariant Inference Phase...")

    val functionsToAnalyse: MutableSet[String] = MutableSet.empty
    var timeout: Option[Int] = None

    for (opt <- ctx.options) opt match {
      case LeonValueOption("functions", ListValue(fs)) =>
        functionsToAnalyse ++= fs

      case v @ LeonValueOption("timeout", _) =>
        timeout = v.asInt(ctx)

      case _ =>
    }

    //create an ui solver
    val uisolver = new UninterpretedZ3Solver(ctx)
    uisolver.setProgram(program)    

    val infEngineGen = new InferenceEngineGenerator(reporter, program, ctx, uisolver)
    val analysedFunctions: MutableSet[String] = MutableSet.empty

    for (
      funDef <- program.definedFunctions.toList.sortWith((fd1, fd2) => fd1 < fd2) 
      if (functionsToAnalyse.isEmpty || functionsToAnalyse.contains(funDef.id.name))
    ) {
      analysedFunctions += funDef.id.name

      if (funDef.body.isDefined && funDef.postcondition.isDefined) {
        val body = funDef.body.get
        val post = funDef.postcondition.get

        reporter.info("- considering function " + funDef.id + "...")
        reporter.info("Body: " + simplifyLets(body))
        reporter.info("Post: " + simplifyLets(post))

        if (post == BooleanLiteral(true)) {
          reporter.info("Post is true, nothing to be proven!!")
        } else {
         
          val t1 = System.nanoTime

          //create a template generator that generates templates for the functions
          val tempGen = TemplateFactory.getTemplateGenerator(program)
          var solved : Option[Boolean] = None

          while (!solved.isDefined) {
            
            val infEngine = infEngineGen.getInferenceEngine(funDef, tempGen)
            var infRes: Option[Boolean] = None

            while (!infRes.isDefined) {
              infRes = infEngine()
            }
            solved = infRes match {
              case Some(true) => Some(true)
              case Some(false) => {
                reporter.info("- Template not solvable!!")
                //refine the templates here
                val refined = TemplateFactory.refineTemplates(tempGen)
                if(refined) None
                else Some(false)
              }
              case _ => throw new IllegalStateException("This case is not possible")
            }
          }
          if(!solved.get) 
            reporter.info("- Exhausted all templates, cannot infer invariants")
                                         
          val t2 = System.nanoTime
          val dt = ((t2 - t1) / 1000000) / 1000.0
        }
      }
    }
    val notFound = functionsToAnalyse -- analysedFunctions
    notFound.foreach(fn => reporter.error("Did not find function \"" + fn + "\" though it was marked for analysis."))
    VerificationReport.emptyReport
  }
}
