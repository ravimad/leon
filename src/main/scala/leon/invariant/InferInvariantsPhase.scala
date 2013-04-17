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
 */
object InferInvariantsPhase extends LeonPhase[Program, VerificationReport] {
  val name = "InferInv"
  val description = "Invariant Inference"
  
  override val definedOptions: Set[LeonOptionDef] = Set(
    LeonValueOptionDef("functions", "--functions=f1:f2", "Limit verification to f1,f2,..."),
    LeonValueOptionDef("timeout", "--timeout=T", "Timeout after T seconds when trying to prove a verification condition."))

  class ClauseListenerFactory(reporter: Reporter, program: Program, context: LeonContext, uisolver: UninterpretedZ3Solver) {    

    def getClauseListener(fundef: FunDef): ((Seq[Expr], Seq[Expr], Seq[Expr]) => Unit) = {
      val constTracker = new ConstraintTracker(fundef)
      val templateFactory = new TemplateFactory()

      //this is set by the listener
      var resultVar: Option[Identifier] = None

      val listener = (body: Seq[Expr], post: Seq[Expr], newClauses: Seq[Expr]) => {
        //reconstructs the linear constraints corresponding to each path in the programs
        //A tree is used for efficiently representing the set of constraints corresponding to each path

        //initialize the goal clauses
        if (!post.isEmpty) {
          println("Post clauses: ")
          post.foreach(println(_))

          //set the result variable
          val resvars = post.foldLeft(Set[Identifier]())((acc, e) => acc ++ variablesOf(e).find(_.name.equals("result")))
          if (resvars.size > 0) {
            if (resvars.size > 1)
              throw IllegalStateException("More than one result identifier: " + resvars)
            else resultVar = Some(resvars.first)
          }

          constTracker.addPostConstraints(post)
          //println("Goal Tree: " + postRoot.toString)
        }

        if (!body.isEmpty) {
          println("Body clauses: ")
          body.foreach(println(_))
          constTracker.addBodyConstraints(body)
          //println("Body Tree: " + bodyRoot.toString)
        }

        //new clauses are considered as a part of the body
        if (!newClauses.isEmpty) {
          println("Unrolled clauses: ")
          newClauses.foreach(println(_))
          constTracker.addBodyConstraints(newClauses)
          //println("Body Tree: " + bodyRoot.toString)

          //solve for the templates at this unroll step
          //get the template for the inFun
          //val fi = FunctionInvocation(fundef,fundef.args.map(_.toVariable))
          val baseTerms = fundef.args.map(_.toVariable) ++ (resultVar match {
            case Some(v) => Seq(Variable(v))
            case _ => Seq()
          })

          val inTemplates = templateFactory.constructTemplate(baseTerms, fundef)
          val templateSynthesizer = templateFactory.getTemplateSynthesizer()
          val res = constTracker.solveForTemplates(fundef, templateSynthesizer, inTemplates, uisolver)
          if (res.isDefined) {                       
            val inv = res.get(fundef)            
            reporter.info("- Found inductive invariant: " + inv)
            //check if this is an invariant 
            reporter.info("- Verifying Invariant " + res.get(fundef))

            //create a new post-condition            
            val newPost = if (resultVar.isDefined) replace(Map(Variable(resultVar.get) -> ResultVariable()), inv)
            			  else inv
            val postExpr = And(fundef.postcondition.get, newPost)
            verifyInvariant(fundef,context,program,postExpr,reporter)            
            System.exit(0)
          }
        }
      }
      listener
    }
  }
  
  /**
   * create a program with the input function replaced by a new function that has the new postcondition
   */
  def verifyInvariant(fundef: FunDef, ctx: LeonContext, program: Program, newpost: Expr, reporter: Reporter): Boolean = {
    
    val newfundef = new FunDef(FreshIdentifier(fundef.id.name, true), fundef.returnType, fundef.args)
    //replace the recursive invocations by invocations of the new function  
    val newbody = searchAndReplaceDFS((e: Expr) => (e match {
      case fi @ FunctionInvocation(fd, args) if (fd == fundef) => Some(FunctionInvocation(newfundef, args))
      case _ => None
    }))(fundef.body.get)
    newfundef.body = Some(newbody)
    //assuming pre and postconditions do not have recursive calls
    //TODO: Noncritical correctness issue: fix this                    
    newfundef.precondition = fundef.precondition
    newfundef.postcondition = Some(newpost)
    val newfuncs = program.mainObject.definedFunctions.filter(_ != fundef) :+ newfundef
    val newObjDef = ObjectDef(program.mainObject.id.freshen, newfuncs ++ program.mainObject.definedClasses, program.mainObject.invariants)
    val newprog = Program(program.id.freshen, newObjDef)
    //println("Program: "+newprog)

    val defaultTactic = new DefaultTactic(reporter)
    defaultTactic.setProgram(newprog)
    val vc = defaultTactic.generatePostconditions(newfundef).first

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
        false
      }
      case _ => {
        reporter.error("- Unable to prove or disprove invariant")
        false
      }
    }
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

    //create an ui solver
    val uisolver = new UninterpretedZ3Solver(ctx)
    uisolver.setProgram(program)    
    val reporter = ctx.reporter
    
    //create a clause listener factory
    val listenerFactory = new ClauseListenerFactory(reporter,program,ctx,uisolver)

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
        /*val body = TransformNot(vcInfo.body)
        val post = TransformNot(vcInfo.post)*/
        val body = vcInfo.body
        val post = vcInfo.post

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
            se.SetClauseListener(listenerFactory.getClauseListener(funDef))

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
}