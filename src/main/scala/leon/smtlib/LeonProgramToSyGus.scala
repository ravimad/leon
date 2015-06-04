package leon
package smtlib

import java.io._
import utils._
import purescala._
import purescala.Definitions._
import purescala.Common._
import purescala.Trees._
import purescala.TreeOps._
import purescala.Extractors._
import purescala.TypeTrees._
import _root_.smtlib.sexpr.SExprs._
import _root_.smtlib.Commands.{ Command, Assert, NonStandardCommand }
import _root_.smtlib.{ PrettyPrinter => SMTPrinter }
import _root_.smtlib.CommandResponses.SExprResponse
import _root_.smtlib.CommandResponses._
import _root_.smtlib._
import leon.invariant.structure.FunctionInfoFactory
import leon.invariant.util._

/*object LeonToSyGusPhase extends UnitPhase[Program] {
  val name = "genSyGus"
  val description = "Dumps template inference in SyGus syntax"

  override val definedOptions: Set[LeonOptionDef] = Set(
    LeonValueOptionDef("outfilename", "--outfilename=<filename>", "name of the output SMTLIB file"))

  def apply(ctx: LeonContext, program: Program) = {

    val reporter = ctx.reporter
    reporter.info("Running SyGus generation Phase...")

    var outfilename = "smtlib"
    var removeOrs = true

    for (opt <- ctx.options) opt match {
      case LeonValueOption("outfilename", ListValue(fs)) => {
        outfilename = fs(0)
      }
      case _ => ;
    }
    val t1 = System.currentTimeMillis()
    new ProgramToSygus(program, ctx).toSygus(outfilename)
    val t2 = System.currentTimeMillis()
    reporter.info("Completed in " + (t2 - t1) / 1000.0 + "s")
  }
}*/

class LeonExprtoSynthlib(expr: Expr, ctx: LeonContext) {

  val smtlibDeclarations = new ExprToSygus(ctx)
  /*var smtlibFuncBodies = List[Command]()
  var smtlibFuncSpec = List[Command]()
  var smtlibSynthDecls = List[Command]()*/
  val tru = BooleanLiteral(true)
  val fls = BooleanLiteral(false)

  def toSygus(filename: String) {

    val header = List(NonStandardCommand(SList(SSymbol("set-logic"), SSymbol("LIA"))))
    val trailer = List(NonStandardCommand(SList(SSymbol("check-synth"))))
    val writer = new PrintWriter(filename + ".sl")

    def writeCmds(cmds: List[Command]) = cmds.foreach {
      case cmd =>
        SMTPrinter(cmd, writer)
        writer.println()
    }

    //the following will add declarations of all variables and functions
    val (_, sexpr) = smtlibDeclarations.toSExprAndDefinitions(expr)
    writeCmds(header)
    writer.println()
    writeCmds(smtlibDeclarations.getCommands)
    writer.println()
    writeCmds(List(NonStandardCommand(SList(SSymbol("constraint"), sexpr)))) //add the constraint
    writer.println()
    writeCmds(trailer)
    writer.println()
    writer.flush()
    writer.close()
  }

  /**
   * For now assuming that there are no type parameters or ADTs.
   * Also that the template does not have any user-defined functions
   * or ADTs.
   */
  /*private def assertFunction(fd: FunDef) = {
    if (fd.hasBody) {
      val params = fd.params.map(_.id)
      val funInvoke = FunctionInvocation(TypedFunDef(fd, fd.tparams.map(_.tp)),
        params.map(_.toVariable))
      val defExpr = Equals(funInvoke, matchToIfThenElse(fd.body.get))
      //      /println("Defexpr: "+defExpr)
      //the following will declare all the variables in the body as declare-vars
      val (_, sexpr) = smtlibDeclarations.toSExprAndDefinitions(defExpr)

      //universally quantify all parameters
      //val varmap = smtlibDeclarations.target.variables
      //val sparams = params.map(p => SList(varmap.getB(p).get, smtlibDeclarations.target.sorts.getB(p.getType).get))
      //val quantSExpr = SList(SSymbol("forall"), SList(sparams.toList), sexpr)      
      smtlibFuncBodies :+= NonStandardCommand(SList(SSymbol("constraint"), sexpr))
      //assert that the specification holds       
      if (fd.hasPostcondition) {
        val (resvar, postExpr) = fd.postcondition.get
        val defPost = replace(Map(resvar.toVariable -> funInvoke), postExpr)
        val defSpec = matchToIfThenElse(if (fd.hasPrecondition) {
          Implies(fd.precondition.get, defPost)
        } else
          defPost)
        //We are using different set of variables here to be safe, but this may not be necessary
        // since we only have bounded variables and they are all in conjunction
        val nparamsMap: Map[Expr, Expr] = params.map(id => id.toVariable ->
          FreshIdentifier(id.name, true).setType(id.getType).toVariable).toMap
        val spec = replace(nparamsMap, defSpec)
        val (_, sSpec) = smtlibDeclarations.toSExprAndDefinitions(spec)
        //val quantSpec = SList(SSymbol("forall"), SList(sparams.toList), sSpec)
        smtlibFuncSpec :+= NonStandardCommand(SList(SSymbol("constraint"), sSpec))
      }

      //define constraints for the synthesis function after declaring it 
      val finfo = FunctionInfoFactory.getFunctionInfo(fd)
      if (finfo.isDefined && finfo.get.hasTemplate) {                
        val resvar =  fd.postcondition.get._1
        //construct a function for synthesis
        val synthFun = new FunDef(FreshIdentifier(fd.id.name + "-template"), List(), 
            BooleanType, (params :+ resvar).map(id => ValDef(id, id.getType)))
        //declare the functions that need to be synthesized
        smtlibSynthDecls :+= NonStandardCommand(smtlibDeclarations.declareSynthFun(synthFun, finfo.get.getTemplate))
        
        val synthfunInvoke = FunctionInvocation(TypedFunDef(synthFun, List()), params.map(_.toVariable) :+ funInvoke)
        val defTemp = matchToIfThenElse(if (fd.hasPrecondition) {
          Implies(fd.precondition.get, synthfunInvoke)
        } else
          synthfunInvoke)
        //rename the parameters 
        val nparamsMap: Map[Expr, Expr] = params.map(id => id.toVariable ->
          FreshIdentifier(id.name, true).setType(id.getType).toVariable).toMap
        val ntemp = replace(nparamsMap, defTemp)
        val (_, sTemp) = smtlibDeclarations.toSExprAndDefinitions(ntemp)       
        smtlibFuncSpec :+= NonStandardCommand(SList(SSymbol("constraint"), sTemp))
      }
    }
  }*/
}
