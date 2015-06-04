package leon
package smtlib

import java.io._
import utils._
import purescala._
import purescala.Common._
import purescala.Trees._
import purescala.TreeOps._
import purescala.Extractors._
import purescala.TypeTrees._
import _root_.smtlib.sexpr.SExprs._
import _root_.smtlib.Commands.{ GetValue, NonStandardCommand, Command }
import _root_.smtlib.CommandResponses.SExprResponse
import _root_.smtlib.CommandResponses._
import _root_.smtlib._
import Definitions._
import _root_.smtlib.Commands.DeclareFun
import leon.invariant.factories._

class ExprToSygus(ctx: LeonContext) {

  var commands = List[Command]()
  val target = new SMTLIBSygusTarget(ctx) {
    def getNewInterpreter() = new Interpreter {
      override def eval(cmd: Command): CommandResponse = {
        commands :+= cmd
        Success
      }

      override def free(): Unit = {
        //nothing to be done here
      }
    }
  }

  def getCommands = commands

  def toSExprAndDefinitions(expr: Expr): (List[Command], SExpr) = {
    variablesOf(expr).filterNot(TemplateIdFactory.IsTemplateIdentifier _ ).foreach(target.declareVariable)
    val sexpr = target.toSMT(expr)(Map())
    (commands, sexpr)
  }  
}

abstract class SMTLIBSygusTarget(context: LeonContext) extends SMTLIBTarget(context) {

  def targetName = "SyGus"
    
  val synthVariables    = new Bijection[Identifier, SSymbol]()

  /**
   * The passed identifier is an unknown and has to be synthesized.
   * So declare it as a synthfun
   */
  def declareSynthFun(id: Identifier) : SSymbol = {   
    synthVariables.cachedB(id){
      val name = id2sym(id)
      val returnSort = declareSort(Int32Type) //note: here templates are interpreted as integers and not as reals                 
      val cmd = NonStandardCommand(SList(SSymbol("synth-fun"), name, SList(List()), returnSort, declareGrammar()))
      sendCommand(cmd)
      name
    }         
  }
  
  def declareGrammar() = {
    //List(Nonterm1, Nonterm2, ...)
    //Nonterm1 = List(name, type, Productions)
    //Productions = List(rhs1, rhs2, ...)
    //rhs1 - List(sym1, sym2, ...)           
    val nonterm = SSymbol("Start")
    val nttype = declareSort(Int32Type) 
    val prods = SList(List(SList(List(SSymbol("Constant"), nttype))))
    val firstEntry = SList(List(nonterm, nttype, prods))
    SList(List(firstEntry))
  }   

  override def declareVariable(id: Identifier): SSymbol = {
    variables.cachedB(id) {
      val s = id2sym(id)
      //println("sort: "+id+" --> "+id.getType)
      val cmd = NonStandardCommand(SList(SSymbol("declare-var"), s, declareSort(id.getType)))
      sendCommand(cmd)
      s
    }
  }

  /*override def declareFunction(tfd: TypedFunDef): SSymbol = {
    functions.cachedB(tfd) {
      val id = if (tfd.tps.isEmpty) {
        tfd.id
      } else {
        FreshIdentifier(tfd.id.name)
      }
      val s = id2sym(id)
      //otherwise the function will be synthesized and therefore has to declared differently
      if (p.definedFunctions.contains(tfd.fd))
        sendCommand(DeclareFun(s.s, tfd.params.map(p => declareSort(p.tpe)), declareSort(tfd.returnType)))
      s
    }
  }*/

  override def toSMT(e: Expr)(implicit bindings: Map[Identifier, SExpr]) = e match {
    //handle template identifiers specially
    case Variable(id) if(TemplateIdFactory.IsTemplateIdentifier(id)) =>      
      //create a new synthesis function definition if it doesn not exist
      val synfun = declareSynthFun(id)     
      SList(synfun) //simply invoke the function              
      	
    case _ =>
      super.toSMT(e)
  }
}
