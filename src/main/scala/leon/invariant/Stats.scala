package leon
package invariant

import purescala.Common._
import purescala.Definitions._
import purescala.Trees._
import purescala.TreeOps._
import purescala.Extractors._
import purescala.TypeTrees._
import scala.collection.mutable.{ Set => MutableSet }
import scala.collection.immutable.Stack
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
import scala.collection.mutable.{Set => MutableSet}
import java.io._

object Stats {
  //stats  
  var totalTime : Long = 0
  var minimizationTime : Long = 0
  var outerIterations = 0
  var innerIterations = 0
  var retries = 0
  
  //per outer iteration statistics
  var cumVCsize : Long = 0
  var maxVCsize : Long = 0
  var cumUIFADTs : Long = 0
  var maxUIFADTs : Long = 0
  var cumTempVars : Long = 0
  var maxTempVars : Long= 0
  
  //non-linear solver stats    
  var cumFarkaSize : Long = 0
  var maxFarkaSize : Long = 0
  var cumFarkaTime : Long = 0
  var maxFarkaTime : Long = 0  
  var cumNLsize : Long = 0
  var maxNLsize : Long = 0
  var cumDijsize : Long = 0
  var maxDijsize : Long = 0
  var cumExploreTime: Long  = 0
  var maxExploreTime: Long  = 0
  var cumCompatCalls : Long = 0
  var maxCompatCalls : Long = 0
  var cumElimVars: Long  = 0
  var maxElimVars: Long  = 0
  var cumElimAtoms: Long  = 0
  var maxElimAtoms: Long  = 0  
  var cumLemmaApps: Long = 0
  var maxLemmaApps: Long  = 0
  
  //cegis solver stats
  var cegisIterations = 0
  var cumTemplateCtrSize : Long = 0  
  var maxTemplateCtrSize : Long = 0
  var boundsTried = 0
  
  //minimization stats
  var lowerBounds = Map[FunDef, Map[Variable, RealLiteral]]()
  var lowerBoundsOutput = Map[FunDef, String]()
  
  var output : String = ""    
  def addOutput(out : String) = {
    output += out + "\n"
  }
  
  def addLowerBoundStats(fd: FunDef, lbMap : Map[Variable,RealLiteral], out: String) = {
    lowerBounds += (fd -> lbMap)
    lowerBoundsOutput += (fd -> out)
  }  
  
  def cumMax(cum : Long, max: Long, newval : Long) : (Long,Long) = {    
    (cum + newval, if(max < newval) newval else max)
  }
  
  def dumpStats(pr : PrintWriter) ={
    //outer iteration statistics
    pr.println("Total Time: "+(totalTime/1000.0)+"s")
    pr.println("Min Time: "+(minimizationTime/1000.0)+"s")
    pr.println("VC refinements : "+outerIterations)    
    pr.println("Avg VC size : "+ (cumVCsize.toDouble / outerIterations))
    pr.println("Max VC size : "+maxVCsize)
    pr.println("Avg UIF-ADT size : "+ (cumUIFADTs.toDouble / outerIterations))
    pr.println("Max UIF-ADT size : "+ maxUIFADTs)        
    pr.println("avgTempVars : "+(cumTempVars.toDouble / outerIterations))
    pr.println("maxTempVars : "+maxTempVars)    
  }
  
  def dumpOutputs(pr : PrintWriter) {
    pr.println("########## Outputs ############")
    pr.println(output)
    pr.flush()       
  }
  
  def dumpMinimizationStats(pr : PrintWriter) {
    pr.println("########## Lower Bounds ############")
    lowerBounds.foreach((pair) => {
      val (fd,lbMap) = pair
      pr.print(fd.id +": \t")
      lbMap.foreach((entry) => {
        pr.print("("+entry._1+"->"+entry._2+"), ")
      })
      pr.print("\t Test results: "+lowerBoundsOutput(fd))
      pr.println()
    })        
    pr.flush()       
  }
  
  def dumpFarkasStats(pr : PrintWriter) ={
    pr.println("### Farkas solver stats ####")
    pr.println("Disjunct Explorations : "+innerIterations)
    pr.println("Total retries: "+retries)    
    //inner iteration statistics
    pr.println("avgFarkaSize : "+(cumFarkaSize.toDouble / innerIterations))
    pr.println("maxFarkaSize : "+maxFarkaSize)
    pr.println("avgFarkaTime : "+((cumFarkaTime.toDouble / innerIterations))/1000.0 +"s")
    pr.println("maxFarkaTime : "+(maxFarkaTime)/1000.0+"s")
    pr.println("avgNLSize : "+(cumNLsize.toDouble / innerIterations))
    pr.println("maxNLSize : "+maxNLsize)           
    pr.println("avgDijSize : "+(cumDijsize.toDouble / innerIterations))
    pr.println("maxDijSize : "+maxDijsize)
    pr.println("avgElimvars : "+(cumElimVars.toDouble / innerIterations))
    pr.println("maxElimvars : "+maxElimVars)
    pr.println("avgElimAtoms : "+(cumElimAtoms.toDouble / innerIterations))
    pr.println("maxElimAtoms : "+maxElimAtoms)
    pr.println("avgExploreTime : "+((cumExploreTime.toDouble / innerIterations))/1000.0 +"s")
    pr.println("maxExploreTime : "+maxExploreTime/1000.0+"s")
    pr.println("avgCompatCalls : "+(cumCompatCalls.toDouble / innerIterations))
    pr.println("maxCompatCalls : "+maxCompatCalls)
    pr.println("avgLemmaApps : "+(cumLemmaApps.toDouble / innerIterations))
    pr.println("maxLemmaApps : "+maxLemmaApps)            
  }
  
  def dumpCegisStats(pr : PrintWriter) ={
    pr.println("### Cegis solver stats #### ")
    //inner iteration statistics
    pr.println("# of bounds tried: "+boundsTried)
    pr.println("cegis iterations: "+cegisIterations)
    pr.println("avgTemplateCtrSize : "+(cumTemplateCtrSize.toDouble / cegisIterations))
    pr.println("maxTemplateCtrSize : "+maxTemplateCtrSize)               
  }
}