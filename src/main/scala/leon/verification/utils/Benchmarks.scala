package leon.verification.utils

import leon._
import leon.purescala.Definitions._
import leon.purescala.Trees._
import leon.purescala.TreeOps._
import leon.solvers.z3._
import leon.solvers.Solver

import java.util.Date
import java.text.SimpleDateFormat

import sys.process._

import java.io.File

object Benchmarks extends App {

  // Parse arguments
  val (options, others) = args.partition(_.startsWith("--"))

  println("# Date: "+new SimpleDateFormat("dd.MM.yyyy HH:mm:ss").format(new Date()))
  println("# Git tree: "+("git log -1 --format=\"%H\"".!!).trim)
  println("# Options: "+options.mkString(" "))


  abstract class TableLayout {
    def infoSep: String
    def infoFooter: String
    def infoHeader: String
    def infoLine(file: String, f: String, kind: String, value: String, ts: String) : String
  }

  object UnicodeTableLayout extends TableLayout {
    val infoSep    : String = "╟" + ("┄" * 89) + "╢"
    val infoFooter : String = "╚" + ("═" * 89) + "╝"
    val infoHeader : String = "  ┌────────────┐\n" +
                              "╔═╡ Benchmarks ╞" + ("═" * 74) + "╗\n" +
                              "║ └────────────┘" + (" " * 74) + "║"

    def infoLine(file: String, f: String, kind: String, value: String, ts: String) : String = {
      "║ %-30s %-33s %10s %2s %5s ms ║".format(
        file,
        f,
        kind,
        value,
        ts)
    }
  }

  object PlainTableLayout extends TableLayout {
    val infoSep    : String = ""
    val infoFooter : String = ""
    val infoHeader : String = ""

    def infoLine(file: String, f: String, kind: String, value: String, ts: String) : String = {
      "%-30s, %-33s, %10s, %2s, %5s".format(
        file,
        f,
        kind,
        value,
        ts)
    }
  }

  val layout: TableLayout = if (options.contains("--plain")) {
    PlainTableLayout
  } else {
    UnicodeTableLayout
  }

  println(layout.infoHeader)

  var nSuccessTotal, nInnapTotal, nDecompTotal, nAltTotal = 0
  var tTotal: Long = 0

  val ctx = leon.Main.processOptions(new SilentReporter, args)

  val timeoutMs: Long = ctx.options.find(_.name == "timeout") match {
    case Some(vo @ LeonValueOption(_, timeout)) =>
      vo.asInt(ctx).map(_*1000L).getOrElse(-1)
    case _ =>
      -1
  }

  for (file <- ctx.files) {
    val innerCtx = ctx.copy(files = List(file))

    val pipeline = leon.plugin.ExtractionPhase andThen (if (options contains "--xlang") {
        xlang.XlangAnalysisPhase
     } else {
        verification.AnalysisPhase
     })

    val vr = pipeline.run(innerCtx)(file.getPath :: Nil)

    for (vc <- vr.conditions) {
      val timeStr = vc.time match {
        case Some(d) =>
          tTotal += (d*1000).toLong
          "%5d".format((d*1000).toLong)
        case None =>
          if (timeoutMs > 0) {
            tTotal += timeoutMs
          }
          "%5d".format(timeoutMs)
      }

      val value = vc.value.map(if (_) "V" else "I").getOrElse("?")

      println(layout.infoLine(file.getName().toString, vc.funDef.id.toString, vc.kind.toString, value, timeStr))
    }

    println(layout.infoSep)

  }

  println(layout.infoLine("TOTAL", "", "", "", "%5d".format(tTotal)))

  println(layout.infoFooter)

  //val infoHeader2 : String = "  ┌────────────┐\n" +
  //                           "╔═╡ Timers     ╞" + ("═" * 71) + "╗\n" +
  //                           "║ └────────────┘" + (" " * 71) + "║"

  //println(infoHeader2)
  //for ((name, sw) <- StopwatchCollections.getAll.toSeq.sortBy(_._1)) {
  //  println("║ %-70s %10s ms ║".format(name, sw.getMillis))
  //}
  //println(infoFooter)
}
