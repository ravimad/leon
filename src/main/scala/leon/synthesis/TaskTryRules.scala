/* Copyright 2009-2014 EPFL, Lausanne */

package leon
package synthesis

import synthesis.search._

case class TaskTryRules(p: Problem) extends AOOrTask[Solution] {
  override def toString = p.toString
}
