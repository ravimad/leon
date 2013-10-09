/* Copyright 2009-2013 EPFL, Lausanne */

package leon

case class LeonFatalError() extends Exception

case  class NotImplementedException(msg: String) extends Exception(msg)

case  class IllegalStateException(msg: String) extends Exception(msg)
