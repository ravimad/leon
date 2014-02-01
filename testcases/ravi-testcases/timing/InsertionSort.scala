import scala.collection.immutable.Set
import leon.Utils._

object InsertionSort {
  sealed abstract class List
  case class Cons(head:Int,tail:List) extends List
  case class Nil() extends List

  def size(l : List) : Int = (l match {
    case Nil() => 0
    case Cons(_, xs) => 1 + size(xs)
  })    

//  def mult(x : Int, y : Int) : Int = {
//    require(x >= 0 && y >= 0)
//      if(x == 0 || y == 0) 0
//      else
//    	  mult(x-1,y-1) +  x + y - 1    	  
//  } 

  def sortedIns(e: Int, l: List): List = {   
    l match {
      case Nil() => Cons(e,Nil())
      case Cons(x,xs) => if (x <= e) Cons(x,sortedIns(e, xs)) else Cons(e, l)
    } 
  } ensuring(res => size(res) == size(l) + 1 template((a,b) => time <= a*size(l) +b))

  def sort(l: List): List = (l match {
    case Nil() => Nil()
    case Cons(x,xs) => sortedIns(x, sort(xs))
    
  }) ensuring(res => size(res) == size(l) template((a,b) => time <= a*(size(l)*size(l)) +b))
  //ensuring(res => size(res) == size(l) template((a,b) => time <= a*mult(size(l),size(l)) +b))
}
