import leon.lang.invariantLang._

object ConcatVariationsAbs {
  /*sealed abstract class List
  case class Cons(head: Int, tail: List) extends List
  case class Nil() extends List

  def size(l: List): Int = (l match {
    case Nil() => 0
    case Cons(_, t) => 1 + size(t)
  })*/

  def genL(n: Int): Int = {
    require(n >= 0)
    if (n == 0) 
      2
    else
      4 + genL(n - 1)
  } ensuring (res => true template((a,b) => res <= a*n + b))

  def append(l1: Int, l2: Int): Int = {
    require(l1 >= 0 && l2 >= 0)
    if (l1 == 0)
      3
    else
      append(l1 - 1, l2 + 1) + 5
  } ensuring (res => true template ((a, b) => res <= a * l1 + b))
  
  def f_good(m: Int, n: Int): Int = {
    require(0 <= m && 0 <= n)
    if (m == 0) 2
    else {
      val t1 = genL(n)
      val t2 = f_good(m - 1, n)
      val t3 = append(n, n*(m-1))
      (t1 + t2 + t3 + 6)
    }
    
  } ensuring(res => true template((a,b,c,d) => res <= a*(n*m) + b*n + c*m +d))

  def f_worst(m: Int, n: Int): Int = {
    require(0 <= m && 0 <= n)    
    if (m == 0) 2
    else {
      val t1 = genL(n)
      val t2 = f_worst(m - 1, n)
      val t3 = append(n*(m-1), n)
      (t1 + t2 + t3 + 6)      
    }
    
  } ensuring(res => true template((a,c,d,e,f) => res <= a*((n*m)*m)+c*(n*m)+d*n+e*m+f)) 
}
