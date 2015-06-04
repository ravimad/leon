import leon.lang.invariantLang._

object AmortizedQueue {
  /*sealed abstract class List
  case class Cons(head : Int, tail : List) extends List
  case class Nil() extends List

  case class Queue(front : List, rear : List) 

  def size(list : List) : Int = (list match {
    case Nil() => 0
    case Cons(_, xs) => 1 + size(xs)
  }) 
  
  def sizeList(list : List) : Int = (list match {
    case Nil() => 0
    case Cons(_, xs) => 1 + sizeList(xs)
  }) ensuring(res => res >= 0 template((a,b) => time <= a*size(list) + b))
  
  def qsize(q : Queue) : Int = size(q.front) + size(q.rear)

  def asList(q : Queue) : List = concat(q.front, reverse(q.rear))

  def concat(l1 : List, l2 : List) : List = (l1 match {
    case Nil() => l2
    case Cons(x,xs) => Cons(x, concat(xs, l2))
    
  }) ensuring (res => size(res) == size(l1) + size(l2) template((a,b,c) => time <= a*size(l1) + b))

  def isAmortized(q : Queue) : Boolean = sizeList(q.front) >= sizeList(q.rear)
  def isEmpty(queue : Queue) : Boolean = queue match {
    case Queue(Nil(), Nil()) => true
    case _ => false
  }*/

  def concat(l1: Int, l2: Int): Int = {
    require(l1 >= 0 && l2 >= 0)
    if (l1 == 0)
      3
    else
      concat(l1 - 1, l2 + 1) + 5
  } ensuring (res => true template ((a, b) => res <= a * l1 + b))

  def reverseRec(l1: Int, l2: Int): Int = {
    require(l1 >= 0 && l2 >= 0)
    if (l1 == 0)
      3
    else {
      reverseRec(l1 - 1, l2 + 1) + 6
    }
  } ensuring (res => true template ((a, b) => res <= a * l1 + b))

  def reverse(l: Int): Int = {
    require(l >= 0)
    reverseRec(l, 0) + 1
  } ensuring (res => true template ((a, b) => res <= a * l + b))

  def create(front: Int, rear: Int): Int = {
    require(front >= 0 && rear >= 0)
    if (rear <= front)
      4
    else {
      val t1 = reverse(rear)
      val t2 = concat(front, rear)
      t1 + t2 + 7
    }
  }

  def enqueue(q: Int, front: Int, rear: Int): Int = {
    require(q == front + rear && q >= 0 && front >= 0 && rear >= 0)
    create(front, rear) + 5
  } ensuring (res => true template ((a, b) => res <= a * q + b))

  def dequeue(q: Int, front: Int, rear: Int): Int = {
    require(q == front + rear && q >= 1 && front >= rear && rear >= 0)
    if (front >= 1) {
      create(front - 1, rear) + 4
    } else {
      //since front should be greater than rear, here rear should be 0 as well
      5
    }
  } ensuring (res => true template ((a, b) => res <= a * q + b))

  def removeLast(l: Int): Int = {
    require(l >= 1)
    if (l == 1) {
      4
    } else {
      removeLast(l - 1) + 6
    }
  } ensuring (res => true template ((a, b) => res <= a * l + b))

  def pop(q: Int, front: Int, rear: Int): Int = {
    require(q == front + rear && q >= 1 && front >= rear && rear >= 0)
    if (rear >= 1) {
      3
    } else {
      val t1 = removeLast(front)
      t1 + 5
    }
  } ensuring (res => true template ((a, b) => res <= a * q + b))
}
