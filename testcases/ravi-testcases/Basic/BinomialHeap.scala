/** 
 * Okasaki3_2
 * 
 * Based on the chapter 3.2 of Okasaki's paper Purely Functional Data Structure
 * Implements the "Binomial Heap" data structure described in the chapter.
 * 
 * @author Florian Briant
 **/

import leon.Utils._
import leon.Annotations._

object BinomialHeap {    
  sealed abstract class BinomialTree
  case class Node(rank: Int, elem: Element, children: BinomialHeap) extends BinomialTree   
  
  sealed abstract class ElementAbs
  case class Element(n: Int) extends ElementAbs
  
  sealed abstract class BinomialHeap
  case class ConsHeap(head: BinomialTree, tail: BinomialHeap) extends BinomialHeap
  case class NilHeap extends BinomialHeap
  
  sealed abstract class List
  case class NodeL(head: BinomialHeap, tail: List) extends List
  case class NilL() extends List
  
  sealed abstract class OptionalTree
  case class Some(t : BinomialTree) extends OptionalTree
  case class None extends OptionalTree
  
  /* Lower or Equal than for Element structure */
  private def leq(a: Element, b: Element) : Boolean = {
    a match {
      case Element(a1) => {
        b match {
          case Element(a2) => {
            if(a1 <= a2) true 
            else false
          }
        }
      }
    }
  }
  
  /* isEmpty function of the Binomial Heap */
  def isEmpty(t: BinomialHeap) = t match {
    case ConsHeap(_,_) => false
    case NilHeap() => true
  }
  
  /* Helper function to determine rank of a BinomialTree */
  def rank(t: BinomialTree) : Int =  t match {
    case Node(r, _, _) => r
  }
  
  /* Helper function to get the root element of a BinomialTree */
  def root(t: BinomialTree) : Element = t match {
    case Node(_, e, _) => e
  }

  /* Helper function which tell if a binomial tree is valid */
  //  private def isBinomialChildrenValid(pare: Element, ch: BinomialHeap) : Boolean = ch match {
  //    case ConsHeap(t1@Node(r, e, _), tail@ConsHeap(t2,_)) => rank(t2) == r - 1 && leq(pare,e) && isBinomialTreeValid(t1) && isBinomialChildrenValid(pare, tail)
  //    case ConsHeap(t1@Node(r, e, _), NilHeap()) => leq(pare,e) && isBinomialTreeValid(t1)
  //    case NilHeap() => true
  //  }
  //  private def isBinomialTreeValid(l: BinomialTree) : Boolean = l match {
  //    case Node(r, e, ch@ConsHeap(t, _)) => rank(t) == r - 1 && isBinomialChildrenValid(e, ch)
  //    case Node(_, _, NilHeap()) => true
  //  }
  //  
  //  // Helper function which tell if a binomial heap is valid 
  //  private def isBinomialHeapValid(h: BinomialHeap) : Boolean = h match {
  //      case ConsHeap(e, tail@ConsHeap(e2, _)) => rank(e) < rank(e2) && isBinomialTreeValid(e) && isBinomialHeapValid(tail)
  //      case ConsHeap(e, NilHeap()) => isBinomialTreeValid(e) 
  //      case NilHeap() => true
  //  }

  def checkChildren(index: Int, origRank: Int, origElem: Element, children: BinomialHeap): Boolean = children match {
    case ConsHeap(h, t) => h match {
      case Node(r1, e1, _) => leq(origElem, e1) && r1 == origRank - index - 1 && isBinomialTreeValid(h) && checkChildren(index + 1, origRank, origElem, t)
    }
    case NilHeap() => index == origRank
  }
  private def isBinomialTreeValid(l: BinomialTree) : Boolean = l match {
    case Node(r, e, c) => {      
      checkChildren(0, r, e, c)
    }
  }  
  def isBinomialHeapValidStep(h1: BinomialHeap, oldR: Int) : Boolean = h1 match {
      case ConsHeap(e, tail) => oldR < rank(e) && isBinomialHeapValidStep(tail, rank(e)) && isBinomialTreeValid(e)
      case NilHeap() => true
  }
  // Helper function which tell if a binomial heap is valid 
  private def isBinomialHeapValid(h: BinomialHeap) : Boolean ={    
    isBinomialHeapValidStep(h, -1)
  }
  
  /*private def rankIncrease(h: BinomialHeap) : Boolean = {
    h match {
      case ConsHeap(tr, tail) => {        
        tail match{ 
          case ConsHeap(tr2, tail2) => tr < tr2 && rankIncrease(tail)
          case NilHeap => true
        }        
        rank(tr) < heapRank(tail) && rankIncrease(tail)
      } 
      case NilHeap() => true
    }
  }*/
  
  /* Linking trees of equal ranks depending on the root element */ 
  def link(t1: BinomialTree, t2: BinomialTree) : BinomialTree = {
    require(isBinomialTreeValid(t1) && isBinomialTreeValid(t2) && rank(t1) == rank(t2))
    t1 match {
        case Node(r, x1, c1) => {
          t2 match {
            case Node(_, x2, c2) => {
              if (leq(x1,x2)) {
                  Node(r+1, x1, ConsHeap(t2, c1))  
              } else {
                  Node(r+1, x2, ConsHeap(t1, c2))
              }
            }
          }
        }
    }
  } ensuring(res => isBinomialTreeValid(res))

  /* Insert a tree into a binomial heap. The tree should be correct in relation to the heap */
  def insTree(t: BinomialTree, h: BinomialHeap) : BinomialHeap = {
    require(isBinomialTreeValid(t) && isBinomialHeapValid(h))// && isTreeValidForInsert(t,h))    
    h match {
      case ConsHeap(head, tail) =>  {
        if (rank(t) < rank(head)) {
          ConsHeap(t, h)
        }else if (rank(t) > rank(head)) {
          ConsHeap(head, insTree(t,tail))
        } else {
          insTree(link(t,head), tail)
        }
      }
      case NilHeap() => ConsHeap(t, NilHeap())
    }
  } ensuring(res => isBinomialHeapValid(res))

  /*def mult(x : Int, y : Int) : Int = {
      if(x == 0 || y == 0) 0
      else
    	  mult(x-1,y-1) + x + y -1
  }*/

  /* Merge two heaps together */
  /*def merge(h1: BinomialHeap, h2: BinomialHeap) : BinomialHeap = {
    //require(isBinomialHeapValid(h1) && isBinomialHeapValid(h2))
    require(rankIncrease(h1) && rankIncrease(h2))
    h1 match {
      case ConsHeap(head1, tail1) => {
        h2 match {
          case ConsHeap(head2, tail2) => {
            if (rank(head1) < rank(head2)) {
              ConsHeap(head1, merge(tail1, h2))
            } else if (rank(head2) < rank(head1)) {
              ConsHeap(head2, merge(h1, tail2))
            } else {
              insTree(link(head1, head2), merge(tail1, tail2))
            }
          }
          case NilHeap() => h1
        }
      }
      case NilHeap() => h2
    }
  } ensuring(res => (treeNum(res)<=treeNum(h1)+treeNum(h2) && rankIncrease(res) && heapRank(res)>=min(heapRank(h1),heapRank(h2))) 
      template((b,c,d) => time <= b*treeNum(h1) + c*treeNum(h2) + d))*/
  //ensuring(res => treeNum(res)<=treeNum(h1)+treeNum(h2) template((a,b,c,d) => time <= a*mult(treeNum(h1),treeNum(h2)) + b*treeNum(h1) + c*treeNum(h2) + d))
  //((((((2 * res4._2) + (-2 * treeNum2(h2))) + (-1 * treeNum2(h1))) + (-74 * mult2(treeNum2(h1), treeNum2(h2)))) + -6) <= 0)
  //ensuring(res => isBinomialHeapValid(res))

  /* Merge two heaps together */
  def merge(h1: BinomialHeap, h2: BinomialHeap): BinomialHeap = {
    require(isBinomialHeapValid(h1) && isBinomialHeapValid(h2))    
    h1 match {
      case ConsHeap(head1, tail1) => {
        h2 match {
          case ConsHeap(head2, tail2) => {
            if (rank(head1) < rank(head2)) {
              ConsHeap(head1, merge(tail1, h2))
            } else if (rank(head2) < rank(head1)) {
              ConsHeap(head2, merge(h1, tail2))
            } else {
              mergeWithCarry(link(head1, head2), tail1, tail2)
            }
          }
          case NilHeap() => h1
        }
      }
      case NilHeap() => h2
    }
  }  ensuring(res => isBinomialHeapValid(res))

  /* Helper function to validate the input tree of insTree */
  private def isValidCarry(t: BinomialTree, h: BinomialHeap) = h match {
    case ConsHeap(head, tail) => rank(t) <= rank(head) 
    case NilHeap() => true
  }
  
  def mergeWithCarry(t: BinomialTree, h1: BinomialHeap, h2: BinomialHeap): BinomialHeap = {
    require(isBinomialTreeValid(t) && isBinomialHeapValid(h1) && isBinomialHeapValid(h2) && isValidCarry(t,h1) && isValidCarry(t,h2))    
    t match {
      case Node(r, _, _) => {
        h1 match {
          case ConsHeap(head1, tail1) => {
            h2 match {
              case ConsHeap(head2, tail2) => {
                if (rank(head1) < rank(head2)) {
                  
                  if (rank(t) < rank(head1))
                    ConsHeap(t, ConsHeap(head1, merge(tail1, h2)))
                  else 
                    mergeWithCarry(link(t, head1), tail1, h2)                                     
                } else if (rank(head2) < rank(head1)) {
                  
                  if (rank(t) < rank(head2))
                    ConsHeap(t, ConsHeap(head2, merge(h1, tail2)))
                  else
                    mergeWithCarry(link(t, head2), h1, tail2)                   
                  
                } else {
                  ConsHeap(t, mergeWithCarry(link(head1, head2), tail1, tail2))
                }
              }
              case NilHeap() => {              
                insTree(t, h1)
              }
            }
          }
          case NilHeap() => insTree(t, h2)
        }
      }
    }
  } ensuring (res => isBinomialHeapValid(res))

  /* Helper function to define ensuring clause in removeMinTree */
  private def isRemovedMinTreeValid(x : (OptionalTree, BinomialHeap)) : Boolean = {
    val (opt,h) = x
    opt match {	    
		case Some(t) => isBinomialTreeValid(t) && isBinomialHeapValid(h)
		case _ => isBinomialHeapValid(h)		  
	  }
  }

  //Auxiliary helper function to simplefy findMin and deleteMin  
  def removeMinTree(h: BinomialHeap): (OptionalTree, BinomialHeap) = {
    require(!isEmpty(h) && isBinomialHeapValid(h)) 
    h match {
      case ConsHeap(head, NilHeap()) => (Some(head), NilHeap())
      case ConsHeap(head1, tail1) => {
        val (opthead2, tail2) = removeMinTree(tail1)
        opthead2 match {
          case None() => (Some(head1), tail1)
          case Some(head2) =>
            if (leq(root(head1), root(head2))) {
              (Some(head1), tail1)
            } else {
              (Some(head2), ConsHeap(head1, tail2))
            }
        }
      }
      case _ => (None(), NilHeap())
    }
  } ensuring(res => isRemovedMinTreeValid(res))
  
//  def revRec(l1: BinomialHeap, l2: BinomialHeap): BinomialHeap = (l1 match {
//    case NilHeap() => l2
//    case ConsHeap(x, xs) => revRec(xs, ConsHeap(x, l2))
//
//  }) 
//
//  def rev(l: BinomialHeap): BinomialHeap = {
//    revRec(l, NilHeap())    
//  } 
  
  // Discard the minimum element of the extracted min tree and put its children back into the heap 
  def deleteMin(h: BinomialHeap) : BinomialHeap = {
	  require(!isEmpty(h) && isBinomialHeapValid(h)) 
	  val (min, ts2) = removeMinTree(h)
	  min match {	    
		case Some(Node(_,_,ts1)) => merge(ts1, ts2)
		case _ => h		  
	  }
  } ensuring(res => isBinomialHeapValid(h))

  /* TEST AREA */
  
  /*def BTtest1() : Boolean = isBinomialTreeValid(Node(1, Element(2), ConsHeap(Node(0, Element(3), NilHeap()),NilHeap())))*/ 
  /*def BTtest2() : Boolean = isBinomialTreeValid(Node(0, Element(3), NilHeap()))*/
  /*def BTtest3() : Boolean = isBinomialTreeValid(Node(1, Element(975), ConsHeap(Node(0, Element(976), NilHeap()), NilHeap())))*/
  /*def BHtest1() : Boolean = isBinomialHeapValid(ConsHeap(Node(0, Element(0), Nil()),ConsHeap(Node(1, Element(0), ConsHeap(Node(0, Element(1), NilHeap()),NilHeap())),NilHeap())))*/
  /*def testInsTree1() : BinomialHeap = insTree(Node(0, Element(2), NilHeap()), ConsHeap(Node(1, Element(3), ConsHeap(Node(0, Element(4), NilHeap()),NilHeap())),NilHeap()))*/
  /*def testInsTree2() : BinomialHeap = insTree(Node(0, Element(2), NilHeap()), ConsHeap(Node(0, Element(1), NilHeap()), NilHeap()))*/
  
  /*def testMerge1() : BinomialHeap = merge(ConsHeap(Node(0, Element(1), NilHeap()), NilHeap()), ConsHeap(Node(1, Element(3), ConsHeap(Node(0, Element(4), NilHeap()),NilHeap())),NilHeap()))*/
  /*def testMerge2() : BinomialHeap = merge(ConsHeap(Node(1, Element(2), ConsHeap(Node(0, Element(4), NilHeap()),NilHeap())),NilHeap()), ConsHeap(Node(0, Element(3), NilHeap()), NilHeap()))*/
  /*def testMerge3() : BinomialHeap = merge(ConsHeap(Node(0, Element(4), NilHeap()), NilHeap()), ConsHeap(Node(0, Element(1), NilHeap()), NilHeap()))*/
  
  /*def testRemoveMinTree1() : (BinomialTree, BinomialHeap) = removeMinTree(ConsHeap(Node(0, Element(1), NilHeap()), NilHeap()))
  def testRemoveMinTree2() : (BinomialTree, BinomialHeap) = removeMinTree(testMerge1())
  def testRemoveMinTree3() : (BinomialTree, BinomialHeap) = removeMinTree(testMerge2())*/
  
  /*def testFindMin1() : Element = findMin(testMerge1())
  
  def testConcat : List = concat(ConsHeap(Node(0, Element(0), NilHeap()), NilHeap()), ConsHeap(Node(0, Element(0), NilHeap()), ConsHeap(Node(0, Element(0), NilHeap()), NilHeap())))
  
  def testRev: List = rev(ConsHeap(Node(0, Element(0), NilHeap()), ConsHeap(Node(1, Element(1), NilHeap()),ConsHeap(Node(2, Element(2), NilHeap()),NilHeap()))))*/
  
  /*def testDeleteMin1() = deleteMin(testMerge1())*/
  
  /*def sortTest() = bubbleSort(NodeL(testMerge1(), NodeL(testMerge2(), NilL())), true)
  def sortTest2() = bubbleSort(NodeL(testMerge1(), NodeL(testMerge2(), NilL())), false)*/
 
  
  
}
