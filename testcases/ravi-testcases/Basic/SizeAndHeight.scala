import leon.Utils._

object SizeAndHeight
{
	sealed abstract class Tree
  	case class Node(left: Tree, value: Int, right: Tree) extends Tree
  	case class Leaf() extends Tree
  	
  	def size(t: Tree) : Int = {
	  t match {
	  	case Leaf() => 0
	  	case Node(l,x,r) => {
	  	  size(l) + size(r) +1
	  	}
	  }	  
	} ensuring(res => res >= height(t) && res  >= 0)
	//ensuring(res => res != height(t) - 1)
	//ensuring(res => res != height(t) - 1 template((a,b) => (a*res + b*height(t) >= 0 ))) 
	//ensuring(res => res >= height(t) && res  >= 0)	
			
	def height(t: Tree): Int = {
	  t match{
	    case Leaf() => 0
	    case Node(l,x,r) => {
	      val hl = height(l)
	      val hr = height(r)
	      if(hl > hr) hl + 1 else hr + 1	    
	    }
	  }
	} 
	//ensuring(res => true template((a,b) => time <= a*size(t) +b)) 
	//ensuring(res => 0*size())	
} 