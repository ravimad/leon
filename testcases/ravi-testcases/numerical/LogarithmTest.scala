import leon.lang.invariantLang._
import leon.annotation._

object LogarithmTest {
  
  @monotonic
  def log(x: Int) : Int = {
    require(x >= 0)    
    if(x <= 1) 0
    else {      
      1 + log(x/2)    
    }    
  } 
  
  /*def logFour(x: Int) : Int = {
    require(x >= 0)
    if(x <= 1) 0 
    else {
      1 + logFour(x/3)
    }
  } ensuring(res => true template((a,b) => time <= a*log(x) + b))*/
  
  /*@monotonic
  def ceillog(x: Int) : Int = {
    require(x >= 0)    
    if(x <= 1) 0
    else {      
      1 + ceillog(x - x/2)          
    }    
  }*/

  /*def mergeSortAbs(x: Int):Int = {
    require(x >=0)    
    if(x <= 1) x
    else {
      val k = x/2   
      mergeSortAbs(k) + mergeSortAbs(x-k)      
    }                	 
  } ensuring(res => true template((a,b) => time <= a*(x*ceillog(x)) + b))*/

  def binarySearchAbs(x: Int, min: Int, max: Int): Int = {
    require(max - min >= 0)
    if (max - min <= 0) 2 //x == min    	
    else {
      val mid = (min + max) / 2
      if (x < mid) {
        binarySearchAbs(x, min, mid) + 5
      } else if (x > mid) {
        binarySearchAbs(x, mid + 1, max) + 7
      } else
        8
    }                	 
  } ensuring(res => true template((a,b) => res <= a*log(max - min) + b))
} 
