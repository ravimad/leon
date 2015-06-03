import leon.lang.invariantLang._

object SimpleLoop
{
	def s(x: Int) : Int = {
	  if(x < 0)
	    0
	  else 
	    s(x-1) + 1	   
	} ensuring(res => res != 0 template((a, b, c) => a*res + b*x + c <= 0))
	//template((a, b, c) => a*res + b*x + c <= 0)
} 