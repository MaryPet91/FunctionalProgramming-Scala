package chapter2

object Factorial {
  
  //factorial not tail recursion
  def factorialNTR (n:Int): Int = {
    def loop(n: Int): Int = 
      if(n == 0) 
        1
      else
        n*loop(n-1)
     loop(n)
  }
  
  //factorial tail recursion
  def factorialTR (n: Int): Int = {
    @annotation.tailrec
    def loop(n: Int, acc: Int): Int = 
      if(n <= 0)
        acc
      else
        loop(n-1, n*acc)
     loop(n, 1)
  }
  
  def main(args: Array[String]): Unit = {
    println("FactorialNTR of 6 is " + factorialNTR(6))
    println("FactorialTR of 6 is " + factorialTR(6))   
  }
}
