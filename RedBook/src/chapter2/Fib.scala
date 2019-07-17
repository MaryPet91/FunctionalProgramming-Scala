package chapter2

object Fib {
  
  def fib(n: Int): Int = {
    @annotation.tailrec
    def loop(n: Int, prev: Int, next: Int) : Int =
     if(n==0) 
       prev
     else 
       loop(n-1, next, prev+next)
   
    loop(n,0,1)
  }
  
  def main(args: Array[String]): Unit = {
    println("Fib 1: " +fib(1))
    println("Fib 6: " +fib(6))
  }
}