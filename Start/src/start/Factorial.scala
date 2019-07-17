package chapter1

import scala.math.BigInt.int2bigInt

class Factorial {
  
  def factorial (n:BigInt): BigInt = {
    if(n==0)
        1
    else
      n*factorial(n-1)
  }
  
}

object CalculateFactorial {
   def main(args: Array[String]): Unit = {
    var fact = new Factorial()
    var f = fact.factorial(30)
    println("Factorial of 30 is "+ f)
  }
}