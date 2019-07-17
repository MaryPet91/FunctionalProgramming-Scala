package chapter2

object PolymorphicFunction {
  
  //Monomorphic function
  def findFirstString(ss: Array[String], key: String): Int = {
    @annotation.tailrec
    def loop(n: Int): Int = 
      if (n >= ss.length) -1
      else if (ss(n) == key) n
      else loop(n+1)
      
    loop(0)
  }
  
  //Polymorphic function
  def findFirst[A] (as: Array[A], p: A => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int =
      if (n >= as.length) -1
      else if (p(as(n))) n
      else loop (n+1)
      
    loop(0)
  }
  
  def isSorted[A] (as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean =
      if (n >= as.length-1) true
      else if (!ordered(as(n),as(n+1))) false
      else loop (n+1)
    
    loop(0)
  }
  
  def partial1[A,B,C] (a:A, f: (A,B) => C): B => C = 
    (b: B) => f(a,b)
  
  def curry[A,B,C] (f: (A,B) => C) : A => (B => C) = 
    a => b => f(a,b)
  
  def uncurry[A,B,C] (f: A => B => C): (A,B) => C = // A => B => C ====== A => (B => C)
    (a,b) => f(a)(b)  
    
  def compose[A,B,C] (f: B => C, g: A => B) : A => C =
    a => f(g(a))
        
  def main (args: Array[String]): Unit = {
    println("Method [findFirst] Monomorphic: "+ findFirstString(Array("Pippo","Pluto","Paperino","Minnie","Topolino"), "Minnie"))
    println("Method [findFirst] Polymorphic: "+ findFirst(Array(1,2,3), (x: Int) => x==3))
    println("Method [isSorted] on List A => " + isSorted(Array(1,2,3,4,5),(x: Int, y: Int) => x <= y))
    println("Method [isSorted] on List B => " + isSorted(Array(3,4,1,2,5),(x: Int, y: Int) => x <= y))
  }
   
}
