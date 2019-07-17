package chapter1

object RT {
  
  def main(args: Array[String]) : Unit = {
    println("String ... ")
    var x = "Hello, World"
    println(x)
    var r1 = x.reverse
    println(r1)
    var r2 = x.reverse
    println(r2)
    r1 = "Hello, World".reverse
    r2 = "Hello, World".reverse
    println("r1: "+ r1 + " r2:" + r2)
    println("StringBuilder ...")
    var xx = new StringBuilder("Hello")
    val yy = xx.append(", World")
    r1 = yy.toString
    r2 = yy.toString
    println("r1: "+ r1 + " " + "r2:" + r2)
    
    xx = new StringBuilder("Hello")
    r1 = xx.append(", World").toString
    r2 = xx.append (", World").toString
    println("r1: "+ r1 + " " + "r2:" + r2)
    
    
  }
}