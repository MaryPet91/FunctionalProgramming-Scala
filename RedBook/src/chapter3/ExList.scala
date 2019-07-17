package chapter3

object ExList {
  
  def main(arg: Array[String]): Unit = {

  val x = List(1,2,3,4,5) match {
  case Cons(x, Cons(2, Cons(4, _))) => x //MatchError
  case Nil => 42 //MatchError
  case Cons(x,Cons(y, Cons(3, Cons(4,_)))) => x+y //Matches 3 -> is the first match
  case Cons(h,t) => h + List.sum(t)//Matches 15
  case _ => 101 //Matches 101
  } 
  //println("val x: "+ x + "\n")
    
  val listInt : List[Int] = List(1,2,3,4,5,6,7,8,9,10)
  val listInt1 : List[Int] = List(11,12,13,14,15)
  val listDouble : List[Double] = List(1.0,2.0,3.0,4.0,5.0,6.0,7.0)
/*  println("METHOD sum: "+List.sum(listInt)+"\n")
  println("METHOD product: "+List.product(listDouble)+"\n")
  println("METHOD tail: "+List.tail(listInt)+"\n")
  println("METHOD head: "+List.head(listInt)+"\n")
  println("METHOD setHead: "+List.setHead(listInt,1)+"\n")
  println("METHOD drop: "+List.drop(listInt,3)+"\n")
  println("METHOD dropWhile: "+List.dropWhile(listInt, (x: Int) => x < 6)+"\n")
  println("METHOD append: "+List.append(listInt,listInt1)+"\n")
  println("METHOD init: "+List.init(listInt)+"\n")
  println("METHOD dropWhileTypeInference: "+ List.dropWhileTypeInference(listInt)(x => x <=5)+"\n")
  println("METHOD dropWhileTypeInference: "+ List.dropWhileTypeInference(listInt)(x => (x%2) != 0)+"\n")
  
  var i = List.foldRight(List(1,2,3), Nil: List[Int]) (Cons(_,_))
  println("METHOD foldRight: "+ i +"\n")
  
  println("METHOD sum2 (with foldRight): "+ List.sum2(listInt) +"\n")
  println("METHOD sum3 (with foldLeft): "+ List.sum3(listInt) +"\n")
  println("METHOD product2 (with foldRight): "+ List.product2(listDouble) +"\n")
  println("METHOD product3 (with foldLeft): "+ List.product3(listDouble) +"\n")
  println("METHOD length (with foldRight): "+ List.length(listInt) +"\n") 
  println("METHOD length (with foldLeft): "+ List.length2(listInt) +"\n") 
  println("METHOD reverse (with foldLeft): "+ List.reverse(listInt) +"\n") 
  println("METHOD appendFoldRight: "+List.appendFoldRight(listInt,listInt1)+"\n")
  println("METHOD add: "+List.add(listInt,1)+"\n")
  println("METHOD map: "+List.map(listInt)(_* 2)+"\n")
  println("METHOD filter: "+List.filter(listInt)(_ %2 == 0)+"\n")
  println("METHOD filterNumOdd: "+List.filterNumOdd(listInt)+"\n")
  println("METHOD flatMap: "+List.flatMap(listInt)(i => List(i,i))+"\n")
  println("METHOD filterMap: "+List.filterMap(listInt)(_ %2 == 0)+"\n")
  println("METHOD addNumLists: "+List.addNumLists(listInt,listInt1)+"\n")
  println("METHOD zipWith: "+List.zipWith(listInt,listInt1)(_+_)+"\n")*/
    println("METHOD dropWhile: "+List.dropWhile(List(1,2,3,4,5,6,7,8,9,10), (x: Int) => x < 6)+"\n")

  }
  
  
}