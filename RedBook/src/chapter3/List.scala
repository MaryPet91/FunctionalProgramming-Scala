package chapter3

sealed trait List[+A] 
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]
  
object List { //companion object
  
  def sum (l: List[Int]) : Int = l match {
    case Nil => 0
    case Cons(h,t) => h + sum(t)
  }
  
  def product (l: List[Double]) : Double = l match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(h,t) => h * product(t)
  }
  
  def apply[A] (l: A*) : List[A] = //variadic function -> create the list
    if(l.isEmpty)
      Nil
    else 
      Cons(l.head, apply(l.tail: _*))
  
  def tail[A] (l: List[A]): List[A] = l match { //remove head
      case Nil => Nil
      case Cons(_,t) => t
  }
  
  def head[A] (l: List[A]) : A = l match { //returns head
    case Nil => sys.error("List empty")
    case Cons(h,_) => h
  }
  
  def setHead[A] (l: List[A], f: A): List[A] = l match {
    case Nil => Nil
    case Cons(_,t) => Cons(f,t)
  }
  
  def drop[A] (l: List[A], n: Int) : List[A] = {
    if(n<=0) 
      l
    else l match {
      case Nil => Nil
      case Cons(_,t) => drop(t,n-1)
    }
  }
  
  def dropWhile[A] (l: List[A], f: A => Boolean) : List[A] = l match {
      case Nil => Nil
      case Cons(h,t) if f(h) => dropWhile(t, f) 
      case _ => l
  }
  
  def dropWhileTypeInference[A] (l: List[A]) (f: A => Boolean) : List[A] = l match {
    case Cons(h,t) if f(h) => dropWhileTypeInference(t)(f)
    case _ => l
  }
  
  def append[A] (a1: List[A], a2: List[A]) : List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t,a2))
  }
  
  def init[A] (l: List[A]) : List[A] = l match { //remove last element of list
    case Nil => sys.error("List empty")
    case Cons(_,Nil) => Nil
    case Cons(h,t) => Cons(h,init(t))
  }
  
  def reverse[A] (l: List[A]) : List[A] = foldLeft(l, List[A]())((t,h) => Cons(h,t))
  
  def foldRight[A,B] (l: List[A], b: B) (f: (A,B) => B) : B = l match {
    case Nil => b //base element
    case Cons(h,t) => f(h, foldRight(t, b)(f))
  }
  
  @annotation.tailrec
  def foldLeft[A,B] (l: List[A], b: B) (f: (B,A) => B) : B = l match {
    case Nil => b
    case Cons(h,t) => foldLeft(t, f(b,h))(f)   
  }
          
  def sum2 (l: List[Int]) = foldRight(l, 0)(_+_)
  
  def sum3 (l: List[Int]) = foldLeft(l, 0)(_+_)
  
  def product2 (l: List[Double]) = foldRight(l, 1.0)(_*_)
  
  def product3 (l: List[Double]) = foldLeft(l, 1.0)(_*_)
  
  def length [A] (l: List[A]) : Int =  foldRight(l, 0)((_,len) => len + 1)
  
  def length2[A] (l: List[A]) : Int =  foldLeft(l, 0)((len,_) => len + 1)
  
  def foldRightViaFoldLeft[A,B](l: List[A], z: B)(f: (A,B) => B): B =
    foldLeft(reverse(l), z)((b,a) => f(a,b))
    
  def foldRightViaFoldLeft_1[A,B](l: List[A], z: B)(f: (A,B) => B): B =
    foldLeft(l, (b:B) => b)((g,a) => b => g(f(a,b)))(z)
    
  def foldLeftViaFoldRight[A,B](l: List[A], z: B)(f: (B,A) => B): B =
    foldRight(l, (b:B) => b)((a,g) => b => g(f(b,a)))(z)
  
  def appendFoldRight[A] (l1: List[A], l2: List[A]) : List[A] = 
    l1 match {
    case Nil => l2
    case Cons(_,_) => foldRight(l1,l2)(Cons(_,_))
  }
  
  def concat[A](l: List[List[A]]): List[A] = foldRight(l, Nil:List[A])(append)
  
  def add (l: List[Int], num: Int) : List[Int] = l match {
    case Nil => Nil
    case Cons(h,t) => Cons(h+num,add(t,num))
  }
  
  def addFoldRight (l: List[Int], num: Int) : List[Int] = 
    foldRight(l, List[Int]())((h,t) => Cons(h+1,t))
  
  def doubleToString (l: List[Double]) : List[String] = l match {
    case Nil => Nil
    case Cons(h,t) => Cons(h.toString(),doubleToString(t))
  }
  
  def doubleToStringFoldRight (l: List[Double]) : List[String] =
    foldRight(l, List[String]())((h,t) => Cons(h.toString(),t))
   
  def map[A,B] (l: List[A]) (f: A => B): List[B] = 
    foldRight(l, List[B]())((h,t) => Cons(f(h),t))
  
  def filter[A] (l: List[A]) (f: A => Boolean): List[A] =
    foldRight(l, List[A]())((h,t) => if (f(h)) Cons(h,t) else t)
 
  def filterNumOdd (l: List[Int]): List[Int] = filter(l)(_%2 == 0) 
  
  def flatMap[A,B] (l: List[A]) (f: A => List[B]) : List[B] =  
     foldRight(l, List[B]())((h,t) => append(f(h), t))
     
  def filterMap[A] (l: List[A]) (f: A => Boolean): List[A] =
      flatMap(l)(a => if (f(a)) List(a) else Nil)
  
  def addNumLists (l1: List[Int], l2: List[Int]) : List[Int] = (l1,l2) match {
    case (Nil,_) => Nil
    case (_,Nil) => Nil
    case (Cons(h1,t1),Cons(h2,t2)) => Cons(h1+h2, addNumLists(t1, t2))
  }
  
  def zipWith[A] (l1: List[A], l2: List[A])(f: (A,A) => A) : List[A] = (l1,l2) match {
    case (Nil,_) => Nil
    case (_,Nil) => Nil
    case (Cons(h1,t1),Cons(h2,t2)) => Cons(f(h1,h2), zipWith(t1, t2)(f))
  }
}