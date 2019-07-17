package chapter6

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  
  type Rand[+A] = RNG => (A,RNG)
  
  def map[A,B] (s: Rand[A]) (f: A => B) : Rand[B] =
    rng => {
      val (a,rng2) = s(rng)
      (f(a),rng2)
  }
    
  def unit[A] (a: A) : Rand[A] =
    rng => (a, rng)
  
   def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, r1) = ra(rng)
      val (b, r2) = rb(r1)
      (f(a, b), r2)
    }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra, rb)((_, _))

  /*val randIntDouble: Rand[(Int, Double)] =
    both(Int, Double)

  val randDoubleInt: Rand[(Double, Int)] =
    both(double, int)*/
  
   def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))
    
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, r1) = f(rng)
      g(a)(r1) // We pass the new state along
    }
   def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }
   
  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }
  }
  
  def rollDie : Rand[Int] = map(nonNegativeLessThan(6))(_+1)
}