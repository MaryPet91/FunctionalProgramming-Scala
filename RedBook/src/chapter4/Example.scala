package chapter4

object Example {
  
  def failingFn (i: Int) : Int = {
    val y: Int = throw new Exception("fail")
    try{
      val x = 42 + 5
      x + y
    }catch{
      case e: Exception => 43
    }
  }
  
  def failingFn2 (i: Int): Int = {
    try{
      val x = 45 + 5
      x + ((throw new Exception("fail!")): Int)
    }catch{
      case e: Exception => 43
    }
  }
  
  def mean (seq: Seq[Double]) : Double = 
    if(seq.isEmpty)
      throw new Exception("mean of empty list!")
    else
      seq.sum / seq.length
  
  def mean_1 (seq: IndexedSeq[Double], onEmpty: Double): Double = 
    if (seq.isEmpty)
      onEmpty
    else
      seq.sum / seq.length
}