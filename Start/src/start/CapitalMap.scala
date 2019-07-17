package start

object CapitalMap {
  def main(args: Array[String]):Unit={
    var capital = Map("US"-> "Washington", "France" -> "Paris")
    capital+= ("Japan" -> "Tokyo")
    println("The Capital of the France is "+capital("France"))
    
  }
}