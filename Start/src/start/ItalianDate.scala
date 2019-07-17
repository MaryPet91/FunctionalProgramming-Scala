package start

import java.util.{Date, Locale}
import java.text.DateFormat
import java.text.DateFormat._

object ItalianDate {
  def main(args: Array[String]){
    var today = new Date
    var date = getDateInstance(LONG, Locale.ITALY);
    println("Today: "+ today)
    println("Date: "+ date.toString)
    print("Date format: ")
    println(date format today)
  }
}

