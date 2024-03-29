package chapter1.coffee

case class Charge(cc: CreditCard, amount: Double) {
  
  def combine(other: Charge): Charge = 
    if (cc == other.cc)
       Charge(cc, amount + other.amount)
   else
     throw new Exception("Can't combine charges to different cards")
  
    def coalesce(charges: List[Charge]) : List[Charge] = 
      charges.groupBy(_.cc).values.map(_.reduce(_ combine _)).toList
}

