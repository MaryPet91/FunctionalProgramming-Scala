package chapter1.coffee

class Cafe {

	def buyCoffeeOne(cc: CreditCard): Coffee = { //first try
			val cup = new Coffee()
			cc.charge(1.0) //This is a side effect
			cup
	}

	def byCoffeeTwo(cc: CreditCard, p: Payments): Coffee = { //second try
			val cup = new Coffee()
			p.charge(cc,cup.price) //This is a side effect
			cup
	}

	def buyCoffee(cc: CreditCard): (Coffee, Charge) = {
			val cup = new Coffee()
			(cup, Charge(cc, cup.price))
	}

	def buyCoffees(cc: CreditCard, n: Int) : (List[Coffee], Charge) = {
			val purchases: List[(Coffee, Charge)] = List.fill(n)(buyCoffee(cc))
			val (coffees, charges) = purchases.unzip;
	    (coffees, charges.reduce((c1,c2) => c1.combine(c2)))
	}

}

object PayCafe {
	def main(args: Array[String]): Unit = {
			val cc: CreditCard = new CreditCard(1L)
			var cafe = new Cafe
			println("Welcome") 
			/////////////////////////////////////s
			println("Buy with side effects ... ")
			cafe.buyCoffeeOne(cc)
			////////////////////////////////////
			println("Buy without side effects ... ")
			var (cup, charge) = cafe.buyCoffee(cc)
			println("Buy one coffee ... Price: " + charge.amount)  
			var (cups, charge1) = cafe.buyCoffees(cc, 10)
			println("Buy ten coffee ... Price: "+charge1.amount)  
	}

}



