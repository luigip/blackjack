package blackjack


// This Main is just a fun tester...

object Main extends App {
  
  implicit val rules = Rules.ruleSetsByName("Las Vegas Strip")
  implicit val strategy = Strategy.BasicStrategy
  var money = 1000
  val stake = 2
  
  val it = new Iterator[Card] {
    var shoe = Shoe.NDeck(8)
    def hasNext = true
    def next() = try shoe.card finally shoe = shoe.next    
  }
 
  for (i <- 1 to 20) {
    val c1 = it.next
    val c2 = it.next
    val c3 = it.next
    money -= stake
    val h = HandNode(Vector(c1, c3), it.shoe, c2, stake = stake, money = money)
    
    val rs = h.traverse
    println("results size =========== " + rs.size)
    
    println("player scored: " + rs.map(_.score).mkString(" and "))
    
    val dealer = Dealer(Vector(c2), rs.last.shoe).traverse
    
    if (dealer.isBust) {
      println("Dealer busts")
    } else println("Dealer sticks on " + dealer.score)
   
    rs.foreach { r =>
      if (r.isBlackjack) money +=
        (if (! dealer.isBlackjack)
          r.stake * (1 + rules.BLACKJACK_PAYOUT)
        else 0).toInt
      else if (r.isBust) {}
      else if (r.score == dealer.score) money += stake
      else if (r.score >  dealer.score || dealer.isBust) money += stake * 2
    }

    println("money: =============== " + money)
    it.shoe = rs.last.shoe
  }

}