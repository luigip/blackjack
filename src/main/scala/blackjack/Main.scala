package blackjack


// This Main is a quick tester, hence yucky vars and iterators...

object Main extends App {

  implicit val rules = Rules.ruleSetsByName("Las Vegas Strip")
  implicit val strategy = Strategy.BasicStrategy
  var Money = 1000
  val Stake = 2

  val it = new Iterator[Card] {
    var shoe = Shoe.NDeck(8)
//    var shoe: Shoe = Shoe.TestDeck(1,3,6,4,13,13,1, 1, 1, 1,1,1)
    def hasNext = true
    def next() = try shoe.card finally shoe = shoe.next
  }

  for (i <- 1 to 20) {
    val c1 = it.next
    val c2 = it.next
    val c3 = it.next
    Money -= Stake
    val h = HandNode(Vector(c1, c3), it.shoe, c2, stake = Stake, money = Money)

    println("Dealer showing: "+ c2)
    // AskUserStrategy lets us put our own moves in!
    // Remove this to use the implicit BasicStrategy
    val rs = h.traverse(Strategy.AskUserStrategy)
    Money = rs.last.money

    val dealer = HandNode(Vector(c2), rs.last.shoe, null).traverse(Strategy.DealerStrategy).last
    println("Dealer cards: " + dealer.cards.mkString("["," ","]"))
    if (dealer.isBust) {
      println("Dealer busts")
    } else println("Dealer sticks on " + dealer.score)

    rs.foreach { r =>
        if (r.isBlackjack) Money +=
          (if (!dealer.isBlackjack)
            r.stake * (1 + rules.BLACKJACK_PAYOUT)
          else {println("Dealer blackjack, push");0}).toInt
        else if (r.isBust) {}
        else if (r.score == dealer.score) {println("Push."); Money += r.stake}
        else if (r.score > dealer.score || dealer.isBust) {println("You win!"); Money += r.stake * 2}
        else println("Dealer wins.")
    }

    println("Money balance: " + Money)
    println
    println("NEW HAND")
    println

    it.shoe = dealer.shoe
  }

}