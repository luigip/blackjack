package blackjack.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import blackjack._

class AdHoc extends FunSuite with ShouldMatchers {

  // Rule sets
  lazy val LVS   = Rules.ruleSetsByName("Las Vegas Strip")
  lazy val PKR   = Rules.ruleSetsByName("PKR")
  lazy val Test1 = Rules.ruleSetsByName("Test")
  lazy val Test2 = Rules.ruleSetsByName("Test2")

  // Shorthand for card sequences
  def cs(xs: Int*): Seq[Card] = xs map (Card(_))
  // Shorthand for test deck
  val td = Shoe.TestDeck 
  
  test("Shoes test") {
    val shoes = Array(
      Shoe.newRandom,
      Shoe.newRandom,
      Shoe.newNDeck(1),
      Shoe.newNDeck(8),
      Shoe.newNDeck(8,5)
    )
    val n = shoes.size
    val cards = Array.fill(n)(Vector.empty[Card])
    for (i <- 1 to 1000; j <- 0 until n) {
      cards(j) :+= shoes(j).card
      shoes(j) = shoes(j).next
    }
    println("First 10 cards from random shoe 1: " + (cards(0) take 10 mkString ", "))
    println("First 10 cards from random shoe 2: " + (cards(1) take 10 mkString ", "))
    println("First 10 cards from NDeck(1)     : " + (cards(2) take 10 mkString ", "))
    println("First 10 cards from NDeck(8)     : " + (cards(3) take 10 mkString ", "))
    println("First 10 cards from Ndeck(8,5)   : " + (cards(4) take 10 mkString ", "))
    cards(0) should not equal(cards(1))
  }
  
  test("Strategy loaded correctly") {
    import TotalType._

    val stratmap = Strategy.load("src/main/resources/BasicStrategy.xml")
    stratmap.size should equal(350)
    stratmap(Query(11, Hard, 5)) should equal(Action.DoubleOrHit)
    stratmap(Query(16, Soft, 7)) should equal(Action.Hit)
    // Test all possible combos are in map
    val qs = for {
      dealer <- 1 to 10
      (player, tt) <- (5 to 20).map(t => (t, Hard)) ++ (13 to 20).map(t => (t, Soft)) ++ (2 to 20 by 2).map(t => (t, Pair)) 
    } yield Query(player, tt, dealer)
    qs forall (stratmap.get(_).isDefined) should be(true)

    val strat = Strategy.BasicStrategy
    strat.action(PlayerHand(cs(1,5), td(1), Card(5))) should equal (Action.DoubleOrHit)
  }
  
  test("Rules loaded correctly") {
    val lvsRules = Rules.ruleSetsByName("Las Vegas Strip")
    lvsRules.DECKS_PER_SHOE should equal(8)
    lvsRules.NEW_DECK_EACH_ROUND should be(false)
    lvsRules.BLACKJACK_PAYOUT should equal(1.5) 
  }
  
  // Set default rules and strategy
  implicit val rules = LVS
  implicit val strat = Strategy.BasicStrategy

  
  test("Hit 3 times & Bust") {
    val shoe = td(4, 5, 13, 8) 
    val h1 = PlayerHand (cs(2, 3), shoe, Card(11))
    val t = h1.traverse
    h1.score should equal(5)
    t(0) should have (
      'cards (cs(2, 3, 4, 5, 13)),
      'shoe (td(8)),
      'dealerCard (Card(11)),
      'stake (2),
      'money (10000)
    )
  }
  
  test("Hit and stand on 20") {
    val shoe = td(13, 5, 13, 8)
    val t = PlayerHand (cs(2, 3), shoe, Card(11)).traverse
    t(0) should have (
      'cards (cs(2, 3, 13, 5)),
      'shoe  (td(13, 8))
    )
  }
  
  test("Stand on soft 18 vs 8") {
    val shoe = Shoe.TestDeck(13, 5, 13, 8)
    val t = PlayerHand (Seq(Card(1), Card(7)), shoe, Card(8)).traverse
    t(0) should have (
      'cards (cs(1, 7)),
      'shoe  (td(13, 5, 13, 8))
    )
  }

  test("Hit on soft 18 vs 9, stand hard 18") {
    val shoe = Shoe.TestDeck(13, 5, 13, 8)
    val t = PlayerHand (Seq(Card(1), Card(7)), shoe, Card(9)).traverse
    t(0) should have (
      'cards (cs(1, 7, 13)),
      'shoe  (td(5, 13, 8))
    )
  }
  
  test("Double on hard 10 vs 7") {
    val shoe = td(3, 5, 13, 8)
    val t = PlayerHand (cs(8, 2), shoe, Card(7)).traverse
    t should have length (1)
    t(0) should have (
      'cards (Seq(Card(8), Card(2), Card(3))),
      'shoe  (Shoe.TestDeck(5, 13, 8)),
      'stake (4),
      'money (9998),
      'hasJustDoubled (true)
    )
  }
  
  test("Double on soft 18 vs 4") {
    val shoe = Shoe.TestDeck(13, 5, 13, 8)
    val t = PlayerHand (Seq(Card(1), Card(7)), shoe, Card(4)).traverse
    t should equal(Seq(
      PlayerHand(Seq(Card(1), Card(7), Card(13)), Shoe.TestDeck(5, 13, 8), Card(4), 4, 9998, hasJustDoubled = true)))
  }

  test("Stand with 3 card soft 18 vs 4") {
    val shoe = td(4, 5, 13, 8)
    val t = PlayerHand (cs(1, 3), shoe, Card(4)).traverse
    t(0) should have ('cards (cs(1, 3, 4)), 'money (10000), 'shoe (td(5, 13, 8)) )
  }

  test("Hit on 3 card 10 vs 8") {
    val shoe = td(3, 5, 13, 8)
    val t = PlayerHand (cs(3, 4), shoe, Card(4)).traverse
    t(0) should have ('cards (cs(3, 4, 3, 5)), 'shoe (td(13, 8)) )
  }

  test("Split 9-9 vs 2") {
    val shoe = td(3, 5, 13, 8, 6, 3)
    val t = PlayerHand (Seq(Card(9, S), Card(9, H)), shoe, Card(2)).traverse
    t should have length (2)
    t(0) should have ('cards (Seq(Card(9, S), Card(3), Card(5))) )
    t(1) should have ('cards (Seq(Card(9, H), Card(13))), 
      'money (9998), 'stake (2), 'shoe  (Shoe.TestDeck(8, 6, 3)) )
  }
  
  test("Stand on T-T") {
    val shoe = Shoe.TestDeck(3, 5, 13, 8, 6, 3)
    val t = PlayerHand (cs(12, 11), shoe, Card(1)).traverse
    t(0) should have (
      'cards (cs(12, 11))
    )  
  }
  
  test("Split multiple times") {
    val shoe = td(9, 7, 9, 6, 3, 4, 9, 8, 1, 10)
    val t = PlayerHand (cs(9, 9), shoe, Card(2)).traverse
    t.length should be (5)
    t(0) should have ('cards (cs(9, 7)) )
    t(1) should have ('cards (cs(9, 6)) )
    t(2) should have ('cards (cs(9, 3, 4)) )
    t(3) should have ('cards (cs(9, 8)) )
    t(4) should have ('cards (cs(9, 1)), 'money (9992), 'shoe (td(10)) )     
  }
  
  test("Double after splitting") {
    val shoe = td(1, 6, 5, 13)
    val t = PlayerHand (cs(7, 7), shoe, Card(4)).traverse
    t.length should be (2)
    t(0) should have ('cards (cs(7, 1, 6)) )
    t(1) should have ('cards (cs(7, 5)), 'money (9996), 'shoe (td(13)) )
  }
  
  test("No multiple split") {
    val shoe = td(9, 7, 9, 6, 3)
    val t = PlayerHand (cs(9,9), shoe, Card(2)) (rules = PKR) .traverse
    t should have length (2)
    t(0) should have ('cards (cs(9, 9)))
    t(1) should have ('cards (cs(9, 7)), 'money (9998), 'shoe (td(9, 6, 3)) )
  }
  
  
  test("No double after splitting") {
    val shoe = td(1, 6, 5, 13)
    val t = PlayerHand (cs(7,7), shoe, Card(4))(rules = Test1).traverse
    t should have length (2)
    t(0) should have ('cards (cs(7, 1)))
    t(1) should have ('cards (cs(7, 6)), 'money (9998), 'shoe (td(5, 13)) )
  }

  test("Muliple split Aces") {
    val shoe = Shoe.TestDeck(1, 6, 5, 1, 13, 9, 6, 3, 5, 13, 13)
    val t = PlayerHand (Seq(Card(1), Card(1)), shoe, Card(4))(rules = Test1).traverse
    t should have length (4)
    t(0) should have ('cards (cs(1, 6, 5)) )
    t(1) should have ('cards (cs(1, 13)) )
    t(2) should have ('cards (cs(1, 9)) )
    t(3) should have ('cards (cs(1, 6, 3)), 'money (9994), 'shoe (td(5, 13, 13)) )
  }
  
  test("No muliple split Aces") {
    val shoe = td(1, 6, 5, 1, 13, 9, 6, 3, 5, 13, 13)
    val t = PlayerHand (cs(1, 1), shoe, Card(4)).traverse
    t should have length (2)
    t(0) should have ('cards (cs(1, 1)))
    t(1) should have ('cards (cs(1, 6)), 'money (9998), 'shoe (td(5, 1, 13, 9, 6, 3, 5, 13, 13)) )    
  }

  test("Allow muliple split Aces with one card to split Aces rule") {
    val shoe = td(1, 6, 1, 13, 9, 6, 3, 5, 13, 13)
    val t = PlayerHand (cs(1,1), shoe, Card(4))(rules = Test2).traverse
    t should have length (4)
    t(0) should have ('cards (cs(1, 6)) )
    t(1) should have ('cards (cs(1, 13)) )
    t(2) should have ('cards (cs(1, 9)) )
    t(3) should have ('cards (cs(1, 6)), 'money (9994), 'shoe (td(3, 5, 13, 13)) )    
  }
  
  test("Surrender on first action") {
    val shoe = td(9, 7, 9, 6, 3)
    val t = PlayerHand (cs(10, 6), shoe, Card(9), money = 10000, stake = 10).traverse
    t(0) should have ('cards (cs(10, 6)), 'money (10005), 'surrendered (true),
      'shoe (td(9, 7, 9, 6, 3)))
  }

  test("No surrender if against rules") {
    val shoe = td(9, 7, 9, 6, 3)
    val t = PlayerHand (cs(10, 6), shoe, Card(9), money = 10000, stake = 10)(rules = PKR).traverse
    t(0) should have ('cards (cs(10, 6, 9)), 'money (10000), 'surrendered (false),
      'shoe (td(7, 9, 6, 3)))
  }  
  
  test("Can't surrender when already hit") {
    val shoe = td(2, 3, 9, 6, 3)
    val t = PlayerHand (cs(10, 4), shoe, Card(9), money = 10000, stake = 10).traverse
    t(0) should have ('cards (cs(10, 4, 2, 3)), 'money (10000), 'surrendered (false),
      'shoe (td(9, 6, 3)))
  }
  
  test("Can't surrender after split") {
    val shoe = td(7, 4, 5, 13, 1)
    val t = PlayerHand (cs(9, 9), shoe, Card(9)).traverse
    t.length should be (2)
    t(0) should have ('cards (cs(9, 7, 4)), 'money (10000), 'stake(2), 'surrendered(false)  )
    t(1) should have ('cards (cs(9, 5, 13)), 'money (9998), 'shoe (td(1)) ) 
  }

  test("Dealer hits on 16") {
    val shoe = td(2, 4, 7, 9)
    val t = DealerHand(cs(13), shoe).traverse
    t should have ('cards(cs(13, 2, 4, 7)), 'shoe (td(9)))
  }

  test("Dealer stands on 17") {
    val shoe = td(2, 5, 7, 9)
    val t = DealerHand(cs(13), shoe).traverse
    t should have ('cards(cs(13, 2, 5)), 'shoe (td(7, 9)))
  }

  test("Dealer stands on soft 17") {
    val shoe = td(1, 5, 7, 9)
    val t = DealerHand(cs(6), shoe).traverse
    t should have ('cards(cs(6, 1)), 'shoe (td(5, 7, 9)))
  }

  test("Dealer hits on soft 17") {
    val shoe = td(1, 5, 7, 9)
    val t = DealerHand(cs(6), shoe)(rules = Test1).traverse
    t should have ('cards(cs(6, 1, 5, 7)), 'shoe (td(9)))
  }


}












