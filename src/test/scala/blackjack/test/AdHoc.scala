package blackjack.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import blackjack._


class AdHoc extends FunSuite with ShouldMatchers {
  
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
    val strat = Strategy.BasicStrategy
    
    println("Strategy table size: " + strat.lookup.size)
    
    strat.lookup(Query(11, Hard, 5)) should equal(Action.DoubleOrHit)
    strat.lookup(Query(16, Soft, 7)) should equal(Action.Hit)
    
    //test all possible combos are in map
    val qs = for {
      dealer <- 1 to 10
      (player, tt) <- (5 to 20).map(t => (t, Hard)) ++ (13 to 20).map(t => (t, Soft)) ++ (2 to 20 by 2).map(t => (t, Pair)) 
    } yield Query(player, tt, dealer)
    
    qs forall (strat.lookup.get(_).isDefined) should be(true)
//    qs foreach {q => 
//      println(q + " " + strat.lookup.get(q).isDefined)
//    }
  }
  
  test("Rules loaded correctly") {
    val lvsRules = Rules.ruleSetsByName("Las Vegas Strip")
    lvsRules.DECKS_PER_SHOE should equal(8)
    lvsRules.NEW_DECK_EACH_ROUND should be(false)
    lvsRules.BLACKJACK_PAYOUT should equal(1.5) 
  }
  
  test("Hit 3 times & Bust") {
    val shoe = Shoe.TestDeck(4, 5, 13, 8) 
    val h1 = HandNode (Seq(Card(2), Card(3)), shoe, Card(11))
    h1.score should equal(5)
    h1.traverse should equal(Return(Seq(Result(Seq(Card(2), Card(3), Card(4), Card(5), Card(13)), 2)),
                                    Context(10000, Shoe.TestDeck(8))))   
  }
  
  test("Hit and stand on 20") {
    val shoe = Shoe.TestDeck(13, 5, 13, 8)
    val h1 = HandNode (Seq(Card(2), Card(3)), shoe, Card(11))
    h1.traverse should equal(Return(Seq(Result(Seq(Card(2), Card(3), Card(13), Card(5)), 2)),
                                    Context(10000, Shoe.TestDeck(13, 8))))
  }
  
  test("Stand on soft 18 vs 8") {
    val shoe = Shoe.TestDeck(13, 5, 13, 8)
    val h1 = HandNode (Seq(Card(1), Card(7)), shoe, Card(8))
    h1.traverse should equal(Return(Seq(Result(Seq(Card(1), Card(7)), 2)),
                                    Context(10000, shoe)))
  }

  test("Hit on soft 18 vs 9, stand hard 18") {
    val shoe = Shoe.TestDeck(13, 5, 13, 8)
    val h1 = HandNode (Seq(Card(1), Card(7)), shoe, Card(9))
    h1.traverse should equal(Return(Seq(Result(Seq(Card(1), Card(7), Card(13)), 2)),
      Context(10000, Shoe.TestDeck(5, 13, 8))))
  }
  
  test("Double on hard 10 vs 7") {
    val shoe = Shoe.TestDeck(3, 5, 13, 8)
    val h1 = HandNode (Seq(Card(8), Card(2)), shoe, Card(7))
    h1.traverse should equal(Return(Seq(Result(Seq(Card(8), Card(2), Card(3)), 4)),
      Context(9998, Shoe.TestDeck(5, 13, 8))))    
  }
  
  test("Double on soft 18 vs 4") {
    val shoe = Shoe.TestDeck(13, 5, 13, 8)
    val h1 = HandNode (Seq(Card(1), Card(7)), shoe, Card(4))
    h1.traverse should equal(Return(Seq(Result(Seq(Card(1), Card(7), Card(13)), 4)),
      Context(9998, Shoe.TestDeck(5, 13, 8))))
  }

  test("Stand with 3 card soft 18 vs 4") {
    val shoe = Shoe.TestDeck(4, 5, 13, 8)
    val h1 = HandNode (Seq(Card(1), Card(3)), shoe, Card(4))
    h1.traverse should equal(Return(Seq(Result(Seq(Card(1), Card(3), Card(4)), 2)),
      Context(10000, Shoe.TestDeck(5, 13, 8))))
  }

  test("Hit on 3 card 10 vs 8") {
    val shoe = Shoe.TestDeck(3, 5, 13, 8)
    val h1 = HandNode (Seq(Card(3), Card(4)), shoe, Card(4))
    h1.traverse should equal(Return(Seq(Result(Seq(Card(3), Card(4), Card(3), Card(5)), 2)),
      Context(10000, Shoe.TestDeck(13, 8))))
  }

  test("Split 9-9 vs 2") {
    val shoe = Shoe.TestDeck(3, 5, 13, 8, 6, 3)
    val h1 = HandNode (Seq(Card(9, S), Card(9, H)), shoe, Card(2))
    h1.traverse should equal(Return(Seq(
      Result(Seq(Card(9, S), Card(3), Card(5)), 2),
      Result(Seq(Card(9, H), Card(13)), 2)
    ), Context(9998, Shoe.TestDeck(8, 6, 3))))
  }
  
  test("Stand on T-T") {
    val shoe = Shoe.TestDeck(3, 5, 13, 8, 6, 3)
    val h1 = HandNode (Seq(Card(12), Card(11)), shoe, Card(1))
    h1.traverse should equal(Return(Seq(
      Result(Seq(Card(12), Card(11)), 2)
    ), Context(10000, Shoe.TestDeck(3, 5, 13, 8, 6, 3))))      
  }
  
  test("Split multiple times") {
    val shoe = Shoe.TestDeck(9, 7, 9, 6, 3, 4, 9, 8, 1, 10)
    val h1 = HandNode (Seq(Card(9), Card(9)), shoe, Card(2))
    h1.traverse should equal(Return(Seq(
      Result(Seq(Card(9), Card(7)), 2),
      Result(Seq(Card(9), Card(6)), 2),
      Result(Seq(Card(9), Card(3), Card(4)), 2),
      Result(Seq(Card(9), Card(8)), 2),
      Result(Seq(Card(9), Card(1)), 2)
    ), Context(9992, Shoe.TestDeck(10))))    
  }
  
  test("Double after splitting") {
    val shoe = Shoe.TestDeck(1, 6, 5, 13)
    val h1 = HandNode (Seq(Card(7), Card(7)), shoe, Card(4))
    h1.traverse should equal(Return(Seq(
      Result(Seq(Card(7), Card(1), Card(6)), 4),
      Result(Seq(Card(7), Card(5)), 2)
    ), Context(9996, Shoe.TestDeck(13))))    
  }
  
  test("No multiple split") {
    val shoe = Shoe.TestDeck(9, 7, 9, 6, 3)
    val h1 = HandNode (Seq(Card(9), Card(9)), shoe, Card(2), rules = Rules.ruleSetsByName("PKR"))
    h1.traverse should equal(Return(Seq(
      Result(Seq(Card(9), Card(9)), 2),
      Result(Seq(Card(9), Card(7)), 2)
    ), Context(9998, Shoe.TestDeck(9, 6, 3))))
  }
  
  test("No double after splitting") {
    val shoe = Shoe.TestDeck(1, 6, 5, 13)
    val h1 = HandNode (Seq(Card(7), Card(7)), shoe, Card(4), rules = Rules.ruleSetsByName("Test"))
    h1.traverse should equal(Return(Seq(
      Result(Seq(Card(7), Card(1)), 2),
      Result(Seq(Card(7), Card(6)), 2)
    ), Context(9998, Shoe.TestDeck(5, 13))))
  }

  test("Muliple split Aces") {
    val shoe = Shoe.TestDeck(1, 6, 5, 1, 13, 9, 6, 3, 5, 13, 13)
    val h1 = HandNode (Seq(Card(1), Card(1)), shoe, Card(4), rules = Rules.ruleSetsByName("Test"))
    h1.traverse should equal(Return(Seq(
      Result(Seq(Card(1), Card(6), Card(5)), 2),
      Result(Seq(Card(1), Card(13)), 2),
      Result(Seq(Card(1), Card(9)), 2),
      Result(Seq(Card(1), Card(6), Card(3)), 2)
    ), Context(9994, Shoe.TestDeck(5, 13, 13))))
  }
  
  test("No muliple split Aces") {
    val shoe = Shoe.TestDeck(1, 6, 5, 1, 13, 9, 6, 3, 5, 13, 13)
    val h1 = HandNode (Seq(Card(1), Card(1)), shoe, Card(4))
    h1.traverse should equal(Return(Seq(
      Result(Seq(Card(1), Card(1)), 2),
      Result(Seq(Card(1), Card(6)), 2)
    ), Context(9998, Shoe.TestDeck(5, 1, 13, 9, 6, 3, 5, 13, 13))))
  }

  test("Allow muliple split Aces with one card to split Aces rule") {
    val shoe = Shoe.TestDeck(1, 6, 1, 13, 9, 6, 3, 5, 13, 13)
    val h1 = HandNode (Seq(Card(1), Card(1)), shoe, Card(4), rules = Rules.ruleSetsByName("Test2"))
    h1.traverse should equal(Return(Seq(
      Result(Seq(Card(1), Card(6)), 2),
      Result(Seq(Card(1), Card(13)), 2),
      Result(Seq(Card(1), Card(9)), 2),
      Result(Seq(Card(1), Card(6)), 2)
    ), Context(9994, Shoe.TestDeck(3, 5, 13, 13))))
  }
}












