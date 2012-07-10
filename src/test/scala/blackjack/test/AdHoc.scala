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
    
    strat.lookup(Query(11, Hard, 5)) should equal(Action.Double)
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
  
  
  
  
  
}