package blackjack

import util.Random.shuffle

object Deck {
  val newDeck: Vector[Card] =
    Vector.tabulate(4, 13)((s, r) => Card(r+1, Suit.all(s))).flatten
  
  def shuffled = shuffle(newDeck)
}