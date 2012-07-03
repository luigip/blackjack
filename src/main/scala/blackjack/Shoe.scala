package blackjack
import util.Random.nextInt


abstract class Shoe {
  def card: Card
  def next: Shoe
}

case object RandomCards extends Shoe {
  def card = Card(nextInt(13) + 1, Suit.random)
  def next = this
}

case class NDeck(n: Int, penetration: Int, cards: Vector[Card]) extends Shoe {
  def card = cards.head
  def next: NDeck = cards.size match {
    case 1 => Shoe.newNDeck(n, penetration)
    case _ => copy(cards = cards.tail)
  }
}

object Shoe {
  def newRandom = RandomCards
  def newNDeck(n: Int, penetration: Int = 0) = { 
    val pen = if (penetration <= 0) n * 52 /2 else penetration
    NDeck(n, pen, Deck.shuffled take pen)
  }
  
}

