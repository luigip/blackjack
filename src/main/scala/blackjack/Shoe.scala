package blackjack
import util.Random.nextInt


abstract class Shoe {
  def card: Card
  def next: Shoe
}


object Shoe {
  def newRandom: Shoe = RandomCards
  def newNDeck(n: Int, penetration: Int = 0): Shoe = { 
    // If penetration left blank, use default of halfway through
    val pen = if (penetration <= 0) n * 52 /2 else penetration
    NDeck(n, pen, Deck.shuffled take pen)
  }

  case object RandomCards extends Shoe {
    def card = Card(nextInt(13) + 1, Suit.random)
    def next = this
  }

  case class NDeck(n: Int, penetration: Int, cards: Seq[Card]) extends Shoe {
    val card = cards.head
    lazy val next: Shoe = cards.size match {
      case 1 => Shoe.newNDeck(n, penetration)
      case _ => copy(cards = cards.tail)
    }
  }

}


