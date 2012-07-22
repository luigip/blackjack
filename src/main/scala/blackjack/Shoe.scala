package blackjack
import util.Random

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

  // A constantly-shuffled shoe
  case object RandomCards extends Shoe {
    def card = Card(Random.nextInt(13) + 1, Suit.random)
    def next = this
  }

  // A shoe consisting of N packs of cards, with a stop part-way through
  // Penetration is a specification of the deck type and is thus a constant; it needs to be remembered for
  // when we re-shuffle the shoe. "Current" penetration is measured as simply the number of cards left.
  case class NDeck(n: Int, penetration: Int, cards: Seq[Card]) extends Shoe {
    val card = cards.head
    lazy val next: Shoe = cards.size match {
      case 1 => Shoe.newNDeck(n, penetration)
      case _ => copy(cards = cards.tail)
    }
  }
  
  object NDeck {   
    def apply(n: Int) = newNDeck(n)
  }

  // A shoe where cards are specified manually for testing purposes
  case class TestDeck(ranks: Int *) extends Shoe {
    val card = Card(ranks.head, S)
    lazy val next = TestDeck(ranks.tail: _*)
  }
  
}


