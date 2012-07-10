package blackjack

sealed abstract class Suit 
case object S extends Suit {override val toString = "s"}
case object H extends Suit {override val toString = "h"}
case object D extends Suit {override val toString = "d"}
case object C extends Suit {override val toString = "c"}

object Suit {
  val all = Vector(S, H, D, C)
  def random = all(util.Random.nextInt(4))
}

case class Card (rank: Int, suit: Suit) {
  val value = rank match {
    case 11 => 10
    case 12 => 10
    case 13 => 10
    case y  => y
  }
  val rankChar = Card.rankToChar(rank)
  override lazy val toString = rankChar + suit.toString
}

object Card {
  
  val rankToChar = (1 to 13).zip((1 to 13).map {
    case 1           => 'A'
    case 10          => 'T'
    case 11          => 'J'
    case 12          => 'Q'
    case 13          => 'K'
    case x           => (x + '0').toChar
  }).toMap
  
  val charToRank: Map[Char, Int] = rankToChar.toSeq.map(_.swap).toMap
}