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
  val (rankChar, value) = rank match {
    case 1  => ('A', 11)
    case 11 => ('J', 10)
    case 12 => ('Q', 10)
    case 13 => ('K', 10)
    case y  => (y.toChar, y)
  }
  override lazy val toString = rankChar + suit.toString
}

object Card {
  
  def rankToChar(rank: Int) = rank match {
    case x if x < 10 => (x + 48).toChar
    case x if x < 14 => 'T'
    case 14 => 'A'
    case _ => throw new IllegalArgumentException("Rank must be 2-14")
  }
}