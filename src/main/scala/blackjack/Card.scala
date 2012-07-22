package blackjack

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
  
  val rankToChar = (1 to 13).zip( (1 to 13).map {
    case 1  => 'A'
    case 10 => 'T'
    case 11 => 'J'
    case 12 => 'Q'
    case 13 => 'K'
    case x  => (x + '0').toChar
  } ).toMap
  
  val charToRank: Map[Char, Int] = rankToChar.toSeq.map(_.swap).toMap
  
  def apply(rank: Int): Card = apply(rank, S)
}