package blackjack

/**
 * Since a player can split potentially multiple times, we define a Hand as a binary tree
 * The player's current money and the Shoe instance must be passed and returned
 */
sealed trait Hand
case object Leaf extends Hand

case class HandNode (
  cards:         Vector[Card],
  shoe:          Shoe,
  furtherAction: Boolean      = true,
  left:          Hand,
  right:         Hand         = Leaf,
  strategy:      Strategy,
  stake:         Int,
  money:         Int
  )         extends Hand {
  
  val score = {
    val rawScore = cards.map(_.value).sum
    val nAces = cards.count(_.rank == 1)
    val scores = (0 to nAces).map(rawScore - _*10)
    scores.find(_ <= 21).getOrElse(scores.last)
  }

  def hit = {
    copy(cards :+ shoe.card)
  }
  
  def stand = copy(furtherAction = false)
  
  def double = {
    copy(cards :+ shoe.card, stake = stake * 2, furtherAction = false)
  }
  
  def split = {
    val card2 = cards(1)
    copy(cards.updated(1, shoe.card))
  }
  
  def preorder: Result = {
    val lres = left match {
      case Leaf => Result(money, shoe)
    }
    
  }
  
}

case class Result(money: Int, shoe: Shoe)