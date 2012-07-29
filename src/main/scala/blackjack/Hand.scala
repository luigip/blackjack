package blackjack

import Action._

/**
 * Hand evaluation methods common to both player and dealer
 */
trait Hand {
  def cards:       Seq[Card]
  def isBlackjack: Boolean
  lazy val rawScore = cards.map(_.value).sum
  lazy val aceCount = cards.count(_.rank == 1)
  lazy val score = {
    val scores = (aceCount to 0 by -1).map(rawScore + _ * 10)
    scores.find(_ <= 21).getOrElse(scores.last)
  }
  lazy val isSoft = cards.exists(_.rank == 1) && rawScore <= 10
  lazy val isBust = score > 21
}

/**
 * Since a player can split potentially multiple times, we define HandNodes
 * which form a binary tree. The traverse method uses a Strategy to form this tree,
 * returning a Seq of the terminal nodes
 */
case class PlayerHand (
  cards:              Seq[Card],
  shoe:               Shoe,
  dealerCard:         Card,
  stake:              Int     = 2,
  money:              Int     = 10000,
  hasJustSplit:       Boolean = false,
  hasJustDoubled:     Boolean = false,
  surrendered:        Boolean = false
  ) (implicit val rules: Rules) extends Hand {
  
  lazy val permitted = rules.PermittedActions(this, hasJustSplit, hasJustDoubled)
   
  lazy val totalType = cards match {
    case xs if permitted.canSplit   => TotalType.Pair
    case xs if isSoft               => TotalType.Soft
    case _                          => TotalType.Hard
  }
  
  // Hand is not counted as blackjack if it is result of split
  lazy val isBlackjack = cards.size == 2 && score == 21 && hasJustSplit == false
  
  def deal = copy(cards = cards :+ shoe.card, shoe = shoe.next)

  def noMoreAction = Seq(this)
  
  def surrender = Seq(copy(surrendered = true, stake = 0, money = money + stake / 2))
  
  def split = copy( 
    cards = Seq(cards(0), shoe.card),
    shoe = shoe.next,
    hasJustSplit = true
  )
  
  def rightSplit(cont: PlayerHand) = copy(
    cards = Seq(cards(1), cont.shoe.card),
    shoe = cont.shoe.next,
    money = cont.money - stake,
    hasJustSplit = true
  )

  // Traversal using a Strategy (automated or not)
  def traverse(implicit strategy: Strategy[PlayerHand]): Seq[PlayerHand] = strategy.action(this) match {
        
    // DOUBLE
    case DoubleOrHit | DoubleOrStand if permitted.canDouble   =>
      deal.copy(
        stake = stake * 2,
        money = money - stake,
        hasJustDoubled = true
      ).traverse

    // SURRENDER
    case SurrenderOrHit if permitted.canSurrender   => surrender

    // HIT
    case Hit | DoubleOrHit | SurrenderOrHit if permitted.canHit   => deal.traverse

    // SPLIT
    case Split if permitted.canSplit   => {
      val left = split.traverse
      val right = rightSplit(left.last).traverse
      left ++ right
    }

    // STAND
    case Stand | DoubleOrStand   => noMoreAction

    case _   => sys.error("Did not match on action")
  }

}

case class DealerHand(
  cards:              Seq[Card],
  shoe:               Shoe,
  strategy:           Strategy[DealerHand] = Strategy.DealerStrategy
  ) (implicit val rules: Rules)
  extends Hand {

  lazy val isBlackjack = cards.size == 2 && score == 21

  def deal = copy(cards :+ shoe.card, shoe.next)

  def traverse: DealerHand = strategy.action(this) match {
    // HIT
    case Hit  => deal.traverse
    // STAND
    case Stand  => this

    case _   => sys.error("Did not match on action")
  }
}


object TotalType extends Enumeration {
  type TotalType = Value
  val Hard, Soft, Pair = Value
}