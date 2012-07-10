package blackjack

/**
 * Since a player can split potentially multiple times, we define a Hand as a binary tree
 * The player's current money and the Shoe instance must be passed and returned
 */
sealed trait Hand
case object Leaf extends Hand

case class HandNode (
  cards:         Seq[Card],
  shoe:          Shoe,
  dealerCard:    Card,
  furtherAction: Boolean      = true,
  strategy:      Strategy,
  stake:         Int,
  money:         Int
  )         extends Hand {

  // Score counting Aces as 1
  val rawScore = cards.map(_.value).sum
  
  val score = {
    val nAces = cards.count(_.rank == 1)
    val scores = (nAces to 0 by -1).map(rawScore + _*10)
    scores.find(_ <= 21).getOrElse(scores.last)
  }

  def isSoft = {
    val i = cards indexWhere (_.rank == 1)
    i != -1 && rawScore <= 10
  }

  val totalType = cards match {
    case Seq(a, b) if a.rank == b.rank => TotalType.Pair
    case xs if isSoft                     => TotalType.Soft
    case _                                => TotalType.Hard
  }
  //TODO remove duplication from these methods with the traverse method
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
  
  def traverse: Return = score match {
    
    // Bust
    case x if x > 21 => Return(Seq(Result(cards, stake)), Context(money, shoe))
    // Not bust, check for correct action
    // Dealer already checked for blackjack
    case x =>    
      val query = Query(x, totalType, dealerCard.rank)
      val action = strategy.lookup(query)
      
      action match {

        case Action.Hit    => copy(cards = cards :+ shoe.card, shoe = shoe.next).traverse

        case Action.Stand  => Return(Seq(Result(cards, stake)), Context(money, shoe))

        case Action.Double => Return(Seq(Result(cards :+ shoe.card, stake * 2)), Context(money - stake, shoe.next))

        case Action.Split  => {
          val left: Return = x match {
            // A - A : only deal 1 card
            case 2 => Return(Seq(Result(Seq(cards(0), shoe.card), stake)), Context(money, shoe.next))
            // other pairs
            case _ => copy(cards = Seq(cards(0), shoe.card), shoe = shoe.next).traverse
          }
          val right: Return = x match {
            // A - A
            case 2 => Return(Seq(Result(Seq(cards(1), left.context.shoe.card), stake)), Context(money, left.context.shoe.next))
            // other pairs
            case _ => copy(cards = Seq(cards(1), left.context.shoe.card), shoe = left.context.shoe.next).traverse
          }
          Return(left.results ++ right.results, right.context)
        }
      }

  }
  
}

case class Return(results: Seq[Result], context: Context)
case class Result(cards: Seq[Card], stake: Int)
case class Context(money: Int, shoe: Shoe)