package blackjack

/**
 * Since a player can split potentially multiple times, we define a Hand as a binary tree
 * The player's current money and the Shoe instance must be passed and returned
 */

import Action._

sealed trait Hand

trait HandScores {
  def cards:           Seq[Card]
  val rawScore = cards.map(_.value).sum
  val aceCount = cards.count(_.rank == 1)
  // Find maximum score
  val score = {
    val scores = (aceCount to 0 by -1).map(rawScore + _*10)
    scores.find(_ <= 21).getOrElse(scores.last)
  }
  def isSoft = cards.exists(_.rank == 1) && rawScore <= 10
  def isBust = score > 21
  def isBlackjack = cards.size == 2 && score == 21
}

case class Return(results: Seq[Result], context: Context)
case class Result(cards: Seq[Card], stake: Int)
case class Context(money: Int, shoe: Shoe)

case class HandNode (
  cards:           Seq[Card],
  shoe:            Shoe,
  dealerCard:      Card,
  stake:           Int     = 2,
  money:           Int     = 10000,
  hasJustSplit:    Boolean = false,
  hasJustDoubled:  Boolean = false
  ) (implicit val rules: Rules) 
  extends Hand with HandScores {
  
  lazy val permitted = rules.PermittedActions(this, hasJustSplit, hasJustDoubled)
   
  val totalType = cards match {
    case xs if permitted.canSplit         => TotalType.Pair
    case xs if isSoft                     => TotalType.Soft
    case _                                => TotalType.Hard
  }
  
  def deal = copy(cards = cards :+ shoe.card, shoe = shoe.next)
  
  // todo: simplify this and the test cases so that it just returns a Seq[HandNode]
  // you can access the money / shoe from the last node
  
  def noMoreAction = Seq(this) 
//    Return(Seq(Result(cards, stake)), Context(money, shoe))
  
  def split = copy( 
    cards = Seq(cards(0), shoe.card),
    shoe = shoe.next,
    hasJustSplit = true
  )
  
  def rightSplit(cont: HandNode) = copy(
    cards = Seq(cards(1), cont.shoe.card),
    shoe = cont.shoe.next,
    money = cont.money - stake,
    hasJustSplit = true
  )
  
  // In case of splittable A-A, don't look up soft total of 12, but as 2
  def lookUpScore = if (permitted.canSplit && aceCount == 2) 2 else score
  
  // Automated traversal using a strategy
  def traverse(implicit strategy: Strategy): Seq[HandNode] = lookUpScore match {

    case x if permitted.actions.isEmpty => noMoreAction

    case x =>    
      val query = Query(x, totalType, dealerCard.value)
      val action = strategy.lookup(query)
      
      action match {
        
        // DOUBLE
        case y if permitted.canDouble && (y == DoubleOrHit || y == DoubleOrStand) =>
          deal.copy( 
            stake = stake * 2, 
            money = money - stake, 
            hasJustDoubled = true
          ).traverse

        // HIT
        case y if permitted.canHit && (y == Hit || y == DoubleOrHit ) => 
          deal.traverse

        // STAND
        case y if y == Stand || y == DoubleOrStand => 
          noMoreAction 

        // SPLIT (canSplit is a sanity check)
        case Split if permitted.canSplit => {
          val left = split.traverse 
          val right = rightSplit(left.last).traverse 
//          Return(left.results ++ right.results, right.context)
          left ++ right
        }

        case _ => sys.error("Did not match on action")
      }

  }
  
}

