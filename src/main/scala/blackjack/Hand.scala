package blackjack

/**
 * Since a player can split potentially multiple times, we define a Hand as a binary tree
 * The player's current money and the Shoe instance must be passed and returned
 */
sealed trait Hand
//case object Leaf extends Hand
case class Return(results: Seq[Result], context: Context)
case class Result(cards: Seq[Card], stake: Int)
case class Context(money: Int, shoe: Shoe)

case class HandNode (
  cards:           Seq[Card],
  shoe:            Shoe,
  dealerCard:      Card,
  strategy:        Strategy   = Strategy.BasicStrategy,
  rules:           Rules      = Rules.ruleSetsByName("Las Vegas Strip"),
  stake:           Int        = 2,
  money:           Int        = 10000,
  doublingAllowed: Boolean    = true,
  splittingAllowed:Boolean    = true,
  furtherAction:   Boolean    = true
  )         extends Hand {

  // Score counting Aces as 1
  val rawScore = cards.map(_.value).sum
  
  // Find maximum score
  val score = {
    val nAces = cards.count(_.rank == 1)
    val scores = (nAces to 0 by -1).map(rawScore + _*10)
    scores.find(_ <= 21).getOrElse(scores.last)
  }

  def isSoft = cards.exists(_.rank == 1) && rawScore <= 10
  

  val totalType = cards match {
    case Seq(a, b) if a.rank == b.rank 
      && splittingAllowed                 => TotalType.Pair
    case xs if isSoft                     => TotalType.Soft
    case _                                => TotalType.Hard
  }
  
  def dealCard = copy(cards = cards :+ shoe.card, shoe = shoe.next)
  
  def noMoreAction = Return(Seq(Result(cards, stake)), Context(money, shoe))
  
  def split = copy(cards = Seq(cards(0), shoe.card),
      shoe = shoe.next,
      doublingAllowed = rules.DOUBLE_AFTER_SPLIT,
      splittingAllowed = if (rawScore == 2) rules.MULTIPLE_SPLIT_ACES else rules.MULTIPLE_SPLITS
    )
  
  
  def rightSplit(cont: Context) = copy(cards = Seq(cards(1), cont.shoe.card),
                   shoe = cont.shoe.next,
                   money = cont.money - stake,
                   doublingAllowed = rules.DOUBLE_AFTER_SPLIT,
                   splittingAllowed = if (rawScore == 2) rules.MULTIPLE_SPLIT_ACES else rules.MULTIPLE_SPLITS
  )
  
  // In case of A-A, don't look up soft total of 12, but as 2
  def lookUpScore = if (totalType == TotalType.Pair) rawScore else score
  
  def traverse: Return = lookUpScore match {

    case x if furtherAction == false => noMoreAction
    // Bust / always stick on 21
    case x if x >= 21 => copy(furtherAction = false).traverse
    // Not bust, check for correct action
    case x =>    
      val query = Query(x, totalType, dealerCard.value)
      val action = strategy.lookup(query)
      
      action match {
        
        // DOUBLE
        case y if (y == Action.DoubleOrHit || y == Action.DoubleOrStand) 
               && cards.size == 2 && doublingAllowed
          => dealCard.copy( 
                  stake = stake * 2, 
                  money = money - stake, 
                  furtherAction = false
                 ).traverse

        // HIT
        case y if y == Action.Hit 
               || y == Action.DoubleOrHit  
          => dealCard.traverse

        // STAND
        case y if y == Action.Stand
               || y == Action.DoubleOrStand 
          => noMoreAction 

        // SPLIT
        case Action.Split  => {
          val left = x match {
            // A - A : only deal 1 card
            case y if y == 2 && rules.ONE_CARD_TO_SPLIT_ACES => 
              split.copy(furtherAction = false).traverse
            // other pairs
            case _ => split.traverse
          }
          val right = x match {
            // A - A
            case y if y == 2 && rules.ONE_CARD_TO_SPLIT_ACES => 
              rightSplit(left.context).copy(furtherAction = false).traverse
            // other pairs
            case _ => rightSplit(left.context).traverse
          }
          Return(left.results ++ right.results, right.context)
        }

        case _ => sys.error("Did not match on action")
      }

  }
  
}

