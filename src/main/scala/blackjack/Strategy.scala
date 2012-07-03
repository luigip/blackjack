package blackjack

object Action extends Enumeration {
  type Action = Value
  val Stand, Hit, Double, Split = Value
}

object TotalType extends Enumeration {
  type TotalType = Value
  val Hard, Soft, Pair = Value
}

import Action._
import TotalType._

case class Query(cardsValue: Int, totalType: TotalType, dealerCardRank: Int)

trait Strategy {
  def lookup: Map[Query, Action]
}

object BasicStrategy extends Strategy {
  
  private val data = xml.XML.loadFile("/resources/BasicStrategy.xml")

  private val xs = for {
    hands @ <Strategy>{_*}</Strategy> <- data
    hand                              <- hands
    elem                              <- hand.child
    if elem.label startsWith "action_"
    playerHand = (hand \ "playerHand").text.toInt
    dealerRank = elem.label.last
    totalType  = TotalType.withName((hand \ "totalType").text)
    action     = Action.withName(elem.text)
  } yield Query(playerHand, totalType, dealerRank) -> action
  
  val lookup = xs.toMap
  
}

