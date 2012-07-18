package blackjack

object Action extends Enumeration {
  type Action = Value
  val Stand, Hit, DoubleOrHit, DoubleOrStand, DoubleDown, Surrender, SurrenderOrHit, Split = Value
  
  def key(s: String) = s match {
    case "S"  => Stand
    case "H"  => Hit
    case "SU" => Hit            // surrender if allowed
    case "Dh" => DoubleOrHit    // if allowed, otherwise hit
    case "Ds" => DoubleOrStand  // if allowed, otherwise stand
    case "SP" => Split 
  }
  
}

object TotalType extends Enumeration {
  type TotalType = Value
  val Hard, Soft, Pair = Value
}

import Action._
import TotalType._

case class Query(cardsValue: Int, totalType: TotalType, dealerCardRank: Int)

class Strategy (val lookup: Map[Query, Action])

object Strategy {
  
  val BasicStrategy = load("src/main/resources/BasicStrategy.xml")
  
  def load(fileName: String): Strategy = {
    val data = xml.XML.loadFile(fileName)
    val xs = for {
      hands @ <Strategy>{_*}</Strategy> <- data
      hand                              <- hands.child
      elem                              <- hand.child
      if elem.label startsWith "action_"
      playerHand = (hand \ "playerHand").text.toInt
      dealerRank = Card.charToRank(elem.label.last)
      totalType  = TotalType.withName((hand \ "totalType").text)
      action     = key(elem.text)
    } yield Query(playerHand, totalType, dealerRank) -> action
    
    new Strategy(xs.toMap)
  }
  
}
