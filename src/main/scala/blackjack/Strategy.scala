package blackjack

import Action._
import TotalType._

case class Query(cardsValue: Int, totalType: TotalType, dealerCardRank: Int)

class Strategy[T <: Hand] (strat: T => Action) {
  def action(hand: T): Action = strat(hand)
}

object Strategy {
  
  val BasicStrategy = {
    val map = load("src/main/resources/BasicStrategy.xml")
    new Strategy[PlayerHand] ({ hand =>
      import hand._
      // In case of splittable A-A, don't look up soft total of 12, but as 2
      val lookUpScore = if (permitted.canSplit && aceCount == 2) 2 else score

      if (permitted.actions == Set(Stand)) Stand
      else {
        val query = Query(lookUpScore, totalType, dealerCard.value)
        map(query)
      }
    })
  }

  val DealerStrategy = new Strategy[DealerHand] ({ hand =>
    import hand._
    if (score < 17 || score == 17 && rules.HIT_SOFT_17)
      Hit
    else
      Stand
  })

  val AskUserStrategy = new Strategy[PlayerHand]({ hand =>
    import hand._
    println("Cards: " + cards.mkString("["," ","]") + " Score: "+score)
    if (isBust) { println("Player busts."); Stand }
    else if (isBlackjack) { println("Blackjack!"); Stand}
    else if (permitted.actions == Set(Stand)) {println("Player stands."); Stand}
    else {
      println("Permitted actions: " + permitted.actions.mkString(", "))
      def act: Action = {
        val a = try Action.userInputKey(readLine)
        catch { case e =>
          println(
            """Valid inputs (not case-sensitive):
              |    case "S"        => Stand
              |    case "H"        => Hit
              |    case "SU" | "U" => SurrenderOrHit
              |    case "SP" | "P" => Split
              |    case "D"        => DoubleOrHit
            """.stripMargin)
          act
        }
        if (! permitted.actions.contains(a)) {
          println("You can't do that!")
          act
        } else a
      }
      act
    }
  })

  def load(fileName: String): Map[Query, Action] = {
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
    
    xs.toMap
  }
  
}
