package blackjack

class Rules (
  val DECKS_PER_SHOE         : Int
 ,val NEW_DECK_EACH_ROUND    : Boolean
 ,val DEALER_PEEKS           : Boolean
 ,val HIT_SOFT_17            : Boolean
 ,val DOUBLE_AFTER_SPLIT     : Boolean
 ,val MULTIPLE_SPLIT_ACES    : Boolean
 ,val MULTIPLE_SPLITS        : Boolean
 ,val ONE_CARD_TO_SPLIT_ACES : Boolean
 ,val BLACKJACK_PAYOUT       : Double
 ,val LATE_SURRENDER         : Boolean
)

object Rules {
  
  implicit def toBool(x: String) = new {
    def toBool = x match {
      case "Y" => true
      case "N" => false
      case _ => x.toBoolean
    }
  }
  
  val ruleSetsByName: Map[String, Rules] = {
    val data = xml.XML.loadFile("src/main/resources/Rules.xml")
    val xs = for {
      rules <- data \ "ruleset" 
    } yield 
      (rules \ "@name").text -> new Rules(
      (rules \ "@DECKS_PER_SHOE"        ).text.toInt,
      (rules \ "@NEW_DECK_EACH_ROUND"   ).text.toBool,
      (rules \ "@DEALER_PEEKS"          ).text.toBool,
      (rules \ "@HIT_SOFT_17"           ).text.toBool,
      (rules \ "@DOUBLE_AFTER_SPLIT"    ).text.toBool,
      (rules \ "@MULTIPLE_SPLIT_ACES"   ).text.toBool,
      (rules \ "@MULTIPLE_SPLITS"       ).text.toBool,
      (rules \ "@ONE_CARD_TO_SPLIT_ACES").text.toBool,
      (rules \ "@BLACKJACK_PAYOUT"      ).text.toDouble,
      (rules \ "@LATE_SURRENDER"        ).text.toBool
    )
    xs.toMap
  }
}