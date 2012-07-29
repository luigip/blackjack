package blackjack

import Action._

class Rules (
  val DECKS_PER_SHOE         : Int,
  val NEW_DECK_EACH_ROUND    : Boolean,
  val DEALER_PEEKS           : Boolean,
  val HIT_SOFT_17            : Boolean,
  val DOUBLE_AFTER_SPLIT     : Boolean,
  val MULTIPLE_SPLIT_ACES    : Boolean,
  val MULTIPLE_SPLITS        : Boolean,
  val ONE_CARD_TO_SPLIT_ACES : Boolean,
  val BLACKJACK_PAYOUT       : Double,
  val EARLY_SURRENDER        : Boolean,
  val LATE_SURRENDER         : Boolean
  ) {

//  trait Permitted {
//    def canHit: Boolean
//    def canSplit: Boolean
//    def
//  }

  case class PermittedActions (
    hand:           Hand,
    hasJustSplit:   Boolean,
    hasJustDoubled: Boolean
  ) {
    
    lazy val pairValue: Option[Int] = if (hand.cards.size == 2 && hand.cards(0).value == hand.cards(1).value) 
                                        Some(hand.cards(0).value) 
                                      else None 
    
    lazy val isAA = pairValue == Some(1)
    lazy val firstCardAce = hand.cards(0).value == 1
    
    lazy val canHit: Boolean = 
      // Total score
      hand.score < 21 &&
      // Hasn't just doubled (which means only 1 card is dealt)
      ! hasJustDoubled &&
      // Hasn't just split with AA where only one card allowed to be dealt to split Aces
      ! (hasJustSplit && firstCardAce && ONE_CARD_TO_SPLIT_ACES)
    
    lazy val canSplit: Boolean = 
      // Two cards of same rank
      pairValue.isDefined &&
      // Hasn't already split unless (multiple split are allowed, or is AA and multiple split aces allowed)
      (! hasJustSplit || ( if (isAA) MULTIPLE_SPLIT_ACES else MULTIPLE_SPLITS ) ) 
      
    lazy val canDouble: Boolean = 
      hand.score < 21 && ( 
      if (hasJustSplit) 
        if (firstCardAce)
          ! ONE_CARD_TO_SPLIT_ACES && DOUBLE_AFTER_SPLIT
        else
          DOUBLE_AFTER_SPLIT
      else hand.cards.size == 2)

    // Note, EARLY surrender effectively occurs before hand starts, so not applicable here
    // LATE surrender is after dealer has peeked for blackjack
    lazy val canSurrender: Boolean = 
      hand.score < 21 && 
      LATE_SURRENDER && 
      hand.cards.size == 2 &&
      // surrender only available as first action
      ! hasJustSplit

    // The set of available actions
    lazy val actions: Set[Action] = 
      Seq(canHit -> Hit, canSplit -> Split, canDouble -> DoubleOrHit, canDouble -> DoubleOrStand, 
          canSurrender -> SurrenderOrHit).flatMap {
      case (true, a) => Some(a)
      case (false,_) => None
    }.toSet + Stand
    
  }
}

object Rules {
  
  private implicit def toBool(x: String) = new {
    def toBool = x match {
      case "Y" => true
      case "N" => false
      case _ => x.toBoolean
    }
  }
  //to do: remove hard-coding filenames and put in implicit config class, vals loaded from XML
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
      (rules \ "@EARLY_SURRENDER"       ).text.toBool,
      (rules \ "@LATE_SURRENDER"        ).text.toBool
    )
    xs.toMap
  }

}