package blackjack

//case class Dealer(cards: Seq[Card], shoe: Shoe)(implicit rules: Rules) extends HandScores {
//
//  def deal = copy(cards = cards :+ shoe.card, shoe = shoe.next)
//
//  def traverse: Dealer =
//    if (score < 17 || score == 17 && rules.HIT_SOFT_17) deal.traverse
//    else this
//
//}