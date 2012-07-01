package blackjack

trait Shoe

case class ::(head: Card, tail: Shoe) extends Shoe

case object EmptyShoe extends Shoe