package blackjack

sealed abstract class Suit
case object S extends Suit {override val toString = "s"}
case object H extends Suit {override val toString = "h"}
case object D extends Suit {override val toString = "d"}
case object C extends Suit {override val toString = "c"}

object Suit {
  val all = Vector(S, H, D, C)
  def random = all(util.Random.nextInt(4))
}