package blackjack

object Action extends Enumeration {
  type Action = Value
  val Stand, Hit, DoubleOrHit, DoubleOrStand, /*DoubleDown,*/ Surrender, SurrenderOrHit, Split = Value

  def key(s: String) = s match {
    case "S"  => Stand
    case "H"  => Hit
    case "SU" => Hit            // surrender if allowed
    case "Dh" => DoubleOrHit    // if allowed, otherwise hit
    case "Ds" => DoubleOrStand  // if allowed, otherwise stand
    case "SP" => Split
    // following for user input
    case "D"  => DoubleOrHit
    case "P"  => Split
  }

}