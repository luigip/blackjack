package blackjack

object Action extends Enumeration {
  type Action = Value
  val Stand, Hit, DoubleOrHit, DoubleOrStand, /*DoubleDown, Surrender,*/ SurrenderOrHit, Split = Value

  def key(s: String) = s match {
    case "S"  => Stand
    case "H"  => Hit
    case "SU" => SurrenderOrHit 
    case "Dh" => DoubleOrHit    
    case "Ds" => DoubleOrStand  
    case "SP" => Split
    // following keys for user input
    case "D"  => DoubleOrHit
    case "P"  => Split
  }

  def userInputKey(s: String) = s.toUpperCase match {
    case "S"        => Stand
    case "H"        => Hit
    case "SU" | "U" => SurrenderOrHit
    case "SP" | "P" => Split
    case "D"        => DoubleOrHit
  }  

}