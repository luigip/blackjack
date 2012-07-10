package blackjack

/**
 * A configuration with some sensible defaults
 */
class Configuration {
  val money        = 100000
  val stake        = 2
  val strategy     = Strategy.BasicStrategy
  val rules        = Rules.ruleSetsByName("Las Vegas Strip")
}