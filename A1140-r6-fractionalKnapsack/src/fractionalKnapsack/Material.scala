package fractionalKnapsack

/**
 * The class for communication material resources.
 * - amount tells the number of units this material is available, and
 * - valuePerUnit gives the amount of credits obtainable for one unit
 *   of this material.
 */
case class Material(val name: String, val amount: Double, valuePerUnit: Double) {
  require(amount >= 0.0)
  require(valuePerUnit > 0.0)
}
