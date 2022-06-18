package nutsAndBolts

/**
 * An abstract base class for bolts.
 */
abstract class Bolt(name: String) {
  /**
   * Compare this bolt to a nut.
   * Returns -1 if this bolt is smaller than, 0 if the same size as,
   * and 1 if larger than the nut.
   */
  def compare(nut: Nut): Int = -nut.compare(this)
}
