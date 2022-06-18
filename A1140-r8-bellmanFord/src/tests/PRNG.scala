package tests

/*
 * A fixed pseudorandom number generator for the sake of reproducibility
 * (if there are several JDKs out there with different implementations).
 * Should be the same as in Java, see http://docs.oracle.com/javase/8/docs/api/java/util/Random.html
 */
class PRNG(seed: Int) {
  val multiplier: Long = 0x5DEECE66DL
  val addend: Long = 0xBL
  val mask: Long = (1L << 48) - 1
  var _seed: Long = (seed ^ multiplier) & mask
  def nextInt(bound: Int): Int = {
    require(bound > 0)
    def next(bits: Int): Int = {
      _seed = (_seed * multiplier + addend) & mask;
      return (_seed >>> (48 - bits)).toInt;
    }
    if((bound & -bound) == bound)
      return ((bound * next(31).toLong) >> 31).toInt
    var bits = 0
    var v = 0
    do {
      bits = next(31);
      v = bits % bound;
    } while (bits - v + (bound-1) < 0);
    return v;
  }
}
