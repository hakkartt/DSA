/* Author: Tommi Junttila, Aalto University.
 * Only for the use on the Aalto course CS-A1140.
 * Redistribution not allowed.
 */

package object quickSelect {
  // (Pseudo)random number generator with a fixed seed so that
  // error situations can be reproduced easier
  val rand = new scala.util.Random(21)

  /**
   * Find the k:th smallest (0 for the smallest) element in
   * the integer sequence seq by using the quickselect algorithm.
   */
  def find(seq: Seq[Int], k: Int): Int = {
    require(0 <= k && k < seq.length)
    // Make a working copy of the sequence as we cannot modify the argument sequence
    val a: Array[Int] = seq.toArray
    def swap(a: Array[Int], x1: Int, x2: Int) = {
      val x3 = a(x1)
      a(x1) = a(x2)
      a(x2) = x3
    }
    
    def select(lo: Int, hi: Int): Int = {
      if (hi <= lo) return a(hi)
      val (lt, gt) = partition(a, lo, hi)
      if (k > gt) return select(gt + 1, hi);
      else if (k < lt) return select(lo, lt -1)
      else return a(k)
    }

    def partition(a: Array[Int], lo: Int, hi: Int): (Int, Int) = {
      var x1 = lo
      var x2 = hi
      
      // Choosing a random index between the hi and lo indexes
      var pivotIndex = lo + rand.nextInt(hi - lo + 1)
      
      // Setting the pivotIndex (on the head of the array)
      swap(a, lo, pivotIndex)
      val p = a(lo)
      var i = lo + 1

      while (i <= x2) {
        if (a(i) < p) {
          swap(a, x1, i)
          x1 += 1
          i += 1
        } else if (a(i) > p) {
          swap(a, i, x2)
          x2 -= 1
        } else {
          i += 1
        }
      }
      (x1, x2)
    }
    
    select(0, a.size - 1)
    
  }
}
