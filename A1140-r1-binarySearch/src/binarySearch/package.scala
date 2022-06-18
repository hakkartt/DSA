/* Author: Tommi Junttila, Aalto University.
 * Only for the use on the Aalto course CS-A1140.
 * Redistribution not allowed.
 */

package object binarySearch {
  /**
   * The classic recursive binary serach, given here as a reference.
   * Returns true if and only if 'e' appears in the ordered array 'data'.
   */
  def search[A](data: IndexedSeq[A], e: A)(implicit ord: Ordering[A]): Boolean = {
    def inner(from: Int, to: Int): Boolean = {
      if(from <= to) {
        val mid = from + (to - from) / 2
        val cmp = ord.compare(e, data(mid))
        if(cmp == 0) true                   // e == data(mid)
        else if(cmp < 0) inner(from, mid-1) // e  < data(mid)
        else inner(mid+1, to)               // e  > data(mid)
      } else
        false
    }
    inner(0, data.length-1)
  }

  /*
   * Returns the smallest index i in the ordered array data
   * such that low <= data(i), or
   * None if all the elements in data are smaller than low.
   * As in the search method above, use ord.compare(x,y) to compare x and y.
   * NEVER use == to compare two elements of the type A,
   * they can be objects and then == evaluates to true if they are
   * the same object, not when their values are the same in the ordering "ord".
   * Use "ord.compare(x,y) == 0" instead.
   *
   * Should only perform log2(n)+1 comparisons,
   * where n is the number of elements in data and
   * the logarithm is rounded up to the next integer.
   */
  def searchLow[A](data: IndexedSeq[A], low: A)(implicit ord: Ordering[A]): Option[Int] = {
    // Use of recursion is recommended but
    // you can also make an iterative version if you wish
    def inner(from: Int, to: Int): Option[Int] = {
      if(from <= to) {
        val mid = from + (to - from) / 2
        val cmp = ord.compare(low, data(mid))
        //low == data(mid), check previous value
        if(cmp <= 0) {
          val res = inner(from, mid-1) 
          if (res == None) return(Some(mid)) else return(res)
        }
        //low > data(mid), new interval (mid+1)-to
        else inner(mid+1, to)
      } else
        None
    }
    inner(0, data.length-1)
  }
}
