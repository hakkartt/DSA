/* Author: Tommi Junttila, Aalto University.
 * Only for the use on the Aalto course CS-A1140.
 * Redistribution not allowed.
 */

package object shortestSubstringMissing {
  /**
   * A helper iterator class that enumerates all the strings of certain length
   * over the given alphabet.
   */
  class StringIter(length: Int, alphabet: Set[Char]) extends Iterator[String] {
    require(length > 0)
    require(alphabet.nonEmpty)
    require(length < 63.0/(math.log(2) * math.log(alphabet.size)))
    private val alpha = alphabet.toArray.sorted
    private var current: Long = 0L
    private val end = (0 until length).foldLeft(1L)({case (prev,i) => prev*alpha.length})
    private var tmp = new Array[Char](length)
    def hasNext = (current != end)
    def next(): String = {
      var v = current
      for(i <- 0 until length) {
        val c = alpha((v % alpha.length).toInt)
        v = v / alpha.length
        tmp(length-i-1) = c
      }
      current += 1
      tmp.mkString("")
    }
  }

  /**
   * Find a shortest string
   * - whose characters are from the alphabet and
   * - that is not a substring of the string str.
   * A slow method given here just for reference.
   */
  def findOneSlow(str: String, alphabet: Set[Char]): String = {
    require(alphabet.nonEmpty)
    require(str.forall(c => alphabet contains c))
    var length = 1
    while(true) {
      val iter = new StringIter(length, alphabet)
      while(iter.hasNext) {
        val s = iter.next()
        if(!str.contains(s))
          return s
      }
      length += 1
    }
    "" // Just for type checking
  }


  /**
   * Find a shortest string
   * - whose characters are from the alphabet and
   * - that is not a substring of the string str.
   */
  
  import scala.collection.mutable.HashSet
  import scala.math._
  
  def findOne(str: String, alphabet: Set[Char]): String = {
    require(alphabet.nonEmpty)
    require(str.forall(c => alphabet contains c))
    val n = str.length
    val m = alphabet.size
    var found = HashSet[String]()
    var status = false
    var t = 1
    // On each iteration there are k possibilities for outcomes
    var k = scala.math.pow(m, t)
    
    def finder(x: Int): String = {
      val iterations = new StringIter(x, alphabet)
      for (i <- iterations) {
        if (!found.contains(i)) {return i}
      }
      "NaN"
    }
    
    var i = 0
    while (t <= n) {
     while (found.size != k && i <= n - t) {
       found += str.slice(i, i+t)
       if (i == n - t && found.size != k) {
         return finder(t)
       }
       i += 1
     }
     found.clear()
     t += 1
     // Update possible outcomes
     k = scala.math.pow(m, t)
     i = 0
    }
    ""
  }
}
