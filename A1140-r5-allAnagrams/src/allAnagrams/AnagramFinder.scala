/* Author: Tommi Junttila, Aalto University.
 * Only for the use on the Aalto course CS-A1140.
 * Redistribution not allowed.
 */

package allAnagrams

/**
 * A simple class for a tool that effciently finds anagrams
 * included in the given dictionary.
 * The initialization phase can take small amount of time but
 * the single queries with the 'find' method should be very fast.
 */
class AnagramFinder(val dictionary: Seq[String]) {
  // Because/if your solution needs initialization code or data structures
  // (a scala.collection.mutable.HashMap perhaps), insert them here
  
  val dict = dictionary.map(n => n.sorted).distinct
  val m = dict.length
  val h = Array.fill(m)(scala.collection.mutable.Set.empty[String])
  val k = dict.zip( (0 to m) ).toMap
  dictionary.foreach(n => h(k(n.sorted)) = h(k(n.sorted)).+(n) )

  /** Find all the anagrams of the the word in the dictionary */
  def find(word: String): Set[String] = {
    h(k(word.sorted)).toSet
  }
}
