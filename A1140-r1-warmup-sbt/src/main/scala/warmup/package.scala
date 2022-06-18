/* Author: Tommi Junttila, Aalto University.
 * Only for the use on the Aalto course CS-A1140.
 * Redistribution not allowed.
 */

package object warmup {
  /**
   * Check whether the target string t is obtained by removing exactly one
   * character from the source string s.
   * If this is the case, return (true, i), where i is an index of
   *   a character in s whose removal results in t.
   * If this is not the case, return (false, 0).
   * This is a slow, quadratic time algorithm given for reference.
   */
  def isRemovedByOneSlow(s: String, t: String): (Boolean, Int) = {
    if(s.length != t.length + 1)
      return (false, 0)
    for(i <- 0 until s.length)
      if(s.take(i) + s.substring(i+1) == t)
        return(true, i)
    return (false, 0)
  }

  /**
   * Check whether the target string t is obtained by removing exactly one
   * character from the source string s.
   * If this is the case, return (true, i), where i is an index of
   *   a character in s whose removal results in t.
   * If this is not the case, return (false, 0).
   * This algorithm should be fast, running in time O(|s|),
   * where |s| is the length of the string s.
   *
   * Hint: start comparing the strings from the beginning. If they differ
   *       at some point, what should hold for the rest of the strings?
   */
  def isRemovedByOne(s: String, t: String): (Boolean, Int) = {
    ???
  }


  /* A helper function for checking whether a string is a palindrome */
  def isPalindrome(s: String): Boolean = {
    var i = 0
    var j = s.length-1
    while(i < j) {
      if(s(i) != s(j))
        return false
      i += 1
      j -= 1
    }
    return true
  }

  /**
   * Check whether the argument string is a palindrome or
   * can be made a palindrome by removing exactly one character from it.
   * If it is not, returns (false, None).
   * If it is a palindrome, return (true, None).
   * If it is a palindrome after removing one character,
   *   return (true, Some(i)) where i is an index to remove to make the string
   *   palindrome.
   * This is a slow, quadratic time algorithm given for reference.
   */
  def almostPalindromeSlow(s: String): (Boolean, Option[Int]) = {
    if(isPalindrome(s)) return (true, None)
    for(i <- 0 until s.length)
      if(isPalindrome(s.take(i) + s.substring(i+1)))
        return(true, Some(i))
    return (false, None)
  }

  /**
   * Check whether the argument string is a palindrome or
   * can be made a palindrome by removing exactly one character from it.
   * If it is not, returns (false, None).
   * If it is a palindrome, return (true, None).
   * If it is a palindrome after removing one character,
   *   return (true, Some(i)) where i is an index to remove to make the string
   *   palindrome.
   * This algorithm should be fast, i.e. run in linear time.
   *
   * Hint: first compare the first and the last character in the string.
   *       If they are the same, then the string can be a palindrome and
   *          one can do, iteratively, similar analysis for the substring
   *          obtained by excluding the first and the last character.
   *          Do not use the "substring" method to obtain the substring but use
   *          indices that point to the currently inspected
   *          first and last characters.
   *       If they are not the same, then what are the three possible cases?
   */
  def almostPalindrome(s: String): (Boolean, Option[Int]) = {
    ???
  }
}
