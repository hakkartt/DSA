/* Authors: Markus Arlander and Tommi Junttila, Aalto University.
 * Only for the use on the Aalto course CS-A1140.
 * Redistribution not allowed.
 */

package object rabinKarp {
  /**
   * A reference implementation of the naive substring search algorithm.
   * Returns the starting index of the first occurrence of the pattern
   * in the text, or -1 if the pattern does not occur in the text.
   * Works in time O(nm), where n is the lenght of the text string and
   * m is the lenght of the pattern string.
   */
  def findSubstringNaive(text: String, pattern: String): Int = {
    val n = text.size
    val m = pattern.size
    val end = n - m
    var i = 0
    while(i <= end) {
      var j = i
      var k = 0
      while(k < m && text(j) == pattern(k)) {
        j += 1
        k += 1
      }
      if(k == m)
        return i
      i += 1
    }
    -1
  }

  /**
   * Substring search with the Rabin-Karp algorithm.
   * Returns the starting index of the first occurrence of the pattern
   * in the text, or -1 if the pattern does not occur in the text.
   * Works in expected time O(n+m), where n is the lenght of the text string and
   * m is the lenght of the pattern string.
   */
  def findSubstring(text: String, pattern: String): Int = {
    val n = text.size
    val m = pattern.size
    if(m > n)
      return -1
      
    // base
    // https://cs.stackexchange.com/questions/93002/by-what-criteria-is-the-base-value-selected-in-rabin-karp-algorithm
    val a = 10
    
    var value = m-1
    var hash_pattern = 0
    var coef = 1
    
    while(value >= 0){
      hash_pattern += coef*pattern(value).toInt
      coef = a * coef
      value -= 1
    }
  
    var i = 0
    value = m-1
    var x = text.take(m)
    var roll = 0
    coef = 1
    
    while(value >= 0){
      roll += coef * x(value).toInt
      coef = a * coef
      value -= 1
    }
    
    var coef2 = 1
    var y = 0
    
    while(y <  m-1){
      y += 1
      coef2 = a * coef2
    }
    
    val goal = n - m
    
    while(i <= goal) {
      if(hash_pattern == roll){
        if(text.substring(i, i+m) == pattern){
          return i
        }
      }
      if(i != goal){
        roll = a*(roll - text(i) * coef2) + text(i+m)
      }
      i += 1
    }
    -1  
  }
}