/* Author: Tommi Junttila, Aalto University.
 * Only for the use on the Aalto course CS-A1140.
 * Redistribution not allowed.
 */

import scala.reflect.ClassTag

package object peakFinder {
  /* An auxiliary helper function used in the unit tests.
   * You may use it as well if you wish (but don't have to). */
  def isPeak[T <% Ordered[T]](arr: Array[T], index: Int): Boolean = {
    require(arr.nonEmpty)
    val n = arr.length
    require(0 <= index && index < n)
    if(index == 0) n == 1 || arr(index) >= arr(index+1)
    else if(index == n-1) arr(index) >= arr(index-1)
    else arr(index) >= arr(index-1) && arr(index) >= arr(index+1)
  }

  /**
   * Returns the index of a peak in the array arr.
   * Works in time O(n), where n is the number of elements in arr.
   */
  def solveLinear[T <% Ordered[T]](arr: Array[T]): Int = {
    require(arr.nonEmpty)
    val n = arr.length
    if(n == 1) return 0
    if(arr(0) >= arr(1)) return 0
    if(arr(n-1) >= arr(n-2)) return n-1
    var i = 1
    while(i < n-1) {
      if(arr(i) >= arr(i-1) && arr(i) >= arr(i+1))
        return i;
      i += 1
    }
    assert(false, "Should never enter this line")
    -1
  }

  /**
   * Returns the index of a peak in the array arr.
   * Works in time O(log n), where n is the number of elements in arr.
   */
  def solveLog[T <% Ordered[T]](arr: Array[T]): Int = {
    require(arr.nonEmpty)
    var u = arr.size - 1
    var l = 0
    if (isPeak(arr, l)) return l
    if (isPeak(arr, u)) return u
    while (u > l) {
      val mid = (u + l) / 2
      if (isPeak(arr, mid)) {
        return mid
      } else {
        if (arr(mid + 1) > arr(mid - 1)) {
          l = mid
        } else {
          u = mid
        }
      }
    }
    u
  }
}
