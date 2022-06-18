/* Author: Tommi Junttila, Aalto University.
 * Only for the use on the Aalto course CS-A1140.
 * Redistribution not allowed.
 */

package nutsAndBolts
import scala.reflect.ClassTag

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Buffer

object matcher {
  /**
   * Find matching pairs for the nuts and bolts.
   * A straightforward, quadratic-time algorithm for reference
   * when measuring performance.
   */
  def slow[NutType <% Nut : ClassTag, BoltType <% Bolt : ClassTag](nuts: IndexedSeq[NutType], bolts: IndexedSeq[BoltType]): IndexedSeq[(NutType,BoltType)] = {
    val result = scala.collection.mutable.ArrayBuffer[(NutType,BoltType)]()
    val unmatchedBolts = bolts.toArray
    var nofUnmatchedBolts = unmatchedBolts.length
    var i = 0
    while(i < nuts.length && nofUnmatchedBolts > 0) {
      val nut = nuts(i)
      var boltFound = false
      var j = 0
      while(j < nofUnmatchedBolts && boltFound == false) {
        val bolt = unmatchedBolts(j)
        if(nut.compare(bolt) == 0) {
          boltFound = true
          // Found a matching bolt, append the pair to the result
          result += ((nut, bolt))
          // (Pseudo)remove the bolt from the unmatched array by
          // moving it to the end of the current subarray and
          // shrinking the subarray
          nofUnmatchedBolts -= 1
          assert(nofUnmatchedBolts >= 0 && j <= nofUnmatchedBolts)
          unmatchedBolts(j) = unmatchedBolts(nofUnmatchedBolts)
          unmatchedBolts(nofUnmatchedBolts) = bolt
        } else
          j += 1
      }
      i += 1
    }
    result.toIndexedSeq
  }


  /**
   * The faster algorithm based on a variant of quicksort.
   */
  def fast[NutType <% Nut : ClassTag, BoltType <% Bolt : ClassTag](nuts: IndexedSeq[NutType], bolts: IndexedSeq[BoltType]): IndexedSeq[(NutType,BoltType)] = {
    
    // Sum up the matched pairs into ArrayBuffer
    val result = scala.collection.mutable.ArrayBuffer[(NutType,BoltType)]()
    
    def rMatch(nuts: IndexedSeq[NutType], bolts: IndexedSeq[BoltType]): Unit = {
      // Picking a nut.
      val n = nuts(0)  
      // Sorting the bolts based on the selected nut.
      val sortedBolts = bolts.groupBy(_.compare(n))
      //Sort the nuts similarly
      val sortedNuts = nuts.groupBy(_.compare(sortedBolts(0)(0)))
      result ++= (sortedNuts(0).zip(sortedBolts(0)))
      // Check if there is need to sort the smaller instances too.
      if (sortedBolts.get(-1).isDefined) rMatch(sortedNuts(-1), sortedBolts(-1));
      if (sortedBolts.get(1).isDefined) rMatch(sortedNuts(1), sortedBolts(1));
    }
    
    rMatch(nuts, bolts)
    result
  }
}
