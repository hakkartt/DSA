package tests
import org.scalatest._

import timer._
import unionFind._

class LargestConnectingWeightSpec extends FlatSpec with Matchers {
  /*
   * A fixed pseudorandom number generator for the sake of reproducibility
   * (if there are several JDKs out there with different implementations).
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

  /* A helper method for generating pseudorandom edge-weighted graphs */
  def randomEdges(nofVertices: Int, nofEdges: Int, minW: Int, maxW: Int,
                  seed: Int): Seq[(Int, Int, Int)] = {
    require(minW <= maxW)
    val rand = new PRNG(seed)
    val edges = new scala.collection.mutable.ArrayBuffer[(Int, Int, Int)]()
    while(edges.length < nofEdges) {
      val vertex1 = rand.nextInt(nofVertices)
      val vertex2 = rand.nextInt(nofVertices)
      if(vertex1 != vertex2) {
        val w = rand.nextInt(maxW - minW + 1) + minW
        edges += ((vertex1, w, vertex2))
      }
    }
    edges.toSeq
  }

  "The solve method" should "be correct" in {
    val tests = List((6, List((0, 1, 1), (0, 5, 2), (1, 7, 2), (2, 4, 3), (3, 10, 4), (3, 12, 5), (4, 2, 5)), Some(4))
                     ,(6, List((0, 1, 1), (0, 5, 2), (1, 7, 2), (3, 10, 4), (3, 12, 5), (4, 2, 5)), None)
                     ,(6, List((0, 3, 1), (1, 8, 2), (2, 9, 3), (3, 4, 4), (4, 17, 5), (5, 6, 0), (5, 5, 2)), Some(5))
                   )
    for((nofVertices, edges, correct) <- tests) {
      val result = solver.solve(nofVertices, edges)
      result should be (correct)
    }
  }

  it should "correct and efficient" in {
    val tests = List((100000, 1000000, 1, 1000, 1, Some(403))
                     ,(100000, 1000000, 1, 1000, 2, Some(339))
                     ,(100000, 1000000, 1, 1000, 3, Some(315))
                     ,(100000, 1000000, 1, 1000, 4, Some(440))
                     ,(100000, 1000000, 1, 1000, 5, Some(449))
                     ,(100000, 1000000, 1, 1000, 6, Some(327))
                     ,(100000, 1000000, 1, 1000, 7, Some(474))
                     ,(100000, 1000000, 1, 1000, 8, Some(277))
                     ,(100000, 1000000, 1, 1000, 9, Some(273))
                     ,(100000, 1000000, 1, 1000, 10, Some(436))
                   )
    for((nofVertices, nofEdges, minCap, maxCap, seed, correct) <- tests) {
      println(s" Testing with edges = randomEdges($nofVertices, $nofEdges, $minCap, $maxCap, $seed)")
      val edges = randomEdges(nofVertices, nofEdges, minCap, maxCap, seed)
      val (result, time) = timer.measureCpuTime {solver.solve(nofVertices, edges) }
      result should be (correct)
      println(s"  Time: $time")
      time should be <= (3.0)
   }
  }
}
