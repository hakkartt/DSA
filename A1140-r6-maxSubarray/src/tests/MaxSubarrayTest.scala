package tests
import org.scalatest._

import maxSubarray._

class MaxSubarraySpec extends FlatSpec with Matchers {
  // Some test cases for evaluating the correctness of the algorithms
  val testCases = List((Array(-2, 1, -3, 4, -1, 2, 1, -5, 4), 6)
                       ,(Array(2,-3,-1,2,-1,2,-1), 3)
                       ,(Array(-1,-2,-3,-4), -1)
                       ,(Array(-1,-2,-3,-4), -1)
                       ,(Array(-4,-2,-3,-1), -1)
                       ,(Array(-4,-1,-3,-2), -1)
                     )

  "The cubic-time algorithm" should "work correctly" in {
    for((a,correct) <- testCases) {
      withClue("Testing Array("+a.mkString(", ")+"): ") {
        val (s, e, v) = solveCubic(a)
        0 should be <= (s)
        s should be <= (e)
        e should be < (a.length)
        subarraySum(a, s, e) should be (v)
        v should be (correct)
      }
    }
  }

  "The linear-time algorithm" should "work correctly" in {
    for((a, correct) <- testCases) {
      withClue("Testing Array("+a.mkString(", ")+"): ") {
        val (s, e, v) = solveLinear(a)
        0 should be <= (s)
        s should be <= (e)
        e should be < (a.length)
        subarraySum(a, s, e) should be (v)
        v should be (correct)
      }
    }
  }

  it should s"be 1000 times faster than the cubic algorithm on arrays with 3000 elements" in {
    val rand = new scala.util.Random(242)
    val N = 3000
    val speedups = scala.collection.mutable.ArrayBuffer[Double]()
    val requiredSpeedup = 1000.0
    val nofTests = 30
    for(t <- 1 to nofTests) {
      val a = Array.tabulate[Int](N)(i => rand.nextInt(200)-100)
      val backup = a.clone
      val ((sCubic,eCubic,vCubic), tCubic) = timer.measureCpuTime {solveCubic(a) }
      System.gc()
      val ((sLinear,eLinear,vLinear), tLinear) = timer.measureCpuTime {solveLinear(a) }
      0 should be <= (sLinear)
      sLinear should be <= (eLinear)
      eLinear should be < (backup.length)
      val speedup = tCubic / tLinear
      speedups += speedup
      println(f"  $tCubic%.3g vs $tLinear%.3g, speedup $speedup%g")
      subarraySum(backup, sLinear, eLinear) should be (vLinear)
      vLinear should be (vCubic)
      //speedup should be >= requiredSpeedup
    }
    val sortedSpeedups = speedups.sorted
    val medianSpeedup = sortedSpeedups(sortedSpeedups.size / 2)
    println(f"Median of speedups: $medianSpeedup%.2f")
    medianSpeedup should be >= requiredSpeedup
  }
}
