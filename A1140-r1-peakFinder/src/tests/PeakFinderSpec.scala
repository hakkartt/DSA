package tests
import org.scalatest._

import peakFinder._

class PeakFinderSpec extends FlatSpec with Matchers {
  val rand = new scala.util.Random

  // Some test cases for evaluating the correctness of the algorithms
  val testCases = List(Array(-2)
                       ,Array(3)
                       ,Array(1,2)
                       ,Array(2,1)
                       ,Array(2,2)
                       ,Array(1,2,3)
                       ,Array(3,2,1)
                       ,Array(1,2,1)
                       ,Array(1,1,1)
                       ,Array(1,-1,1)
                       ,Array(1,-1,-1)
                       ,Array(1,2,3,4,5,6,5)
                       ,Array(1,2,3,4,5,6,5,4)
                       ,Array(4,5,6,5,4,3,2,1,0,-1)
                       ,Array(1,2,3,4,5,6,5,4,3)
                     )

  "The linear-time algorithm" should "work correctly" in {
    for(a <- testCases) {
      withClue("Testing Array("+a.mkString(", ")+"): ") {
        val index = solveLinear(a)
        isPeak(a, index) should be (true)
      }
    }
  }

  "The log-time algorithm" should "work correctly" in {
    for(a <- testCases) {
      withClue("Testing Array("+a.mkString(", ")+"): ") {
        val index = solveLog(a)
        isPeak(a, index) should be (true)
      }
    }
  }

  val speedupTestN = 10000000
  val requiredSpeedup = 100.0
  it should s"be at least $requiredSpeedup times faster than the linear-time algorithm on arrays with $speedupTestN elements and only one peak" in {
    val nofTests = 11
    val N = speedupTestN
    val speedups = scala.collection.mutable.ArrayBuffer[Double]()
    for(t <- 1 to nofTests) {
      val peakIndex = rand.nextInt(N*60/100)+(N*20/100)
      val a = Array.tabulate[Int](N)(i => -math.abs(peakIndex-i))
      val (iLinear, tLinear) = timer.measureCpuTime {solveLinear(a) }
      val (iLog, tLog) = timer.measureCpuTime {solveLog(a) }
      val speedup = tLinear / tLog
      println(f"$tLinear%.3g vs $tLog%.3g, speedup $speedup%g")
      isPeak(a, iLinear) should be (true)
      isPeak(a, iLog) should be (true)
      speedups += speedup
    }
    val sortedSpeedups = speedups.sorted
    val medianSpeedup = sortedSpeedups(nofTests/2)
    println(f"The median of speedups: $medianSpeedup%.2f")
    medianSpeedup should be >= requiredSpeedup
  }

  var nofComps = 0
  class compMeasuringInt(private val v: Int) extends Ordered[compMeasuringInt] {
    def compare(that: compMeasuringInt): Int = {
      nofComps += 1
      v.compare(that.v)
    }
  }

  val logUseTestN = 1000000
  it should s"use at most 3*log(n) comparisons on arrays with $logUseTestN elements and only one peak" in {
    val N = logUseTestN
    val nofTests = 10
    for(t <- 1 to nofTests) {
      val peakIndex = rand.nextInt(N*60/100)+(N*20/100)
      val base = rand.nextInt(2*N)-N
      val a = Array.tabulate[compMeasuringInt](N)(i => new compMeasuringInt(base-math.abs(peakIndex-i)))
      val iLinear = solveLinear(a)
      isPeak(a, iLinear) should be (true)
      nofComps = 0
      val iLog = solveLog(a)
      val maxUse = math.ceil(3*math.log(N)/math.log(2)).toInt
      println(s" log-use test $t: used $nofComps of max allowed $maxUse comparisons")
      isPeak(a, iLog) should be (true)
      nofComps should be <= (maxUse)
    }
  }
}
