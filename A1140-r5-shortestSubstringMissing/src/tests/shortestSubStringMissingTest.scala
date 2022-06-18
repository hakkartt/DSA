package tests
import org.scalatest._

import scala.util.Random
import timer._
import shortestSubstringMissing._

class ShortestSubstringMissingSpec extends FlatSpec with Matchers {
  val smallTestsAlphabet = Set('A','C','G','T')
  val smallTests = List(("AAAAAAAA", 1)
                        ,("ACGTACGT", 2)
                        ,("ACAAGCAG", 1)
                        ,("ACACACGT", 2)
                      )

  "The findOneSlow method" should "work correctly" in {
    for((s, l) <- smallTests) {
      val r = findOneSlow(s, smallTestsAlphabet)
      r.length should be (l)
      s.contains(r) should be (false)
    }
  }

  "The findOne method" should "work correctly" in {
    for((s, l) <- smallTests) {
      val r = findOne(s, smallTestsAlphabet)
      r.length should be (l)
      s.contains(r) should be (false)
    }
  }

  it should "work correctly on random strings" in {
    val rand = new scala.util.Random(2105)
    val nofTests = 10
    val alphabet = Array('0','1')
    val N = 50
    for(t <- 1 to nofTests) {
      val str = Array.tabulate[Char](N)(i => alphabet(rand.nextInt(alphabet.length))).mkString("")
      val resultSlow = findOneSlow(str, alphabet.toSet)
      val result = findOne(str, alphabet.toSet)
      result.length should be (resultSlow.length)
      str.contains(result) should be (false)
    }
  }

  it should "work efficiently, i.e., at least five times faster than findOneSlow on largish strings" in {
    val rand = new scala.util.Random(2105)
    val nofTests = 10
    val alphabet = Array('0','1')
    val N = 500000
    var timeCumu = 0.0
    var timeSlowCumu = 0.0
    for(t <- 1 to nofTests) {
      val str = Array.tabulate[Char](N)(i => alphabet(rand.nextInt(alphabet.length))).mkString("")
      val (result, time) = measureCpuTime {findOne(str, alphabet.toSet)}
      timeCumu += time
      val (resultSlow, timeSlow) = measureCpuTime {findOneSlow(str, alphabet.toSet)}
      timeSlowCumu += timeSlow
      result.length should be (resultSlow.length)
      str.contains(result) should be (false)
      println(f"findOne vs findOneSlow: $time%.3g vs $timeSlow%.3g")
    }
    println(f"findOne vs findOneSlow, cumulative: $timeCumu%.3g vs $timeSlowCumu%.3g")
    val speedup = timeSlowCumu / timeCumu
    println(f"Speedup: $speedup%.2f")
    speedup should be >= 5.0
  }
}
