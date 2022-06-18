/* Authors: Markus Arlander and Tommi Junttila, Aalto University.
 * Only for the use on the Aalto course CS-A1140.
 * Redistribution not allowed.
 */

package tests
import org.scalatest._
import timer._
import rabinKarp._

class RabinKarpSpec extends FlatSpec with Matchers {

  def randomChar(alphabet: Vector[Char], rand: scala.util.Random): Char =
    alphabet(rand.nextInt(alphabet.length))
  
  "Your findSubstring" should "give the same results as indexOfSlice" in {
    val testCases = List(("AAAAAAA", "AAA"),
                         ("ABCDEFG", "FG"),
                         ("ABCDEFG", "FGE"),
                         ("BLABLAB", "LA"))
    for((text, pattern) <- testCases){
      val result: Int = findSubstring(text, pattern)
      val correct = text.indexOfSlice(pattern)
      withClue(s"$pattern in $text") {
        result should be (correct)
      }
    }
  }
  
  it should "work efficiently on random binary alphabet strings" in {
    val rand = new scala.util.Random()
    val alphabet = Set('0', '1').toVector
    val nofTests = 20
    val n = 10000000
    val m = 20
    var rkTimeCumu = 0.0
    var naiveTimeCumu = 0.0
    var indexOfSliceCumu = 0.0
    var indexOfCumu = 0.0
    println("Testing on random binary strings")
    for(t <- 1 to nofTests){
      println(" Test "+t)
      val text = (0 until n).map(i => randomChar(alphabet, rand)).mkString
      val pattern = if(rand.nextInt(100) < 50) (0 until m).map(i => randomChar(alphabet, rand)).mkString else {val s = rand.nextInt(n-m); text.substring(s,s+m) }
      System.gc()
      val (result, rkTime) = measureCpuTime{ findSubstring(text, pattern) }
      rkTimeCumu += rkTime
      val (correct, indexOfSlice) = measureCpuTime{ text.indexOfSlice(pattern) }
      indexOfSliceCumu += indexOfSlice
      val (indexOfValue, indexOfTime) = measureCpuTime{ text.indexOf(pattern) }
      indexOfCumu += indexOfTime
      val (naive, naiveTime) = measureCpuTime{ findSubstringNaive(text, pattern) }
      naiveTimeCumu += naiveTime
      println(f"  findSubstring: $rkTime%.3g")
      println(f"  findSubstringNaive: $naiveTime%.3g")
      val naiveSpeedup = naiveTime / rkTime
      println(f"   speedup: $naiveSpeedup%.3g")
      println(f"  indexOfSlice: $indexOfSlice%.3g")
      val indexOfSliceSpeedup = indexOfSlice / rkTime
      println(f"   speedup: $indexOfSliceSpeedup%.3g")
      println(f"  indexOf: $indexOfTime%.3g")
      val indexOfSpeedup = indexOfTime / rkTime
      println(f"   speedup: $indexOfSpeedup%.3g")
      result should be (correct)
    }
    println(" Cumulative times")
    println(f"  findSubstring: $rkTimeCumu%.3g") 
    println(f"  findSubstringNaive: $naiveTimeCumu%.3g") 
    val naiveSpeedup = naiveTimeCumu / rkTimeCumu
    println(f"   speedup: $naiveSpeedup%.3g")
    naiveSpeedup should be >= (1.2)
    println(f"  indexOfSlice: $indexOfSliceCumu%.3g")
    val indexOfSliceSpeedup = indexOfSliceCumu / rkTimeCumu
    println(f"   speedup: $indexOfSliceSpeedup%.3g")
    println(f"  indexOf: $indexOfCumu%.3g")
    val indexOfSpeedup = indexOfCumu / rkTimeCumu
    println(f"   speedup: $indexOfSpeedup%.3g")
  }

}
