package tests
import org.scalatest._
import org.scalatest.Assertions._

import scala.util.Random
import binarySearch._

class BinarySearchSpec extends FlatSpec with Matchers {
  /*
   * Compute the ceiling of log2(x).
   * Due to rounding errors, math.ceil(math.log(x)/math.log(2))
   * reports wrong answer when x = 536870912.
   */
  def log2ceil(x: Int): Int = {
    require(x > 0)
    32 - Integer.numberOfLeadingZeros(x-1)
  }

  /*
   * A wrapper for Int Ordering that 
   * also counts the number of comparisons.
   */
  var nofComparisons = 0
  object OrderingWithCount extends Ordering[Int] {
    def compare(x: Int, y: Int): Int = {
      nofComparisons += 1
      x.compare(y)
    }
  }

  "The searchLow method" should "work correctly" in {
    val seed = 2020
    val rand = new Random(seed)
    val nofTests = 1000
    for(test <- 1 to nofTests) {
      val n = test * 10
      val offset = rand.nextInt(2*n-n)
      val range = 2*n
      val data = Array.tabulate[Int](n)(i => rand.nextInt(range)-offset).sorted
      val low = rand.nextInt(range+n/3)-offset
      nofComparisons = 0
      val result = searchLow(data, low)(OrderingWithCount)
      withClue(s"""search(Array(${data.mkString(",")}),$low) = $result: """) {
        if(data(n-1) < low) {
          result shouldBe None
        }
        else {
          val lowIndex = result.get
          assert(0 <= lowIndex)
          assert(lowIndex < n)
          data(lowIndex) should be >= (low)
          if(0 < lowIndex)
            data(lowIndex - 1) should be < (low)
        }
        val nofAllowedComparisons = log2ceil(n)+1
        withClue("Too many comparisons: ") {
          nofComparisons should be <= (nofAllowedComparisons)
        }
      }
    }
  }
}
