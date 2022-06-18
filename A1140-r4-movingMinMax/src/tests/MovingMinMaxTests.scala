package tests
import org.scalatest._

import scala.util.Random
import movingMinMax._

class MovingMinMaxSpec extends FlatSpec with Matchers {
  val rand = new scala.util.Random(2016)

  "The FastFilter" should "work correctly" in {
    val n = 10000
    val windowSize = 1000
    val m = 2000
    val slowFilter = new SlowFilter[Int](windowSize)
    val fastFilter = new FastFilter[Int](windowSize)
    for(i <- 0 until n) {
      val value = rand.nextInt(m)
      val (slowMin, slowMax) = slowFilter.insert(value)
      val (fastMin, fastMax) = fastFilter.insert(value)
      fastMin should be (slowMin)
      fastMax should be (slowMax)
    }
  }

  it should "be at least five times faster than the slow version on window of size 1000" in {
    val n = 100000
    val windowSize = 1000
    val m = Int.MaxValue
    val values = Array.tabulate[Int](n)(i => rand.nextInt(m))

    println("Doing performance evaluation on SlowFilter")
    val correctMins = new Array[Int](n)
    val correctMaxs = new Array[Int](n)
    val (_, slowTime) = timer.measureCpuTime {
      val slowFilter = new SlowFilter[Int](windowSize)
      for(i <- 0 until n) {
        val (min,max) = slowFilter.insert(values(i))
        correctMins(i) = min
        correctMaxs(i) = max
      }
    }
    println(f"SlowFilter time: $slowTime%.3g")

    println("Doing performance evaluation on FastFilter")
    val (_, fastTime) = timer.measureCpuTime {
      val fastFilter = new FastFilter[Int](windowSize)
      for(i <- 0 until n) {
        val (min, max) = fastFilter.insert(values(i))
        min should be (correctMins(i))
        max should be (correctMaxs(i))
      }
    }
    println(f"FastFilter time: $fastTime%.3g")

    val speedup = slowTime / fastTime
    println(f"Speedup: $speedup%.2f")
    speedup should be >= 5.0
  }
}
