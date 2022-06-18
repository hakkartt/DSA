package tests
import org.scalameter.api._
import org.scalameter.picklers.Implicits._

import radixSort._

object RadixSortBenchmarking extends Bench.OfflineReport {
  val sizes = Gen.range("size")(50000, 2000000, 50000)

  val nofSamples = 1
  val nofRuns = 11
  val rand = new scala.util.Random
  var l = new Array[Int](1)
  def initArray(size: Int) = {
    l = new Array[Int](size)
    for (i <- 0 until size) l(i) = rand.nextInt(Int.MaxValue)
  }

  performance of "running time" in {
    measure method "radix-vs-standard" config (
      exec.benchRuns -> nofRuns,
      exec.independentSamples -> nofSamples) in {

      using(sizes) curve ("radix sort") warmUp {
        initArray(1000000)
        radixSort.lsdfRadixSort(l)
      } setUp {
        n => initArray(n)
      } in {
        n => radixSort.lsdfRadixSort(l)
      }

      using(sizes) curve ("java.util.Arrays.sort") warmUp {
        initArray(1000000)
        java.util.Arrays.sort(l)
      } setUp {
        n => initArray(n)
      } in {
        n => java.util.Arrays.sort(l)
      }
    }
  }
}
