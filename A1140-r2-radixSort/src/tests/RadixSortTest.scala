package tests
import org.scalatest._

import scala.util.Random
import radixSort._

class LSDFRadixSortSpec extends FlatSpec with Matchers {
  val rand = new scala.util.Random

  "The lsdfRadixSort method" should "work correctly on arrays of non-negative integers" in {
    val nofTests = 200
    for(t <- 1 to nofTests) {
      val N = t
      val a = Array.tabulate[Int](N)(j => rand.nextInt(Int.MaxValue))
      val sorted = a.sorted
      lsdfRadixSort(a)
      for(i <- 0 until N)
        withClue("At index "+i+": ") { a(i) shouldBe sorted(i) }
    }
  }


  it should "be at least two times faster than the sorted-method on arrays of non-negative integers" in {
    val nofTests = 10
    val N = 1000000
    println(s"Comparing against 'sorted' on arrays of $N non-negative integers")
    var cumuStandard = 0.0
    var cumuRadix = 0.0
    val a = new Array[Int](N)
    val reference = new Array[Int](N)
    for(t <- 1 to nofTests) {
      // Build the test arrays
      for(i <- 0 until N) {
        reference(i) = rand.nextInt(Int.MaxValue)
        a(i) = reference(i)
      }
      val (sorted, t1) = timer.measureCpuTime {reference.sorted }
      val (_, t2) = timer.measureCpuTime {lsdfRadixSort(a) }
      cumuStandard += t1
      cumuRadix += t2
      for(i <- 0 until N)
        withClue("At index "+i+": ") { a(i) shouldBe sorted(i) }
    }
    val avgStandard = cumuStandard / nofTests
    val avgRadix = cumuRadix / nofTests
    val speedup = avgStandard / avgRadix
    println(f" sorted :  $avgStandard%.3g")
    println(f" radix:    $avgRadix%.3g")
    println(f" speed-up: $speedup%.3f")
    Console.out.flush()
    speedup should be >= (2.0)
  }

  /**
   * A helper for comparing java.util.Arrays.sort and the radix sort.
   */
  private def compareSortAndRadix(nofTests: Int, N: Int, allowNegative: Boolean = false) = {
    require(nofTests > 0)
    require(N > 0)
    var sortCumu = 0.0
    var radixCumu = 0.0
    val a = new Array[Int](N)
    val reference = new Array[Int](N)
    for(test <- 1 to nofTests) {
      for(i <- 0 until N) {
        reference(i) = if(allowNegative) rand.nextInt else rand.nextInt(Int.MaxValue)
        a(i) = reference(i)
      }
      val (_, t1) = timer.measureCpuTime { java.util.Arrays.sort(reference) }
      val (_, t2) = timer.measureCpuTime { lsdfRadixSort(a) }
      for(i <- 0 until N)
        withClue("At index "+i+": ") { a(i) shouldBe reference(i) }
      sortCumu += t1
      radixCumu += t2
    }
    (sortCumu / nofTests, radixCumu / nofTests)
  }

  /**
   * A helper for comparing java.util.Arrays.sort and the radix sort.
   */
  private def compareSortAndRadixFew(nofTests: Int, N: Int, values: IndexedSeq[Int]) = {
    require(nofTests > 0)
    require(N > 0)
    require(values.nonEmpty)
    var sortCumu = 0.0
    var radixCumu = 0.0
    for(t <- 1 to nofTests) {
      val reference = Array.tabulate[Int](N)(i => values(rand.nextInt(values.size)))
      val a = reference.clone()
      val (_, t1) = timer.measureCpuTime {java.util.Arrays.sort(reference) }
      val (_, t2) = timer.measureCpuTime {lsdfRadixSort(a) }
      sortCumu += t1
      radixCumu += t2
      for(i <- 0 until N)
        withClue("At index "+i+": ") { a(i) shouldBe reference(i) }
    }
    (sortCumu / nofTests, radixCumu / nofTests)
  }

  /*
   * Performance tests for smallish integer arrays.
   */
  it should s"be faster than java.util.Arrays.sort on smallish arrays of non-negative integers" in {
    val N = 10000
    println(s"Comparing against 'java.util.Arrays.sort' on arrays of $N non-negative integers")
    val nofTests = 100
    val (avgSort, avgRadix) = compareSortAndRadix(nofTests, N)
    val speedup = avgSort / avgRadix
    println(f" sort:     $avgSort%.3g")
    println(f" radix:    $avgRadix%.3g")
    println(f" speed-up: $speedup%.3f")
    Console.out.flush()
    speedup should be > (1.0)
  }

  it should s"be at most 3 times slower than java.util.Arrays.sort on smallish arrays of non-negative integers drawn from a small set" in {
    val N = 10000
    println(s"Comparing against 'java.util.Arrays.sort' on arrays of $N non-negative integers drawn from a small set")
    val nofTests = 100
    val M = 21
    val values = IndexedSeq.tabulate[Int](M)(j => rand.nextInt(Int.MaxValue))
    val (avgSort, avgRadix) = compareSortAndRadixFew(nofTests, N, values)
    val speedup = avgSort / avgRadix
    println(f" sort:     $avgSort%.3g")
    println(f" radix:    $avgRadix%.3g")
    println(f" speed-up: $speedup%.3f")
    Console.out.flush()
    speedup should be > (1.0 / 3.0)
  }

  it should s"be faster than java.util.Arrays.sort on smallish arrays of small integers" in {
    val N = 10000
    println(s"Comparing against 'java.util.Arrays.sort' on arrays of $N small non-negative integers")
    val nofTests = 100
    val M = 21
    val values = IndexedSeq.tabulate[Int](M)(i => i)
    val (avgSort, avgRadix) = compareSortAndRadixFew(nofTests, N, values)
    val speedup = avgSort / avgRadix
    println(f" sort:     $avgSort%.3g")
    println(f" radix:    $avgRadix%.3g")
    println(f" speed-up: $speedup%.3f")
    Console.out.flush()
    speedup should be > (1.0)
  }

  /*
   * Performance tests for larger integer arrays.
   */
  it should s"be faster than java.util.Arrays.sort on large arrays of non-negative integers" in {
    val N = 5000000
    println(s"Comparing against 'java.util.Arrays.sort' on arrays of $N non-negative integers")
    val nofTests = 10
    val (avgSort, avgRadix) = compareSortAndRadix(nofTests, N)
    val speedup = avgSort / avgRadix
    println(f" sort:     $avgSort%.3g")
    println(f" radix:    $avgRadix%.3g")
    println(f" speed-up: $speedup%.3f")
    Console.out.flush()
    speedup should be > (1.0)
  }

  it should s"be at most 3 times slower than java.util.Arrays.sort on large arrays of non-negative integers drawn from a small set" in {
    val N = 5000000
    println(s"Comparing against 'java.util.Arrays.sort' on arrays of $N non-negative integers drawn from a small set")
    val nofTests = 10
    val M = 21
    val values = IndexedSeq.tabulate[Int](M)(j => rand.nextInt(Int.MaxValue))
    val (avgSort, avgRadix) = compareSortAndRadixFew(nofTests, N, values)
    val speedup = avgSort / avgRadix
    println(f" sort:     $avgSort%.3g")
    println(f" radix:    $avgRadix%.3g")
    println(f" speed-up: $speedup%.3f")
    Console.out.flush()
    speedup should be > (1.0 / 3.0)
  }

  it should s"be faster than java.util.Arrays.sort on large arrays of small non-negative integers" in {
    val N = 5000000
    println(s"Comparing against 'java.util.Arrays.sort' on arrays of $N small non-negative integers")
    val nofTests = 10
    val M = 21
    val values = IndexedSeq.tabulate[Int](M)(i => i)
    val (avgSort, avgRadix) = compareSortAndRadixFew(nofTests, N, values)
    val speedup = avgSort / avgRadix
    println(f" sort:     $avgSort%.3g")
    println(f" radix:    $avgRadix%.3g")
    println(f" speed-up: $speedup%.3f")
    Console.out.flush()
    speedup should be > (1.0)
  }

  it should "work correctly on arrays of arbitrary integers" in {
    val nofTests = 200
    for(t <- 1 to nofTests) {
      val N = t
      val a = Array.tabulate[Int](N)(j => rand.nextInt)
      val sorted = a.sorted
      lsdfRadixSort(a)
      for(i <- 0 until N)
        withClue("At index "+i+": ") { a(i) shouldBe sorted(i) }
    }
  }

  it should s"be faster than java.util.Arrays.sort on large arrays of arbitrary integers" in {
    val N = 5000000
    println(s"Comparing against 'java.util.Arrays.sort' on arrays of $N arbitrary integers")
    val nofTests = 10
    val (avgSort, avgRadix) = compareSortAndRadix(nofTests, N, true)
    val speedup = avgSort / avgRadix
    println(f" sort:     $avgSort%.3g")
    println(f" radix:    $avgRadix%.3g")
    println(f" speed-up: $speedup%.3f")
    Console.out.flush()
    speedup should be > (1.0)
  }
}
