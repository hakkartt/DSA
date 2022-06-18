package tests
import org.scalatest._

import scala.util.Random

class QuickSelectSpec extends FlatSpec with Matchers {
  "Your quickselect implementation" should "compute correct results on some selected inputs" in {
    val tests = List(Seq(1)
                     ,Seq(1,1,1,1)
                     ,Seq(1,2,3)
                     ,Seq(3,2,1)
                   )
    for(t <- tests; i <- 0 until t.length) {
      println("Testing quickSelect.find("+t+", "+i+")")
      val result = quickSelect.find(t, i)
      val correct = t.sorted.apply(i)
      result should be (correct)
    }
  }
  it should "compute correct results on some randomized inputs" in {
    val N = 100
    val M = 1000
    val nofTests = 1000
    val rand = new scala.util.Random
    for(t <- 1 to nofTests) {
      //println("Test "+t)
      val s = Random.shuffle(Seq.tabulate[Int](N)(j => rand.nextInt(M)))
      val i = rand.nextInt(N)
      val result = quickSelect.find(s, i)
      val correct = s.sorted.apply(i)
      result should be (correct)
    }
  }

  it should "be twice as fast as the approach based on applying scala.util.Arrays.sort on sequences of 1000000 integers" in {
    val rand = new scala.util.Random
    val N = 1000000
    var cumuQuickSelect = 0.0
    var cumuArraysSort = 0.0
    val nofTests = 10
    println("Running performance tests")
    for(t <- 1 to nofTests) {
      val i = rand.nextInt(N)
      val s = scala.util.Random.shuffle(IndexedSeq.tabulate[Int](N)(j => rand.nextInt()))
      val (valueArraysSort, timeArraysSort) = timer.measureCpuTime {
        val tmp = s.toArray
        java.util.Arrays.sort(tmp)
        tmp(i)
      }
      println(" Test "+t)
      val (valueQuickSelect,timeQuickSelect) = timer.measureCpuTime {quickSelect.find(s, i)}
      valueQuickSelect should be (valueArraysSort)
      println(f"  quickSelect vs Arrays.sort: $timeQuickSelect%.3g vs $timeArraysSort%.3g")
      cumuQuickSelect += timeQuickSelect
      cumuArraysSort += timeArraysSort
    }
    val avgQuickSelect = cumuQuickSelect / nofTests
    val avgArraysSort = cumuArraysSort / nofTests
    println(f" Average: $avgQuickSelect%.3g vs $avgArraysSort%.3g")
    avgQuickSelect should be < (0.5*avgArraysSort)
  }

  it should "be as fast as the approach based on applying scala.util.Arrays.sort on almost-sorted sequences of 1000000 integers" in {
    val rand = new scala.util.Random
    val N = 1000000
    var cumuQuickSelect = 0.0
    var cumuArraysSort = 0.0
    val nofTests = 10
    println("Running performance tests on almost-sorted sequences")
    for(t <- 1 to nofTests) {
      val i = rand.nextInt(N)
      val base = rand.nextInt(2*N)-N      
      val s1 = Array.tabulate[Int](N)(j => j + base)
      def swap(i: Int, j: Int) {val tmp = s1(i); s1(i) = s1(j); s1(j) = tmp }
      for(f <- 1 to N/100) {
        swap(rand.nextInt(N), rand.nextInt(N))
      }
      val s = s1.toIndexedSeq
      val (valueArraysSort, timeArraysSort) = timer.measureCpuTime {
        val tmp = s.toArray
        java.util.Arrays.sort(tmp)
        tmp(i)
      }
      println(" Test "+t)
      val (valueQuickSelect,timeQuickSelect) = timer.measureCpuTime {quickSelect.find(s, i)}
      println(f"  quickSelect vs Arrays.sort: $timeQuickSelect%.3g vs $timeArraysSort%.3g")
      cumuQuickSelect += timeQuickSelect
      cumuArraysSort += timeArraysSort
    }
    val avgQuickSelect = cumuQuickSelect / nofTests
    val avgArraysSort = cumuArraysSort / nofTests
    println(f" Average: $avgQuickSelect%.3g vs $avgArraysSort%.3g")
    avgQuickSelect should be <= (1.0*avgArraysSort)
  }

  it should "be as fast as the approach based on applying scala.util.Arrays.sort on sequences of 1000000 small integers (=> lots of same values in the array)" in {
    val rand = new scala.util.Random
    val N = 1000000
    val M = 21
    var cumuQuickSelect = 0.0
    var cumuArraysSort = 0.0
    val nofTests = 10
    println("Running performance tests on sequences with few values only")
    for(t <- 1 to nofTests) {
      val i = rand.nextInt(N)
      val s = scala.util.Random.shuffle(IndexedSeq.tabulate[Int](N)(j => rand.nextInt(M)))
      val (valueArraysSort, timeArraysSort) = timer.measureCpuTime {
        val tmp = s.toArray
        java.util.Arrays.sort(tmp)
        tmp(i)
      }
      println(" Test "+t)
      val (valueQuickSelect,timeQuickSelect) = timer.measureCpuTime {quickSelect.find(s, i)}
      println(f"  quickSelect vs Arrays.sort: $timeQuickSelect%.3g vs $timeArraysSort%.3g")
      cumuQuickSelect += timeQuickSelect
      cumuArraysSort += timeArraysSort
    }
    val avgQuickSelect = cumuQuickSelect / nofTests
    val avgArraysSort = cumuArraysSort / nofTests
    println(f" Average: $avgQuickSelect%.3g vs $avgArraysSort%.3g")
    avgQuickSelect should be <= (1.0*avgArraysSort)
  }
}
