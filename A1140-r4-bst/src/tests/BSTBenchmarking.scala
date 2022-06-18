package tests
import org.scalameter.api._
import org.scalameter.picklers.Implicits._

import bst._

object BSTBenchmarks extends Bench.OfflineReport {

  val sizes = Gen.range("size")(10000, 200000, 10000)

  val nofSamples = 1
  val nofRuns = 11
  val rand = new scala.util.Random
  var l = new Array[Int](1)
  var nofInits = 0
  def initArray(size: Int) = {
    l = new Array[Int](size)
    for (i <- 0 until size) l(i) = rand.nextInt
  }
  performance of "OrderedSet" in {
    measure method "insert random integers" config (
      exec.benchRuns -> nofRuns,
      exec.independentSamples -> nofSamples) in {
      
      using(sizes) curve ("TreeSet") warmUp {
        initArray(100000)
        var i = 0
        val tree = new TreeSet[Int]()
        while(i < l.length) {
          tree.insert(l(i))
          i += 1
        }
      } setUp {
        n => initArray(n)
      } in {
        n => {
          var i = 0
          val tree = new TreeSet[Int]()
          while(i < l.length) {
            tree.insert(l(i))
            i += 1
          }
        }
      }

      
      using(sizes) curve ("java.util.TreeSet") warmUp {
        initArray(100000)
        var i = 0
        val tree = new java.util.TreeSet[Int]()
        while(i < l.length) {
          tree.add(l(i))
          i += 1
          }
      } setUp {
        n => initArray(n)
      } in {
        n => {
          var i = 0
          val tree = new java.util.TreeSet[Int]()
          while(i < l.length) {
            tree.add(l(i))
            i += 1
            }
        }
      }
    }
     
    var avlTree: TreeSet[Int] = null
    var treeSet: java.util.TreeSet[Int] = null
    
    measure method "search" config (
      exec.benchRuns -> nofRuns,
      exec.independentSamples -> nofSamples) in {

      using(sizes) curve ("TreeSet") warmUp {
        initArray(100000)
        var i = 0
        val tree = new TreeSet[Int]()
        while(i < l.length) {
          tree.insert(l(i))
          tree.search(l(i))
          i += 1
        }
      } setUp {
        n => {
          initArray(n)
          var i = 0
          avlTree = new TreeSet[Int]()
          while(i < l.length) {
            avlTree.insert(l(i))
            i += 1
          }
        }
      } in {
        n => {
          var i = 0
          while(i < l.length) {
            avlTree.search(l(i))
            i += 1
          }
        }
      }
      
      
      using(sizes) curve ("java.util.TreeSet") warmUp {
        initArray(100000)
        var i = 0
        val tree = new java.util.TreeSet[Int]()
        while(i < l.length) {
          tree.add(l(i))
          tree.contains(l(i))
          i += 1
        }
      } setUp {
        n => {
          initArray(n)
          var i = 0
          treeSet = new java.util.TreeSet[Int]()
          while(i < l.length) {
            treeSet.add(l(i))
            i += 1
          }
        }
      } in {
        n => {
          var i = 0
          while(i < l.length) {
            treeSet.contains(l(i))
            i += 1
          }
        }
      }
    }
  }
}
