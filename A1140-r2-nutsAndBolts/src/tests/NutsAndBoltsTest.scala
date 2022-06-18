package tests
import org.scalatest._

import scala.util.Random
import nutsAndBolts._

class MatcherSpec extends FlatSpec with Matchers {
  var nofCompares = 0

  // A concrete class of nuts for testing
  // The number of comparisons is recorded for evaluation purposes
  protected case class EuroNut(name: String, size: Int) extends Nut(name) {
    override def compare(bolt: Bolt): Int = {
      nofCompares += 1
      bolt match {
        case EuroBolt(boltName, boltSize) => {
          if(size < boltSize) -1
          else if(size == boltSize) 0
          else 1
        }
      }
    }
  }
  protected case class EuroBolt(name: String, size: Int) extends Bolt(name) { 
  }

  val rand = new Random()

  /* A small helper for generating random Euro nuts and bolts */
  def randomNutsAndBolts(n: Int, maxSize: Int) = { 
    val sizes = (1 to n).map(i => rand.nextInt(maxSize))
    val nuts = rand.shuffle(sizes).zipWithIndex.map({case (s,i) => EuroNut("n"+i, s)}).toVector
    val bolts = rand.shuffle(sizes).zipWithIndex.map({case (s,i) => EuroBolt("b"+i, s)}).toVector
    (nuts, bolts)
  }

  "The slow matcher" should "work correctly" in {
    val nofTests = 1000
    for(test <- 1 to nofTests) {
      val N = 5 + rand.nextInt(20)
      val M = 5 + rand.nextInt(N)
      val (nuts, bolts) = randomNutsAndBolts(N, M)
      var result = matcher.slow(nuts, bolts)
      withClue("When nuts="+nuts+", bolts="+bolts+" and result="+result+":") {
        result.map(_._1).toSet should be (nuts.toSet)
        result.map(_._2).toSet should be (bolts.toSet)  
        result.foreach(p => p._1.compare(p._2) should be (0))
      }
    }
 }

  "The fast matcher" should "work correctly" in {
    val nofTests = 1000
    for(test <- 1 to nofTests) {
      val N = 5 + rand.nextInt(20)
      val M = 5 + rand.nextInt(N)
      val (nuts, bolts) = randomNutsAndBolts(N, M)
      var result = matcher.fast(nuts, bolts)
      withClue("When nuts="+nuts+", bolts="+bolts+" and result="+result+":") {
        // The length should match
        result.length should be (nuts.length)
        // All the nuts should be included
        result.map(_._1).toSet should be (nuts.toSet)
        // All the bolts should be included
        result.map(_._2).toSet should be (bolts.toSet)  
        // The pairs should be matching
        result.foreach(p => p._1.compare(p._2) should be (0))
      }
    }
  }

  it should "be ten times faster than the quadratic time algorithm on largish instances and only use at most 4*N*log(N) comparisons" in {
    val nofTests = 10
    val N = 40000
    val M = 60000
    var slowCumuTime = 0.0
    var fastCumuTime = 0.0
    var fastCompares = 0
    for(test <- 1 to nofTests) {
      val (nuts, bolts) = randomNutsAndBolts(N, M)
      val (slowResult, slowTime) = timer.measureCpuTime { matcher.slow(nuts, bolts) }
      slowCumuTime += slowTime
      nofCompares = 0
      val (fastResult, fastTime) = timer.measureCpuTime { matcher.fast(nuts, bolts) }
      fastCumuTime += fastTime
      val maxComparisons = (4*N*math.log(N)/math.log(2)).toInt
      println(f"Fast algorithm comparisons: $nofCompares of $maxComparisons allowed")
      nofCompares should be <= (maxComparisons)
      fastCompares += nofCompares
      fastResult.length should be (nuts.length)
      fastResult.map(_._1).toSet should be (nuts.toSet)
      fastResult.map(_._2).toSet should be (bolts.toSet)    
      fastResult.foreach(p => p._1.compare(p._2) should be (0))
      println(f"Fast vs slow: $fastTime%.3g vs $slowTime%.3g")
      Console.out.flush()
    }
    println(f"Fast vs slow, cumulative: $fastCumuTime%.3g vs $slowCumuTime%.3g")
    fastCumuTime should be < (0.1 * slowCumuTime)
  }
}
