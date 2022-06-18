package tests
import org.scalatest._

import warmup._

class IsRemovedbyOneSpec extends FlatSpec with Matchers {
  val rand = new scala.util.Random

  def makeRandomString(n: Int, alphabet: Set[Char]): String = {
    require(n > 1)
    val a = new Array[Char](n)
    var i = 0
    val chars = alphabet.toIndexedSeq
    while(i < n) {
      val c = chars(rand.nextInt(chars.length))
      a(i) = c
      i += 1
    }
    a.mkString("")
  }
  def makeReducedString(n: Int, alphabet: Set[Char]): (String, String) = {
    require(n > 1)
    val s = makeRandomString(n, alphabet)
    val i = rand.nextInt(n)
    (s, s.take(i)+s.substring(i+1))
  }
  def makeMutatedReducedString(n: Int, alphabet: Set[Char], maxMutations: Int): (String, String) = {
    require(n > 1)
    val (s,t) = makeReducedString(n, alphabet)
    val indices = (0 until rand.nextInt(maxMutations)).map(i => rand.nextInt(n-1)).toSet
    val chars = alphabet.toIndexedSeq
    val t2 = t.toArray
    for(i <- indices) {
      val c = chars(rand.nextInt(chars.length))
      t2(i) = c
    }
    (s, t2.mkString(""))
  }

  "The isRemovedByOne method" should "work correctly" in {
    val nofTests = 100
    val alphabet = Set('a','b','c','d')
    for(test <- 1 to nofTests) {
      val (s,t) = makeMutatedReducedString(rand.nextInt(10)+2, alphabet, 2)
      withClue("On strings s="+s+" and t="+t+":") {
        val (refSol, refIndex) = isRemovedByOneSlow(s, t)
        val (sol, index) = isRemovedByOne(s, t)
        sol should be (refSol)
        if(sol) {
          s.take(index)+s.substring(index+1) should be (t)
        }
      }
    }
  }
  it should "be at least 100 times faster than the slow method on strings of size around 10000" in {
    val nofTests = 50
    val alphabet = Set('a','b','c','d')
    var refCumuTime = 0.0
    var cumuTime = 0.0
    for(test <- 1 to nofTests) {
      val (s,t) = makeMutatedReducedString(rand.nextInt(10)+10000, alphabet, 2)
      val ((refSol, refIndex), refTime) = timer.measureCpuTime {isRemovedByOneSlow(s, t)}
      val ((sol, index), time) = timer.measureCpuTime {isRemovedByOne(s, t) }
      println(f"slow vs yours: $refTime%.3g vs $time%.3g")
      refCumuTime += refTime
      cumuTime += time
      sol should be (refSol)
      if(sol) {
        s.take(index)+s.substring(index+1) should be (t)
      }
    }
    val speedup = refCumuTime / cumuTime
    println(f"Cumulative times:")
    println(f" isRemovedByOneSlow: $refCumuTime%.3f")
    println(f" isRemovedByOne:     $cumuTime%.3f")
    println(f" speedup: $speedup%.1f")
    speedup should be >= (100.0)
  }
}
