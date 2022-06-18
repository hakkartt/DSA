package tests
import org.scalatest._
import scala.collection.mutable.ArrayBuffer

import cutVertices._

class CutVerticesSpec extends FlatSpec with Matchers {
  val smallTests: Seq[(Int, Seq[(Int,Int)], Set[Int])] = List(
    (5, List((0,1),(1,2),(2,0),(0,3),(3,4),(4,0)), Set(0))
    ,(5, List((0,1),(1,2),(2,0),(0,3),(3,4),(4,1)), Set())
    ,(6, List((0,1), (1,2), (2,3), (3,1), (3,4), (4,5), (5,2)), Set(1))
    ,(14, List((0,1),(0,2),(1,3),(2,3),(3,4),(4,5),(5,7),(5,8),(5,11),(7,9),(7,10),(8,12),(8,13),(9,13)), Set(3, 4, 5, 7, 8))
  )

  def diamondGraph(n: Int): (Graph, Set[Int]) = {
    require(n >= 1)
    val edges = new ArrayBuffer[(Int,Int)]()
    var latest = 0
    for (i <- 1 to n) {
      edges += ((latest, latest+1))
      edges += ((latest, latest+2))
      latest += 3
      edges += ((latest-2, latest))
      edges += ((latest-1, latest))
    }
    val g = new Graph(latest+1, edges)
    val correct = (3 until latest by 3).toSet
    (g, correct)
  }

  "The cutVerticesSlow method" should "work correctly 1" in {
    for((nofVertices, edges, correct) <- smallTests) {
      withClue(s"on Graph($nofVertices, $edges): ") {
        val g = new Graph(nofVertices, edges)
        g.cutVerticesSlow should be (correct)
      }
    }
  }
  it should "work correctly 2" in {
    val (g, correct) = diamondGraph(1000)
    g.cutVerticesSlow should be (correct)
  }

  "The cutVertices method" should "work correctly 1" in {
    for((nofVertices, edges, correct) <- smallTests) {
      withClue(s"on Graph($nofVertices, $edges): ") {
        val g = new Graph(nofVertices, edges)
        g.cutVertices should be (correct)
      }
    }
  }
  it should "work correctly 2" in {
    val (g, correct) = diamondGraph(100)
    g.cutVertices should be (correct)
  }
  it should "work correctly 3" in {
    val g = new Graph(60, List((11,53), (9,48), (1,58), (56,23), (44,6), (17,49), (56,9), (54,14), (21,22), (2,29), (41,15), (51,55), (56,52), (17,52), (54,39), (31,37), (36,21), (53,13), (40,42), (25,51), (5,20), (6,56), (35,32), (3,17), (55,57), (47,10), (47,5), (40,33), (0,41), (43,3), (35,50), (35,42), (50,2), (38,59), (12,16), (33,0), (24,19), (52,25), (20,10), (37,34), (59,54), (8,40), (18,38), (23,47), (15,1), (7,14), (51,10), (49,11), (59,5), (13,49), (4,7), (57,4), (28,50), (14,28), (10,31), (27,36), (58,44), (29,26), (34,30), (57,32), (57,45), (22,8), (32,46), (19,43), (39,12), (16,35), (26,27), (42,24)))
    val correct = Set(10, 56, 37, 57, 38, 9, 32, 34, 17, 59, 49, 31)
    g.cutVertices should be (correct)
  }
  it should "be non-recursive" in {
    val (g, correct) = diamondGraph(10000)
    try {
      g.cutVertices should be (correct)
    } catch {
      case e: java.lang.StackOverflowError => {true should be (false)}
    }
  }
  it should "be efficient" in {
    val (g, correct) = diamondGraph(1000)
    val (slowSol, slowTime) = timer.measureCpuTime {g.cutVerticesSlow }
    val (sol, time) = timer.measureCpuTime {g.cutVertices }
    sol should be (slowSol)
    println(f"Time slow vs yours: $slowTime%.3g vs $time%.3g")
    time should be <= (0.1 * slowTime)
  }
}
