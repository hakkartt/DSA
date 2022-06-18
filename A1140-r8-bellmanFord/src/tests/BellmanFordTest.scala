package tests
import org.scalatest._

import scala.util.Random
import bellmanFord._

class BellmanFordSpec extends FlatSpec with Matchers {

  "The Bellman-Ford algorithm" should "work correctly" in {
    // List of (nofVertices, edges, dist), where dist is
    // - a pair (v,d) such that d is the distance of v from the vertex 0, or
    // - None if a negative cycle is reachable from the vertex 0
    val testCases: Seq[(Int, Seq[(Int,Int,Int)], Option[(Int,Int)])] =
      List((3, List((0,2,1),(1,3,2),(2,-5,0)), Some((2, 5)))
           ,(3, List((0,2,1),(1,3,2),(2,-6,0)), None)
           ,(10, List((0,0,1), (0,0,2), (0,0,3), (0,0,4), (0,0,5), (0,0,6), (0,0,7), (0,0,8), (0,0,9), (1,-1,3), (8,10,5), (2,-1,6), (3,-1,2), (5,3,7), (6,-3,1)), None)
           ,(10, List((0,0,1), (0,0,2), (0,0,3), (0,0,4), (0,0,5), (0,0,6), (0,0,7), (0,0,8), (0,0,9), (2,8,4), (2,-2,1), (4,9,9), (1,1,8), (9,-1,2), (5,6,3), (7,8,9), (8,-2,9), (8,9,4), (8,9,3), (5,-2,6), (3,9,8), (9,9,1), (5,10,3), (9,8,1), (2,5,6)), None)
           ,(10, List((0,0,1), (0,0,2), (0,0,3), (0,0,4), (0,0,5), (0,0,6), (0,0,7), (0,0,8), (0,0,9), (8,7,5), (7,3,2), (6,5,3), (8,-2,2), (8,2,3), (7,-1,4), (4,4,5), (3,-3,7), (2,4,8), (1,5,2), (4,-3,6), (2,2,3), (1,-1,6), (5,0,6), (8,9,5), (9,1,2)), None)
           ,(10, List((0,0,1), (0,0,2), (0,0,3), (0,0,4), (0,0,5), (0,0,6), (0,0,7), (0,0,8), (0,0,9), (2,-2,7), (6,2,1), (9,0,3), (8,9,3), (5,-3,9), (2,10,7), (7,1,9), (7,-1,9), (4,-2,7), (4,-3,6), (3,2,6), (5,8,1), (6,9,5), (3,1,2), (7,8,1), (2,3,1)), None)
         )
    for((nofVertices, edges, testDistance) <- testCases) {
      val graph = new Graph(nofVertices, edges)
      val result = graph.bellmanFord(0)
      withClue(s"On Graph($nofVertices, $edges): ") {
        result match {
          case graph.BellmanFordDistances(dists) => {
            testDistance should not be (None)
            val (testVertex, distance) = testDistance.get
            dists(testVertex) should be (distance)
            }
          case graph.BellmanFordNegativeCycle(cycle) => {
            testDistance should be (None)
            graph.isNegativeCycle(cycle) should be (true)
            }
        }
      }
    }
  }

  it should "work correctly 2" in {
    val testParameters = List(
      (1000, 2000, -3, 10, 57629, Some(365, -10))
      ,(1000, 2000, -3, 10, 87258, Some(926, -6))
      ,(1000, 2000, -3, 10, 82492, Some(123, -15))
      ,(1000, 2000, -3, 10, 16584, Some(5, -8))
      ,(1000, 2000, -3, 10, 43423, Some(710, -10))
      ,(1000, 2000, -3, 10, 75771, Some(842, -9))
      ,(1000, 2000, -3, 10, 70759, Some(693, -9))
      ,(1000, 2000, -3, 10, 38896, Some(650, -8))
      ,(1000, 2000, -3, 10, 75962, Some(44, -11))
      ,(1000, 2000, -3, 10, 85210, Some(856, -8))
      ,(1000, 2000, -3, 10, 57587, None)
      ,(1000, 2000, -3, 10, 64237, None)
      ,(1000, 2000, -3, 10, 54558, None)
      ,(1000, 2000, -3, 10, 50922, None)
      ,(1000, 2000, -3, 10, 48278, None)
      ,(1000, 2000, -3, 10, 86105, None)
      ,(1000, 2000, -3, 10, 41725, None)
      ,(1000, 2000, -3, 10, 55349, None)
      ,(1000, 2000, -3, 10, 18412, None)
      ,(1000, 2000, -3, 10, 72301, None)
    )
    for((nofVertices, nofEdges, minW, maxW, seed, testDistance) <- testParameters) {
      val edges = genRandom.gen(nofVertices, nofEdges, minW, maxW, seed)
      val graph = new Graph(nofVertices, edges)
      val result = graph.bellmanFord(0)
      withClue(s"On Graph($nofVertices, genRandom.gen($nofVertices, $nofEdges, $minW, $maxW, $seed)): ") {
        result match {
          case graph.BellmanFordDistances(dists) => {
            testDistance should not be (None)
              val (testVertex, distance) = testDistance.get
            dists(testVertex) should be (distance)
          }
          case graph.BellmanFordNegativeCycle(cycle) => {
            testDistance should be (None)
            graph.isNegativeCycle(cycle) should be (true)
          }
        }
      }
    }
  }

}
