package tests
import org.scalatest._

import timer._
import mst._

class MinimumSpanningTreeSpec extends FlatSpec with Matchers {

  def randomEdges(nofVertices: Int, nofEdges: Int, minW: Int, maxW: Int,
                  seed: Int): Seq[(Int, Int, Int)] = {
    require(nofEdges >= nofVertices)
    require(minW <= maxW)
    val rand = new PRNG(seed)
    def randWeight = rand.nextInt(maxW - minW + 1) + minW
    val edges = new scala.collection.mutable.ArrayBuffer[(Int, Int, Int)]()
    // Make sure that the graph is connected
    for(i <- 1 until nofVertices)
      edges += ((rand.nextInt(i), randWeight, i))
    // Add extra random edges
    while(edges.length < nofEdges) {
      val vertex1 = rand.nextInt(nofVertices)
      val vertex2 = rand.nextInt(nofVertices)
      if(vertex1 != vertex2)
        edges += ((vertex1, randWeight, vertex2))
    }
    edges.toSeq
  }

  "The minimumSpanningTree method" should "be correct" in {
    val tests = List((6, List((0, 1, 1), (0, 5, 2), (1, 7, 2), (2, 4, 3), (3, 10, 4), (3, 12, 5), (4, 2, 5)), 22)
                     ,(6, List((0, 1, 1), (0, 5, 2), (1, 7, 2), (3, 10, 4), (3, 12, 5), (4, 2, 5), (2, 3, 3)), 21)
                     ,(6, List((0, 3, 1), (1, 8, 2), (2, 9, 3), (3, 4, 4), (4, 17, 5), (5, 6, 0), (5, 5, 2)), 27)
                   )
    for((nofVertices, edges, correct) <- tests) {
      val g = new Graph(nofVertices, edges)
      val tree = g.minimumSpanningTree
      val result = g.isSpanningTree(tree)
      withClue(s"g = new Graph($nofVertices, $edges): tree=$tree: ") {
        result should not be (None)
        result.get should be (correct)
      }
    }
  }

  it should "correct and efficient" in {
    val tests = List((100000, 500000, 1, 100, 1, 1247648)
                     ,(100000, 500000, 1, 100, 2, 1243786)
                     ,(100000, 500000, 1, 100, 3, 1258528)
                     ,(100000, 500000, 1, 100, 4, 1245767)
                     ,(100000, 500000, 1, 100, 5, 1243253)
                     ,(100000, 500000, 1, 100, 6, 1246936)
                     ,(100000, 500000, 1, 100, 7, 1246272)
                     ,(100000, 500000, 1, 100, 8, 1251080)
                     ,(100000, 500000, 1, 100, 9, 1244499)
                     ,(100000, 500000, 1, 100, 10, 1250346)
                   )
    for((nofVertices, nofEdges, minCap, maxCap, seed, correct) <- tests) {
      println(s" Testing with edges = randomEdges($nofVertices, $nofEdges, $minCap, $maxCap, $seed)")
      val edges = randomEdges(nofVertices, nofEdges, minCap, maxCap, seed)
      val (g, initTime) = timer.measureCpuTime {new Graph(nofVertices, edges) }
      val (tree, time) = timer.measureCpuTime {g.minimumSpanningTree }
      val result = g.isSpanningTree(tree)
      withClue(s"When g = new Graph($nofVertices, randomEdges($nofVertices, $nofEdges, $minCap, $maxCap, $seed)):") {
        result should not be (None)
        result.get should be (correct)
        println(s"  Time: ${initTime + time}")
        (initTime + time) should be <= (3.0)
      }
   }
  }
}
