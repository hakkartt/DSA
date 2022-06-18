package tests
import org.scalatest._

import graphs._

class DiameterSpec extends FlatSpec with Matchers {
  "The eccentricity method" should "work correctly 1" in {
    val (g, wordToIndex) = wordGraphs.getGraph(List("way", "say", "day", "was", "has", "his", "had", "him"))
    val indexToWord = wordToIndex.map({case (w,i) => (i,w)}).toMap
    g.eccentricity(wordToIndex("say")) should be (5)
    g.eccentricity(wordToIndex("was")) should be (3)
  }

  it should "work correctly 2" in {
    val (g, personToIndex) = fbGraph.getGraph
    val indexToPerson = personToIndex.map({case (w,i) => (i,w)}).toMap
    g.eccentricity(personToIndex("p4029")) should be (10)
    g.eccentricity(personToIndex("p4020")) should be (6)
  }

  it should "work correctly 3" in {
    val g = new Graph(60, List((54,53), (56,19), (22,14), (0,26), (48,24), (15,5), (53,36), (22,11), (30,27), (25,45), (10,32), (11,19), (31,30), (52,2), (34,1), (50,29), (45,57), (10,50), (29,46), (41,0), (6,47), (23,16), (3,38), (8,43), (9,23), (46,28), (27,58), (44,54), (13,35), (1,38), (1,3), (45,17), (56,44), (2,51), (50,45), (16,7), (49,9), (35,25), (43,4), (46,40), (7,52), (37,21), (39,55), (20,10), (30,19), (4,6), (12,34), (28,12), (18,20), (4,48), (58,13), (22,15), (24,37), (21,11), (11,33), (57,8), (55,18), (26,56), (51,31), (42,49), (59,41), (45,40), (38,49), (47,22), (3,42), (36,39), (43,38)))
    g.eccentricity(8) should be (12)
  }

  it should "work efficiently" in {
    println("Testing efficiency...")
    val (dummy, time) = timer.measureCpuTime {
      val g = mediumTestGraph.graph
      var v = 0
      while(v < g.nofVertices) {
        g.eccentricity(v) should be (mediumTestGraph.eccentricities(v))
        v += 1
      }
    }
    println(f"Efficiency test time: $time%.3g")
    time should be <= (5.0) // On any reasonably efficient modern computer
  }


  "The diameter method" should "work correctly 1" in {
    val(g, wordToIndex) = wordGraphs.getGraph(List("way", "say", "nay", "day", "was"))
    g.diameter should be (2)
  }
  it should "work correctly 2" in {
    val (g, wordToIndex) = wordGraphs.getGraph(List("way", "say", "day", "was", "has", "his", "had", "him"))
    g.diameter should be (5)
  }
  it should "work correctly 3" in {
    val (g, nameToIndex) = fbGraph.getGraph
    g.diameter should be (10)
  }
  it should "work correctly 4" in {
    val g = new Graph(60, List((26,43), (29,6), (48,24), (18,44), (41,10), (45,4), (57,30), (52,34), (7,35), (11,8), (15,18), (59,32), (50,41), (28,46), (33,50), (8,14), (38,46), (5,17), (57,47), (1,13), (4,3), (13,49), (50,20), (36,57), (49,36), (58,56), (25,37), (47,55), (9,15), (0,27), (38,28), (2,16), (44,29), (22,38), (12,2), (2,52), (20,12), (10,3), (30,54), (21,36), (58,39), (32,13), (46,30), (39,48), (10,47), (57,25), (23,42), (32,11), (6,7), (14,5), (29,26), (17,51), (40,53), (3,0), (19,22), (35,21), (51,45), (56,2), (31,9), (24,33), (27,31), (4,19), (43,1), (42,58), (53,59), (54,23), (46,3)))
    g.diameter should be (16)
  }
}
