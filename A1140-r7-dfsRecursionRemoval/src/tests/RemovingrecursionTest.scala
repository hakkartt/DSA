package tests
import org.scalatest._

import graphs._

class DfsRecursionRemovalSpec extends FlatSpec with Matchers {
  "The pathFromDFSNonrec method" should "work correctly 1" in {
    val g = wordGraphs.getGraph(wordGraphs.words3common)
    type VertexType = String
    val evalSequence = scala.collection.mutable.ArrayBuffer[VertexType]()
    def p(name: VertexType): VertexType => Boolean = n => {
      evalSequence.append(n)
      (n == name)
    }
    val pathDFS = g.pathFromDFS("say", p("had"))
    val dfsSequence = evalSequence.toList

    evalSequence.clear()

    val pathDFSNonrec = g.pathFromDFSNonrec("say", p("had"))
    val dfsNonrecSequence = evalSequence.toList

    withClue("Predicate evaluation orders should match:") {
      dfsNonrecSequence should be (dfsSequence)
    }
    withClue("The returned paths should match:") {
      pathDFSNonrec should be (pathDFS)
    }
  }

  it should "work correctly 2" in {
    val g = fbGraph.getGraph
    type VertexType = fbGraph.Person
    val evalSequence = scala.collection.mutable.ArrayBuffer[VertexType]()
    def p(target: VertexType): VertexType => Boolean = v => {
      evalSequence.append(v)
      (v == target)
    }
    val pathDFS = g.pathFromDFS(fbGraph.idToPerson(3981), p(fbGraph.idToPerson(4033)))
    val dfsSequence = evalSequence.toList

    evalSequence.clear()

    val pathDFSNonrec = g.pathFromDFSNonrec(fbGraph.idToPerson(3981), p(fbGraph.idToPerson(4033)))
    val dfsNonrecSequence = evalSequence.toList

    withClue("Predicate evaluation orders should match:") {
      dfsNonrecSequence should be (dfsSequence)
    }
    withClue("The returned paths should match:") {
      pathDFSNonrec should be (pathDFS)
    }
  }

  it should "work correctly 3" in {
    val g = biggerFbNetwork.getGraph
    val pathDFSNonrec = g.pathFromDFSNonrec(1, v => false)
    withClue("The returned paths should match:") {
      pathDFSNonrec.isEmpty should be (true)
    }
  }
}
