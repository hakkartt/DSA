package graphs
import java.io.{ File, FileReader, BufferedReader, IOException }

/**
 * http://snap.stanford.edu/data/egonets-Facebook.html
 */
object biggerFbNetwork {
  def getGraph: Graph[Int] = {
    val g = Graph[Int]()
    val reader = new BufferedReader(new FileReader(new File(new File("data"), "biggerFbNetwork.edges")))
    var line = reader.readLine()
    while (line != null) {
      line = line.trim()
      if (line.nonEmpty) {
        val data = line.split(' ').map(s => s.trim())
        g.addEdge(data(0).toInt, data(1).toInt)
      }
      line = reader.readLine()
    }
    g
  }
}
