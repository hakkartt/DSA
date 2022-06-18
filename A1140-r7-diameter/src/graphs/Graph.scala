/* Author: Tommi Junttila, Aalto University.
 * Only for the use on the Aalto course CS-A1140.
 * Redistribution not allowed.
 */

package graphs
import scala.collection.mutable.ArrayBuffer

/**
 * A simple immutable class for undirected graphs.
 * The vertices are integers from 0 to nofVertices-1.
 * The edges are of form (vertex1, vertex2).
 * Self-loops and parallel edges between two vertices are not supported.
 */
class Graph(val nofVertices: Int, edges: Seq[(Int,Int)]) {
  require(nofVertices > 0)

  /**
   * neighbours(u) is the list of vertices v such that {u,v} is an edge.
   */
  val neighbours = Array.tabulate[ArrayBuffer[Int]](nofVertices)(i => new ArrayBuffer[Int]())

  /* Validate the input and build the neighbours data structure.
   * Duplicate edges are discarded at this point. */
  private def init(edges: Seq[(Int,Int)]) = {
    val seen = scala.collection.mutable.HashSet[(Int,Int)]()
    for((vertex1, vertex2) <- edges if !seen((vertex1, vertex2))) {
      require(0 <= vertex1 && vertex1 < nofVertices)
      require(0 <= vertex2 && vertex2 < nofVertices)
      neighbours(vertex1) += vertex2
      neighbours(vertex2) += vertex1
      seen((vertex1,vertex2)) = true
      seen((vertex2,vertex1)) = true
    }
  }
  init(edges)

  /**
   * Get the degree of a vertex.
   */
  def degree(v: Int): Int = {
    require(0 <= v && v < nofVertices)
    neighbours(v).length
  }

  /**
   * Get the maximum degree.
   * Constant time operation after the initialization phase.
   */
  val maxDegree = {
    (0 until nofVertices).map(v => degree(v)).max
  }

  /* Is the graph connected?
   * As the graph data structure here is immutable and we use
   * connectedness as a requirement in many places,
   * compute this information once in the beginning. */
  val isConnected: Boolean = {
    if(nofVertices == 0) true
    else {
      // A simple DFS from the vertex 0
      val seen = new Array[Boolean](nofVertices)
      val q = new scala.collection.mutable.Queue[Int]()
      q.enqueue(0)
      seen(0) = true
      var nofSeen = 1
      while(q.nonEmpty) {
        val v: Int = q.dequeue
        for(w <- neighbours(v) if !seen(w)) {
          q.enqueue(w)
          seen(w) = true
          nofSeen += 1
        }
      }
      nofSeen == nofVertices
    }
  }

  /**
   * The eccentricity of a vertex v is the greatest minimum distance
   * between v and any other vertex.
   * It can be thought of as how far a vertex is from the vertex most distant
   * from it in the graph.
   * Hint: perform a breadth-first search starting from v and
   *       report the longest distance encounted
   */
  def eccentricity(v: Int): Int = {
    require(0 <= v && v < nofVertices)
    require(isConnected, "Eccentricity is only defined for connected graphs")
    
    // Save the visited vertices into an array
    val arr = Array.fill(this.nofVertices)(false, -1)
    arr(v) = (true,0)
    // Find neighbours to visit
    var n = this.neighbours(v).filter(n => arr(n)._1 == false)
    // Initialize variable for depth
    var d = 0
    // breahdth-first...
    while (!n.isEmpty) {
      n.foreach( n => arr(n) = (true, d+1) )
      val x = ArrayBuffer[Int]()
      n.foreach( n => x.++=( this.neighbours(n) ) )
      n = x.filter(n => arr(n)._1 == false)
      d += 1
    }
    // report the longest distance counted
    arr.map(_._2).max
  }

  /**
   * The diameter of a connected graph is the length of
   * the longest shortest path between vertices in the graph.
   * That is, it is the maximum number of edges one has to traverse
   * in order to reach a vertex from another.
   * Hint: this method is possibly a one-liner once the "eccentricity"
   * method has been mplemented.
   */
  def diameter: Int = {
    require(isConnected, "Only connected graphs have a finite diameter")
    Range
    .apply( 0, this.nofVertices - 1 )
    .toArray.map( n => eccentricity(n) )
    .max
  }

  /**
   * Output the graph in the JSON (JavaScript Object Notation,
   * http://www.json.org/) format that our simple visualizer understands.
   * - 'names' gives the name for each vertex, "" is used if missing for a vertex
   * - 'coloring' is a mapping that associates vertices in the graph to
   *   a Double in the range [-1.0,1.0]; if some vertex is not associated to
   *   any value by coloring, it is implicitly associated to 0.0.
   */
  def toJSON(names: Map[Int, String] = Map[Int, String](),
             coloring: Map[Int, Double] = Map[Int, Double]()): String = {
    coloring.foreach({ case (v, c) => require(-1.0 <= c && c <= 1.0, "The colors should be in range [-1.0,1.0] but the vertex '" + v + "' has color " + c) })
    def name(v: Int) = names.getOrElse(v, v.toString)
    def color(v: Int) = coloring.getOrElse(v, 0.0)
    val vString = "  " + (0 until nofVertices).map(v => s"""{"name":"${name(v)}","group":${color(v)}}""").mkString(",\n  ")
    val lString = "  " + (for (v <- 0 until nofVertices; w <- neighbours(v) if w <= v) yield (
      s"""{"source":${v},"target":${w},"value":1}""")).mkString(",\n  ")
    "{ \"nodes\":[\n" + vString + "\n], \"links\":[\n" + lString + "\n]}"
  }
}
