/* Author: Tommi Junttila, Aalto University.
 * Only for the use on the Aalto course CS-A1140.
 * Redistribution not allowed.
 */

package graphs
import scala.collection.mutable.Stack
import scala.collection.mutable.Map

/**
 * A mutable data structure for undirected graphs
 * (self-loops are not allowed).
 *
 * Please observe that the data structures and the code provided here
 * are not the most efficient ones but kept simple for the sake
 * of clarity and learning graph traversal algorithms.
 */
final class Graph[V] {
  type VertexSet = scala.collection.mutable.HashSet[V]
  /* The vertices are collected here */
  private val vertices = new VertexSet()
  /* Each vertex in the graph is associated to a set of neighbours here */
  private val neighbours = scala.collection.mutable.HashMap[V, VertexSet]()

  /**
   * A string representation of the graph.
   * Probably useful only for debugging small graphs.
   */
  override def toString: String = {
    vertices.map(v => "" + v + " -> {" + (neighbours(v).mkString(",")) + "}").mkString("\n")
  }


  /**
   * Include the vertex v in the graph.
   * If it is already in the graph, nothing happens.
   */
  def addVertex(v: V): Unit = {
    if (!vertices.contains(v)) {
      vertices(v) = true
      neighbours(v) = new VertexSet()
    }
  }

  /**
   * Remove the vertex v from the graph.
   * If it is not in the graph, nothing happens.
   */
  def removeVertex(v: V): Unit = {
    if (vertices.contains(v)) {
      vertices(v) = false
      neighbours.remove(v)
    }
  }

  /**
   * Does the graph include vertex v?
   */
  def hasVertex(v: V): Boolean = vertices.contains(v)

  /**
   * Get the vertices in the graph.
   * A new copy is returned so that the actual vertex set cannot be modified accidentally.
   */
  def getVertices: scala.collection.immutable.Set[V] = vertices.toSet

  /**
   * The size of the graph, i.e. the number of vertices in it.
   */
  def size = vertices.size

  /**
   * Add an edge between the vertices v1 and v2 in this graph.
   * If v1 or v2 is not in the graph yet, it will be included in it.
   */
  def addEdge(v1: V, v2: V): Unit = {
    require(v1 != v2, "self-loops are not allowed")
    if (!hasVertex(v1)) addVertex(v1)
    if (!hasVertex(v2)) addVertex(v2)
    neighbours(v1)(v2) = true
    neighbours(v2)(v1) = true
  }

  /**
   * Is there an edge between v1 and v2 in this graph?
   */
  def hasEdge(v1: V, v2: V): Boolean =
    (vertices.contains(v1) && neighbours(v1)(v2) == true)

  /**
   * Remove the edge between vertices v1 and v2.
   * If the edge is not in the graph, nothing happens.
   */
  def removeEdge(v1: V, v2: V): Unit = {
    if (vertices.contains(v1) && vertices.contains(v2)) {
      neighbours(v1)(v2) = false
      neighbours(v2)(v1) = false
    }
  }

  /**
   * The degree of a vertex, i.e. the number of edges incident to it.
   */
  def degree(v: V) = neighbours(v).size

  /**
   * The maximum degree of the graph, i.e. the largest degree of the vertices in it
   */
  def maxDegree = vertices.map(v => degree(v)).max


  /**
   * Return a path from the "source" vertex to a vertex satisfying
   * the predicate "targetPred" (None if no such path exists).
   * Uses a variant of depth-first search.
   * Written in imperative style with recursion, is not tail recursive.
   */
  def pathFromDFS(source: V, targetPred: V => Boolean): Option[Seq[V]] = {
    // When visiting a vertex, mark it here so that it does
    // not get visited again
    val visited = scala.collection.mutable.HashSet[V]()
    // The path is constructed here
    var path: List[V] = Nil
    // The recursive search routine; return true if a path to a vertex
    // where the predicate holds is found
    def visit(v: V): Boolean = {
      // Already visited?
      if (visited(v))
        return false
      // No, visit v
      visited(v) = true
      // Check if this is a vertex where the predicate holds
      if (targetPred(v)) {
        // The predicate holds, initialize the path to end in v
        path = List(v)
        return true
      }
      // The predicate does not hold, search the neighbours.
      // The iterator construction in the next three lines is shown as
      // an example for you: it performs the same as
      // "for (neighbour <- neighbours(v)) {" would.
      val neighbourIterator = neighbours(v).iterator
      while(neighbourIterator.hasNext) {
        val neighbour = neighbourIterator.next
        if (visit(neighbour)) {
          // We have found a path from v to a vertex with
          // the predicate holding, augment path with v and return
          path = v :: path
          return true
        }
      }
      // Could not find a path to vertex with the predicate holding
      return false
    }
    // Do the recursive search for a path
    if (visit(source))
      Some(path)
    else
      None
  }

  /**
   * Perform *exactly* the same search as pathFromDFS but
   * without using recursion.
   * That is, the sequence of "targetPred" calls should be
   * exactly the same and the returned path should be the same as well.
   * When removing recursion, you need to remember which neighbours
   * of a vertex you have already traversed and which are yet to
   * be traversed: use an iterator for this (see the code in pathFromDFS).
   *
   * You may assume that the foreach method and an iterator on
   * a scala.collection.mutable.hashSet object traverse the set in
   * the same order (unless perhaps you modify the set during
   * the traversal, which you should of course NOT do).
   */
  def pathFromDFSNonrec(source: V, targetPred: V => Boolean): Option[Seq[V]] = {
    // When visiting a vertex, mark it here so that it does
    // not get visited again
    val visited = scala.collection.mutable.HashSet[V]()
    // The path is constructed here
    var path: List[V] = Nil

    val s = new Stack[V]().push(source)
    val previous = Map[V, V]()
    var l = List[V]()
    
    def fetchPath(from: V) = {
      var i = from
      while (i != source) {
        l = l.::(i)
        i = previous(i)
      }
      l.::(i)
    }
    
    while (s.nonEmpty) {
      val top = s.pop
      if (!visited(top)) {
        visited(top) =  true
        if (targetPred(top)) {
          return Some(fetchPath(top))
        }
        val N = this.neighbours(top).toList.reverse
        for (j <- N) {
          if (!visited(j)) {
            s.push(j) 
            previous.+=((j,top))
          }
        }
      }
    }
    None
  }
}

object Graph {
  /**
   * Build an empty undirected graph with no vertices or edges.
   */
  def apply[V]() = new Graph[V]()

  /**
   * Build an undirected graph from a set of edges.
   */
  def apply[V](edges: Traversable[(V, V)]): Graph[V] = {
    val g = new Graph[V]
    edges.foreach({ case (v1, v2) => g.addEdge(v1, v2) })
    g
  }

  /**
   * Build an undirected graph from a map that associates vertices to sets of neighbours.
   */
  def apply[V](neighbourSets: Map[V, Traversable[V]]): Graph[V] = {
    val g = new Graph[V]
    neighbourSets.foreach({
      case (v, neighbours) => {
        g.addVertex(v)
        neighbours.foreach(n => { g.addVertex(n); g.addEdge(v, n) })
      }
    })
    g
  }
}
