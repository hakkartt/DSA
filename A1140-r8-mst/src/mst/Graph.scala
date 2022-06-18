/* Author: Tommi Junttila, Aalto University.
 * Only for the use on the Aalto course CS-A1140.
 * Redistribution not allowed.
 */

package mst
import scala.collection.mutable.ArrayBuffer

/**
 * If you want to (but you don't have to) use your own data structure classes,
 * such as union-find, put them here
 */

class UnionFind {
  /* Insert your internal data structures here */
  val map = scala.collection.mutable.HashMap[Int,(Option[Int], Int)]()
  var N = 0
  
  def sets: Int = N
  
  def set(n: Int): Boolean = {
    if (map.contains(n)) {
      false
    } else {
      map += (n -> (None, 0))
      N += 1
      true
    }
 
  }
  
  def set(array: Array[Int]): Unit = {
    array.foreach(n => this.set(n))
  }
  
  def find(n: Int): Int = {
    var prev = n
    var x = map(n)
    while (x._1 != None) {
      prev = x._1.get
      x = map(x._1.get)
    }
    prev
  }

  def merge(n1: Int, n2: Int): Unit = {
    val a = find(n1)
    val b = find(n2)
    val x = map(a)
    val y = map(b)

    if (a != b) {
      
      if (x._2 < y._2) {
        map(a) = (Some(b), x._2) 
      } 
      else {
        map(b) = (Some(a), y._2)
        if(x._2 == y._2) {
          map(a) = (x._1, x._2 + 1)
        }
      }
      
      N -= 1
    }
    
  }
  
}







/**
 * A simple immutable class for edge-weighted undirected graphs.
 * The vertices are integers from 0 to nofVertices-1.
 * The edges are of form (vertex1, weight, vertex2).
 * Self-loops are not supported but parallel edges between two vertices are.
 */
class Graph(val nofVertices: Int, edges: Seq[(Int,Int,Int)]) {
  require(nofVertices > 0)

  /**
   * neighbours(u) is the list of pairs (w,v) such {u,v} is an edge of weight w.
   */
  val neighbours = Array.tabulate[ArrayBuffer[(Int,Int)]](nofVertices)(i => new ArrayBuffer[(Int,Int)]())

  /* Validate the input and build the neighbours data structure.
   * Parallel and duplicate edges are allowed, self-loops are not. */
  private def init(edges: Seq[(Int,Int,Int)]) = {
    for((vertex1, w, vertex2) <- edges) {
      require(0 <= vertex1 && vertex1 < nofVertices)
      require(0 <= vertex2 && vertex2 < nofVertices)
      require(vertex1 != vertex2)
      neighbours(vertex1) += ((w,vertex2))
      neighbours(vertex2) += ((w,vertex1))
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
   * Get the maximum degree
   * Constant time operation after the initialization phase.
   */
  val maxDegree = {
    (0 until nofVertices).map(v => degree(v)).max
  }

  /* As the graph data structure here is immutable and we use
   * connectedness as a requirement in many places,
   * compute this information once in the beginning. */
  val isConnected: Boolean = {
    if(nofVertices == 0) true
    else {
      val seen = new Array[Boolean](nofVertices)
      val q = new scala.collection.mutable.Queue[Int]()
      q.enqueue(0)
      seen(0) = true
      var nofSeen = 1
      while(q.nonEmpty) {
        val u: Int = q.dequeue
        for((w,v) <- neighbours(u) if !seen(v)) {
          q.enqueue(v)
          seen(v) = true
          nofSeen += 1
        }
      }
      nofSeen == nofVertices
    }
  }


  /**
   * Find a minimum spanning tree (MST) of the graph.
   * Return the set of edges in the MST.
   */
  def minimumSpanningTree: Set[(Int,Int,Int)] = {
    require(isConnected)
    // sort edge seq
    val ss = edges.sortBy(_._2)
    // utilize UnionFind
    val x = new UnionFind
    x.set((0 to nofVertices -1).toArray)
    var cur = scala.collection.mutable.Set[(Int, Int, Int)]()
    
    // loop from...
    var i = 0
    while (x.sets > 1 && i < ss.size) {
      val n = ss(i)
      if (x.find(n._1) != x.find(n._3)) {
        x.merge(n._1, n._3)
        cur += n
      }
      i += 1
    }
    // convert to set
    cur.toSet
  }


  /**
   * Check if the edge set given as argument forms a spanning tree of the graph.
   * If this is not the case, return None.
   * If yes, return the sum of the weight of the edges in the set.
   */
  def isSpanningTree(treeEdges: Set[(Int,Int,Int)]): Option[Int] = {
    if(nofVertices == 0) {
      if(treeEdges.isEmpty) return Some(0)
      else return None
    }
    if(treeEdges.size != nofVertices - 1) return None

    val seen = new Array[Boolean](nofVertices)
    val q = new scala.collection.mutable.Queue[Int]()
    q.enqueue(0)
    seen(0) = true
    var nofSeen = 1
    var weight = 0
    while(q.nonEmpty) {
      val u: Int = q.dequeue
      for((w,v) <- neighbours(u) if !seen(v)) {
        if(treeEdges((v,w,u)) || treeEdges((u,w,v))) {
          q.enqueue(v)
          seen(v) = true
          nofSeen += 1
          weight += w
        }
      }
    }
    if(nofSeen != nofVertices) return None
    Some(weight)
 }

}
