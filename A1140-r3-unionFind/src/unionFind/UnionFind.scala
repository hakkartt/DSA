/* Author: Tommi Junttila, Aalto University.
 * Only for the use on the Aalto course CS-A1140.
 * Redistribution not allowed.
 */

package unionFind

object solver {
  /**
   * Given vertices {0,...,nofVertices-1} and weighted edges between them,
   * each edge of weight w between vertices v1 and v2 given as a triple
   * (v1, w, v2), find the largest weight W such that if we only consider
   * the edges of weight W or more, the graph stays connected (i.e., we can get
   * from any vertex to each other vertex).
   * Returns None if no such weight exists, i.e., if the graph is not connected
   * even if we consider all the edges given.
   * Provided that the union-find data structure is properly implemented and
   * includes either ranks or path compression (or both),
   * the running time of the algorithm is
   * O(nofVertices + |edges|*(log(|edges|) + log(nofVertices)))
   * on average.
   */
  def solve(nofVertices: Int, edges: Seq[(Int, Int, Int)]): Option[Int] = {
    require(nofVertices >= 1)
    edges.foreach({case (vertex1, weight, vertex2) =>
      require(0 <= vertex1 && vertex1 < nofVertices &&
              0 <= vertex2 && vertex2 < nofVertices &&
              weight >= 0)})
    // The sets of vertices 
    val sets = new UnionFind[Int]()
    // Let x be the sorted version of 'edges'
    val x = edges.sortBy(-_._2)
    
    var i = 0
    // Making separate sets of the vertices
    while(i < x.size) {
      sets.makeSet(x(i)._1)
      sets.makeSet(x(i)._3)
      i += 1
    }   
    i = 0
    // Merging vertices until running out of edges or the number of trees is equal to 1
    while(i < x.size && sets.nofSets > 1) {
      sets.union(x(i)._1, x(i)._3)
      i += 1
    }
    // Returning the last weight
    if (sets.nofSets == 1) Some(x(i - 1)._2)
    else None
  }
}


class UnionFind[E] {
  /* Insert your internal data structures here.
   * One possible choice is the following but
   * you are free to use your own choices as well:
   * protected val parent = new scala.collection.mutable.HashMap[E, E]()
   * protected val rank = new scala.collection.mutable.HashMap[E, Int]()
   * protected var _nofSets = 0
   */
  protected val parent = new scala.collection.mutable.HashMap[E, E]()
  protected val rank = new scala.collection.mutable.HashMap[E, Int]()
  protected var _nofSets = 0

  /**
   * Introduce a new element in this disjoint sets data structure and
   * put it into the set consisting only of the element itself.
   * Does nothing if the element is already in the disjoint sets data structure.
   * Returns true if the element was inserted (i.e., was not already in
   * the data structure), false otherwise.
   * A constant-time operation (actually only on average if hash map
   * searches and insertions are used in the code).
   */
  def makeSet(element: E): Boolean = {
    if (!rank.get(element).isDefined) {
      _nofSets += 1
      parent += (element -> element)
      rank += (element -> 0)
      true
    } else {
      false
    }
  }
  
  /**
   * Get the representative element of the given element
   * in the current disjoint sets data structure.
   * Two elements are in the same set if and only if their representatives
   * are the same.
   * The representatives may change during union operations and
   * thus it is *not* safe to use previously calculated representatives
   * after an union operation has been performed.
   * Throws an exception if the element has not been introduced earlier
   * with makeSet.
   * An O(log n) operation on average,
   * where n is the number of elements in all the sets.
   */
  def findSet(element: E): E = {
    var e = element
    while (parent(e) != e) {
      e = parent(e)
    }
    e
  }

  /**
   * Merge (i.e., make union of) the sets containing the elements
   * element1 and element2.
   * An O(log n) operation, where n is the number of elements in all the sets.
   */
  def union(element1: E, element2: E): Unit = {
    val r1 = findSet(element1)
    val r2 = findSet(element2)

    if (r1 != r2) {
      if (rank(r1) < rank(r2)) {
        parent(r1) = r2
      } else {
        parent(r2) = r1
        if (rank(r1) == rank(r2)) rank(r1) = rank(r1) + 1
      }
      _nofSets -= 1
    }
  }

  /** Get the number of elements in this disjoint sets data structure. */
  def nofElements: Int = {
    rank.size
  }

  /** Get the number of sets in this disjoint sets data structure. */
  def nofSets: Int = {
    _nofSets
  }
}
