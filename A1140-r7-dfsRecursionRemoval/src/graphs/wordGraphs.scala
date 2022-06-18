package graphs

object wordGraphs {
  val words3common = List("the", "and", "for", "are", "but", "not", "you", "all", "any", "can", "had", "her", "was", "one", "our", "out", "day", "get", "has", "him", "his", "how", "man", "new", "now", "old", "see", "two", "way", "who", "boy", "did", "its", "let", "put", "say", "she", "too", "use")
  val words3common2 = List("the", "and", "for", "are", "bug", "not", "you", "all", "any", "can", "had", "her", "was", "one", "our", "out", "shy", "get", "has", "him", "his", "how", "man", "new", "now", "old", "see", "two", "way", "who", "boy", "did", "its", "let", "put", "say", "she", "too", "use")

  /**
   * Returns true iff the strings are of the same length and
   * differ in exactly one position (in case insensitive mode).
   */
  private def areNeighbours(w1: String, w2: String): Boolean = {
    if (w1.length != w2.length) false
    else {
      (w1 zip w2).count({ case (c1, c2) => (c1.toLower != c2.toLower) }) == 1
    }
  }

  /**
   * Given a sequence of strings of the same length,
   * builds an undirected graph such that
   * - the vertices are the strings in the sequence, and
   * - there is an edge between two vertices (strings) if and only if
   *   the strings differ only in one location.
   */
  def getGraph(words: Seq[String]): graphs.Graph[String] = {
    require(words.size > 0, "The word list must not be empty")
    require(words.forall(w => w.length == words.head.length), "The words should be of the same length")
    val g = new graphs.Graph[String]()
    words.foreach(w => g.addVertex(w))
    var l1 = words
    while (l1.nonEmpty) {
      val w1 = l1.head
      for (w2 <- l1.tail) {
        if (areNeighbours(w1, w2))
          g.addEdge(w1, w2)
      }
      l1 = l1.tail
    }
    g
  }
}
