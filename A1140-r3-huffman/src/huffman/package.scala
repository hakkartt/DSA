/* Author: Tommi Junttila, Aalto University.
 * Only for the use on the Aalto course CS-A1140.
 * Redistribution not allowed.
 */

import scala.collection.mutable.ArrayBuffer

/*
 * NOTE: only modify the methods marked with ???.
 *  In other words, do NOT touch the other methods
 *   (the grading process will use unmodified versions of them).
 */

package object huffman {

  /**
   * Encode the sequence of bytes.
   * Returns a pair consisting of
   * - the encoding of a tree (needed for decoding), and
   * - the encoding of the sequence according to the tree.
   * The results are sequences of bits: for manipulation simplicity,
   * bit sequences are here represented with Vectors of Booleans while
   * a more compact way would be to represent them with Arrays of Bytes or Ints.
   */
  def encode(bytes: Vector[Byte]): (Vector[Boolean],Vector[Boolean]) = {
    // Build the Huffman tree for the input byte sequence
    val tree = buildTree(bytes)
    // Encode the input byte sequence
    val encodedBytes = {
      // Get the encoding table mapping bytes to bit sequences
      val table = getTable(tree)
      // Mutable buffer for building the result
      val result = new ArrayBuffer[Boolean]()
      // Encode each input sequence byte
      for(b <- bytes)
        result ++= table(b)
      // The result is converted to an immutable Vector
      result.toVector
    }
    // Encode the Huffman tree in a bit sequence
    val encodedTree = encodeTree(tree)
    // Return the encoded tree and byte sequence
    (encodedTree, encodedBytes)
  }

  /**
   * Reverse of encode.
   * Input consists of
   * - the encoding of a Huffman tree and
   * - the encoding of a byte sequence obtained with the tree.
   * Returns the byte sequence.
   */
  def decode(encodedTree: Vector[Boolean], encodedBytes: Vector[Boolean]): Vector[Byte] = {
    val tree = decodeTree(encodedTree)
    val result = new ArrayBuffer[Byte]()
    val iter = encodedBytes.iterator
    while(iter.hasNext) {
      // Start from the root node
      var node = tree
      var done = false
      while(!done) {
        node match {
          case Internal(left, right) => {
            // Read the next bit in the encoded byte sequence
            val bit = iter.next
            // And branch by its value: 0 to the left, 1 to the right
            if(bit) node = right
            else node = left
          }
          case Leaf(byte, _) => {
            // Reached a leaf, output the byte
            result += byte
            done = true
          }
        }
      }
    }
    result.toVector
  }

  import scala.collection.mutable.PriorityQueue
  import scala.collection.mutable.Buffer
  /**
   * Build a Huffman tree for the byte sequence.
   */
  def buildTree(bytes: Vector[Byte]): Node = {
    /* Hints:
     * First compute the frequencies (number of occurrences)
     *  of the bytes occurring in the sequence.
     * Then associate each byte with non-zero occurrence with
     *  a leaf node and put the nodes in a priority queue.
     * Finally, build the tree by taking the two nodes with smallest
     *  frequencies from the queue, make a new internal node
     *  having them as the children (make the node with smaller
     *  frequency to be the left child), and insert the new node in
     *  the queue. Repeat until the queue has only one node
     *  (the final tree) left.
     * You can also see the Wikipedia article
     *  https://en.wikipedia.org/wiki/Huffman_coding
     */
    
    val nodes = PriorityQueue[Node]()(Ordering.by(-_.freq))
    val cnt = new scala.collection.mutable.HashMap[Byte, Int]()
    // Map the number of occurences for each byte
    for(i <- 0 until bytes.size) {
      if (cnt.get(bytes(i)).isDefined) {
        cnt(bytes(i)) = cnt(bytes(i)) + 1
      }
      else {
        cnt += (bytes(i) -> 1)
      }
    }
    // Creating nodes and adding them to queue
    cnt.foreach(p => nodes.enqueue(new Leaf( p._1, p._2 )))
    while(nodes.size > 1) {
      val n1 = nodes.dequeue
      val n2 = nodes.dequeue
      nodes.enqueue(new Internal(n1, n2))
    }
    nodes.head
  }

  /**
   * Get the encoding table from a Huffman tree,
   *  associating each byte occurring in the tree to its bit encoding.
   * The encoding value of a byte in a leaf node is formed
   *  by the path from the root of the tree to the leaf node:
   *  - going to the left child adds a 0 (i.e., false),
   *  - going to the right adds a 1 (i.e., true).
   * See the unit tests for examples and you can also
   *  consult the Wikipedia article
   *  https://en.wikipedia.org/wiki/Huffman_coding
   */
  def getTable(tree: Node): Map[Byte, Vector[Boolean]] = {
    val hashmap = scala.collection.mutable.HashMap[Byte, Vector[Boolean]]()
    // Use recursion
    def func_(node: Node, vector: Vector[Boolean]): Unit = {
      node match {
        case Leaf(sym, freq) => {
          hashmap += (sym -> vector)
        }
        case Internal(left, right) => {
          func_(left, vector ++ Vector(false))
          func_(right, vector ++ Vector(true))
        }
      }
    }
    func_(tree, Vector[Boolean]())
    hashmap.toMap
  }

  /**
   * Encode a Huffman tree in a sequence of bits:
   * - The encoding of an internal node consists of a 0 (i.e., false)
   *   followed by the encoding of the left subtree and
   *    the encoding of the right subtree
   * - The encoding of a leaf node consists of a 1 (i.e., true)
   *   followed by the 8 bits encoding the byte of the leaf
   *   node in the most-significant-bit-first order.
   *   Note: the frequency is not included in the encoding,
   *   it is not needed when decoding a byte sequence based on the tree.
   */
  def encodeTree(tree: Node): Vector[Boolean] = {
    
    val res = Buffer[Boolean]()

    def bitsToBool(x: Leaf): Unit = {
      var i = 7
      while (i > -1) {
        res += ((x.symbol >> i) & 0x01) != 0
        i -= 1
      }
    }

    if (tree.isLeaf) {
      res += true
      bitsToBool(tree.asInstanceOf[Leaf])
    } 
    else {
      res += false
      res ++= encodeTree(tree.asInstanceOf[Internal].left)
      res ++= encodeTree(tree.asInstanceOf[Internal].right)
    }
    res.toVector
    
  }

  /**
   * The reverse of encodeTree.
   * The freq-values of the leaf nodes shall be 0 in the decoded tree.
   */
  def decodeTree(bits: Vector[Boolean]): Node = {
    
    def bitsToByte(x: Vector[Boolean]): Byte = {
      Integer.parseInt(x.map { case false => '0' ; case true => '1' }.mkString, 2).toByte
    }

    def decode_(vect: Vector[Boolean]): (Node, Vector[Boolean]) = {
      
      if (vect.head) {
        val spl = vect.tail.splitAt(8)
        (new Leaf(bitsToByte(spl._1), 0), spl._2)
      } 
      else {
        val l = decode_(vect.tail)
        val r = decode_(l._2)
        (new Internal(l._1, r._1), r._2)
      }
    }
    
    decode_(bits)._1
    
  }
}
