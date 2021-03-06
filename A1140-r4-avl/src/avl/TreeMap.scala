/* Author: Tommi Junttila, Aalto University.
 * Only for the use on the Aalto course CS-A1140.
 * Redistribution not allowed.
 */

package avl

/**
 * An ordered map class, implemented with AVL trees.
 */
class TreeMap[Key, Value](implicit ord: Ordering[Key]) {
  /*
   * The root of the tree.
   * If the tree is empty, then this should be null.
   * In reality, this would be a private member but we keep it public
   * for the sake of the testing and grading processes.
   */
  var root: Node[Key, Value] = null

  /*
   * In order to have constant time size queries,
   * we keep count of how many keys the tree currently has.
   * Remember to update this when you insert/remove keys.
   */
  private var _nofKeys = 0
  /** The size of the tree */
  def size: Int = _nofKeys
  /** Is the tree empty? */
  def isEmpty = (_nofKeys == 0)
  
  // setting node x as parent node for node y
  private def pc(x: Node[Key, Value], y:Node[Key, Value]) = {
    var parent = x
    var child = y
    if (ord.compare(child.key, parent.key) < 0) {
      parent.left_=(child)
    } else {
      parent.right_=(child)
    }
  }
  
  // implement the helper methods recommended in the exercise description
  private def leftrotate(a: Node[Key, Value]) = {
    val b = a.right
    if (a.hasParent) {
      val parent = a.parent
      pc(parent, b)
    } 
    else root = b
    a.right_=(b.left)
    b.left_=(a)
    a.updateHeight()
    b.updateHeight()
    b
  }
  
  private def rightrotate(a: Node[Key, Value]) = {
    val b = a.left
    if (a.hasParent) {
      val par= a.parent
      pc(par, b)
    } 
    else root = b
    a.left_=(b.right)
    b.right_=(a)
    a.updateHeight()
    b.updateHeight()
    b
  }
  
  private def bottomup(a: Node[Key, Value]): Unit = {
    var b = a
    while (true) {
      if (b.balance < -1) {
        if (b.left.balance <= 0) {
          this.rightrotate(b)
        } 
        else {
          this.leftrotate(b.left)
          this.rightrotate(b)
        }
      } 
      else if (b.balance > 1) {
        if (b.right.balance >= 0) {
          this.leftrotate(b)
        } 
      else {
          this.rightrotate(b.right)
          this.leftrotate(b)
        }
      }
      if (!b.hasParent) return
      b.updateHeight()
      b = b.parent
      b.updateHeight()
    }
  }
  

  /**
   * Insert the (key,value) mapping in the tree.
   * If the key already has a value, replace it with the new one.
   * Returns the old value of the key or None if it did not have one.
   * Should work in time O(h), where h is the height of the tree.
   * Remember to update the _nofKeys counter.
   */
  def insert(key: Key, value: Value): Option[Value] = {
    if (_nofKeys == 0) {
      root = new Node(key,value)
      root.updateHeight()
      _nofKeys += 1
      return None
    }
    var p = root
    var x = root
    var w = false
    while (x != null) {
      p = x
      if (ord.compare(key, x.key) > 0) {
        x = x.right
        w = false
      }
      else if (ord.compare(key, x.key) < 0) {
        x = x.left
        w = true
      }
      else {
        val last = x.value
        x.value_=(value)
        return Some(last)
      }
    }
    w match {
      case false => p.right_=(new Node(key, value))
      case true => p.left_=(new Node(key, value)) 
    }
    this.bottomup(p)
    _nofKeys += 1
    None
  }


  /**
   * Get the value of the key, or None if the key does not have a value.
   * Should work in time O(h), where h is the height of the tree.
   */
  def get(key: Key): Option[Value] = {
    ???
  }

  /*
   * Return the smallest key in the treemap, or None if the tree is empty.
   * Should work in time O(h), where h is the height of the tree.
   */
  def min: Option[Key] = {
    ???
  }

  /*
   * Return the smallest key in the treemap that is equal to or greater than
   * the argument key (or None if the tree is empty or all its keys are
   * less than the argument key).
   * Should work in time O(h), where h is the height of the tree.
   */
  def ceiling(key: Key): Option[Key] = {
    ???
  }



  /*
   * An internal helper function.
   * Substitutes the node n1 with the node n2.
   * The node n1 must not be null and it (with all its descendants)
   * will be effectively deleted from the tree.
   * The node n2 can be null.
   */
  private def substWith(n1: Node[Key,Value], n2: Node[Key,Value]): Unit = {
    require(n1 != null)
    // Remove the connection from the previous parent of n2
    if(n2 != null && n2.hasParent) {
      if(n2.parent.left == n2) n2.parent.left = null
      else {assert(n2.parent.right == n2); n2.parent.right = null }
    }
    // Make n2 to substitute n1
    if(n1 == root) {root = n2 }
    else {
      if(n1.parent.left == n1) n1.parent.left = n2
      else {assert(n1.parent.right == n1); n1.parent.right = n2 }
    }
  }

  /*
   * Remove the key from the treemap.
   * Do nothing if the key is not in the treemap.
   * Return the old value of the key or None if the key was not in the treemap.
   * Should work in time O(h), where h is the height of the tree.
   * Remember to update the _nofKeys counter.
   */
  def remove(key: Key): Option[Value] = {
    ???
  }


  /**
   * Check whether the BST property hods in the tree, i.e., for each node
   * the descendants in the left sub-tree should be less than the key, and
   * the descendants in the right sub-tree should be greater than the key.
   * Linear in the size of the tree, only for debuggin and validation purposes.
   */
  def isValidBST: Boolean = {
    def inner(n: Node[Key,Value], lower: Option[Key], upper: Option[Key]): Boolean = {
      if(n == null) true
      else if(lower.nonEmpty && ord.compare(n.key, lower.get) <= 0) false
      else if(upper.nonEmpty && ord.compare(n.key, upper.get) >= 0) false
      else inner(n.left, lower, Some(n.key)) && inner(n.right, Some(n.key), upper)
    }
    inner(root, None, None)
  }

  /**
   * Does the tree have the AVL property, i.e., is it properly balanced?
   * Slow, for validation and debugging purposes only
   */
  def hasAVLProperty: Boolean = hasAVLProperty(root)

  /**
   * Does the sub-tree rooted at the node have the AVL property, i.e.,
   * is it properly balanced?
   * Slow, for validation and debugging purposes only
   */
  def hasAVLProperty(node: Node[Key,Value]): Boolean = {
    def inner(n: Node[Key,Value]): (Boolean,Int) = {
      if(n == null) (true, -1)
      else {
        val (leftOk, leftHeight) = inner(n.left)
        if(!leftOk) return (false, 0)
        val (rightOk, rightHeight) = inner(n.right)
        if(!rightOk) return (false, 0)
        val balance = rightHeight - leftHeight
        if(!(-1 <= balance && balance <= 1)) return (false, 0)
        (true, 1 + (leftHeight max rightHeight))
      }
    }
    inner(node)._1
  }


  /**
   * Print the tree in a nicely formatted multi-line string.
   */
  def prettyString: String = prettyString(root)

  def prettyString(subTreeRoot: Node[Key,Value]): String = {
    val s = new scala.collection.mutable.StringBuilder()
    def sep = " "
    def inner(node: Node[Key,Value], indent: String): Unit = {
      s ++= indent
      if(node == null) {
        s ++= "null\n"
      } else {
        s ++= s"key=${node.key} -> value=${node.value}\n"
        inner(node.left, indent+sep)
        inner(node.right, indent+sep)
      }
    }
    inner(subTreeRoot, "")
    s.toString
  }
}
