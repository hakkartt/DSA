package avl

/*
 * A mutable data structure for nodes in mutable AVL tree maps.
 * In reality, this class would be protected or private but
 * we keep it public for the sake of the automatic grading process.
 */
sealed class Node[Key, Value](k: Key, v: Value) {
  private var _key: Key = k
  def key = _key
  def key_=(newKey: Key): Unit = _key = newKey

  private var _value: Value = v
  def value = _value
  def value_=(newValue: Value): Unit = _value = newValue

  private var _left: Node[Key,Value] = null
  /** Does the node have a left child? */
  def hasLeft = _left != null
  /** Getter for the left child link. */
  def left = _left
  /**
   * Setter for the left child link.
   * Also updates the parent link of the newLeft node (if non-null).
   * Note that the child link of the old parent of the old left node is NOT
   * updated and should thus be taken care elsewhere.
   */
  def left_=(newLeft: Node[Key,Value]): Unit = {
    if(hasLeft && left._parent == this) left._parent = null
    _left = newLeft
    if(hasLeft) left._parent = this
  }

  private var _right: Node[Key,Value] = null
  /** Does the node have a right child? */
  def hasRight = _right != null
  /** Getter for the right child link. */
  def right = _right
  /**
   * Setter for the right child link.
   * Also updates the parent link of the newRight node (if non-null).
   * Note that the child link of the old parent of the old right node is NOT
   * updated and should thus be taken care elsewhere.
   */
  def right_=(newRight: Node[Key, Value]): Unit = {
    if(hasRight && right._parent == this) right._parent = null
    _right = newRight
    if(hasRight) right._parent = this
  }

  private var _parent: Node[Key,Value] = null
  /** Does the node have a parent?
   * The root and nodes that do not belong to any tree (just created or deleted)
   * do not have a parent, the non-root ones in a tree do.
   */
  def hasParent = _parent != null
  /** Getter for the parent link. */
  def parent = _parent
  // No setter for parent, parent links should only be modified
  // by modifying the child links

  private var _height: Int = 0
  /** Get the height of the node */
  def height = _height
  /** Set the height of the node manually */
  def height_=(newHeight: Int): Unit = {
    require(newHeight >= 0)
    _height = newHeight
  }
  /* Update the height of this by using the heights of the children.
   * Assumes that the heights of the children are correct.
   * Leaves are at height 0.
   * A constant-time operation. */
  def updateHeight(): Unit = {
    var heightLeft = if(hasLeft) left.height else -1
    var heightRight = if(hasRight) right.height else -1
    height = 1 + (heightLeft max heightRight)
  }
  /*
   * Get the "AVL balance" of a node, i.e., the difference between
   * the heights of the left and right subtrees.
   * A positive value i means that the right subtree is i levels higher
   * than the left one; negative values mean that the left subtree is higher.
   * Assumes that the height fields of the children are correct.
   * A constant-time operation.
   */
  def balance: Int = {
    val heightLeft = if(hasLeft) left.height else -1
    val heightRight = if(hasRight) right.height else -1
    heightRight - heightLeft
  }
}
