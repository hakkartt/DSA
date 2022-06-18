package bst

/*
 * A mutable data structure for nodes in mutable tree maps.
 * In reality, this class would be protected or private but
 * we keep it public for the sake of the automatic grading process.
 */
sealed class Node[Key, Value](k: Key, v: Value) {
  private var _key: Key = k
  /** Get the key stored in the node */
  def key = _key
  /** Set the key stored in the node.
   * Allows us to write "n.key = k" for a node n and key k. */
  def key_=(newKey: Key): Unit = _key = newKey

  private var _value: Value = v
  /** Get the value stored in the node */
  def value = _value
  /** Set the value stored in the node.
   * Allows us to write "n.value = v" for a node n and value v. */
  def value_=(newValue: Value): Unit = _value = newValue

  private var _left: Node[Key,Value] = null
  /** Does the node have a left child? */
  def hasLeft = _left != null
  /** Getter for the left child link. */
  def left = _left
  /**
   * Setter for the left child link.
   * Also updates the parent link of the newLeft node (if non-null).
   */
  def left_=(newLeft: Node[Key,Value]): Unit = {
    if(hasLeft) {
      assert(left._parent == this);
      left._parent = null
    }
    _left = newLeft
    if(hasLeft)
      left._parent = this
  }

  private var _right: Node[Key,Value] = null
  /** Does the node have a right child? */
  def hasRight = _right != null
  /** Getter for the right child link. */
  def right = _right
  /**
   * Setter for the right child link.
   * Also updates the parent link of the newLeft node (if non-null).
   */
  def right_=(newRight: Node[Key, Value]): Unit = {
    if(hasRight) {
      assert(right._parent == this);
      right._parent = null
    }
    _right = newRight
    if(hasRight)
      right._parent = this
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
}
