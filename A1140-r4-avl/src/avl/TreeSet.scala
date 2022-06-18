/* Author: Tommi Junttila, Aalto University.
 * Only for the use on the Aalto course CS-A1140.
 * Redistribution not allowed.
 */

package avl

/**
 * An ordered set class, implemented with AVL trees.
 * Uses the TreeMap class internally to implement ordered sets.
 */
class TreeSet[Key](implicit ord: Ordering[Key]) {
  private val m = new TreeMap[Key, Object]()
  private val dummy = new Object()

  /**
   * Insert a key in the tree.
   * Do nothing if the key is already in the tree.
   * Return true if the key was inserted, false if it was already in the tree.
   * Should work in time O(h), where h is the height of the tree.
   * Remember to update the _nofNodes counter.
   */
  def insert(key: Key): Boolean = {
    m.insert(key, dummy) == None
  }

  /**
   * Check whether a key is in the tree.
   * Should work in time O(h), where h is the height of the tree.
   */
  def search(key: Key): Boolean = {
    m.get(key) != None
  }

 /*
   * Remove the key from the tree.
   * Do nothing if the key is not in the tree.
   * Return true if the key was in the tree, false otherwise.
   * Remember to update the _nofNodes counter.
   * Should work in time O(h), where h is the height of the tree.
   */
  def remove(key: Key): Boolean = {
    m.remove(key) != None
  }
}
