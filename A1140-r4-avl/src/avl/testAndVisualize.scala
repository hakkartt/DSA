package avl

/**
 * A small helper for visualizing binary trees.
 * Inserting requires that both TreeMap.get and 
 * TreeMap.insert are implemented. Removing requires
 * only the proper method, however.
 */
object testAndVisualize {
  def main(args: Array[String]) {
    val gui = new GUI
    gui.visible = true
  }
}