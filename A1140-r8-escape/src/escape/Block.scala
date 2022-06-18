/* Author: Tommi Junttila, Aalto University.
 * Only for the use on the Aalto course CS-A1140.
 * Redistribution not allowed.
 */

package escape

/* Base class for block types */
sealed class Block(val duration: Int, val charRepr: Char) {
  require(duration >= 0)
}

/* The concrete block types as objects */
object Start extends Block(0, '*') {}
object Safe extends Block(0, '@') {}
object Corridor extends Block(1, '.') {}
object Door extends Block(3, 'D') {}
object Swamp extends Block(8, 'S') {}
object Wall extends Block(20, 'W') {}

/**
 * The companion object helping to map blocks to their character representation
 * and vice versa.
 */
object Block {
  private val charMap = scala.collection.mutable.Map[Char, Block]()
  private def register(block: Block): Unit = {
    require(block != null)
    require(!charMap.contains(block.charRepr))
    charMap(block.charRepr) = block
  }
  /* Register all the concrete block types */
  register(Start)
  register(Safe)
  register(Corridor)
  register(Door)
  register(Swamp)
  register(Wall)
  /** Get the concrete block type represented by the character 'char' */
  def charToBlock(char: Char): Block = charMap(char)
}
