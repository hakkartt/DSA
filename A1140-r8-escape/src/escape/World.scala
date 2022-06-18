/* Author: Tommi Junttila, Aalto University.
 * Only for the use on the Aalto course CS-A1140.
 * Redistribution not allowed.
 */

package escape

/**
 * The class for world grids.
 * Do not modify (an unmodifed version is used in the grading process).
 */
class World(rows: Seq[Seq[Block]]) {
  /**
   * The number of rows in the world grid.
   */
  val nofRows = rows.length

  /**
   * The number of columns in the world grid.
   */
  val nofColumns = rows.head.length

  // Input validation checks
  require(nofRows > 3)
  require(nofColumns > 3)
  for(row <- rows)
    require(row.length == nofColumns, "All the rows must be of the same length")

  // Initialize internal data structures
  protected val m = Array.fill[Block](nofRows, nofColumns)(null)
  protected var _startPosition: (Int, Int) = null
  for((row, r) <- rows.zipWithIndex; (block, c) <- row.zipWithIndex) {
    m(r)(c) = block
    if(block == Start) {
      require(_startPosition == null, "The world has more than one start block")
      _startPosition = (r, c)
    }
  }
  require(_startPosition != null, "The world has no start block")

  /**
   * The coordinate (row, column) of the start block.
   */
  val startPosition: (Int, Int) = _startPosition

  /**
   * Is the position (row, column) inside the world?
   * Mainly for requirements and testing purposes.
   */
  def isValidCoord(row: Int, column: Int) : Boolean = {
    0 <= row && row < nofRows &&
    0 <= column && column < nofColumns
  }

  /**
   * Is the position 'coord' inside the world?
   * Mainly for requirements and testing purposes.
   */
  def isValidCoord(coord: (Int, Int)): Boolean = isValidCoord(coord._1, coord._2)

  /**
   * Get the set of legal neighbour position of the position (row, column):
   * these are the positions to which one can move in one step
   */
  def neighbours(row: Int, column: Int): Set[(Int,Int)] = {
    require(isValidCoord(row, column))
    val result = scala.collection.mutable.ArrayBuffer[(Int, Int)]()
    if(row > 0) result += ((row-1, column))
    if(row < nofRows-1) result += ((row+1, column))
    if(column > 0) result += ((row, column-1))
    if(column < nofColumns-1) result += ((row, column+1))
    result.toSet
  }

  /** Get the set of legal neighbour poisition of the position 'coord':
   * these are the positions to which one can move in one step */ 
  def neighbours(coord: (Int, Int)): Set[(Int,Int)] = neighbours(coord._1, coord._2)


  /** world(r, c) gives the block at position (r,c) */
  def apply(row: Int, column: Int): Block = {
    require(isValidCoord(row, column))
    m(row)(column)
  }

  /** world(coord) gives the block at position coord */
  def apply(coord: (Int, Int)): Block = apply(coord._1, coord._2)


  /** Checks if path is a valid escape path:
   * - each coordinate is within the world
   * - the next coordinate is obtained from the previous by moving one step
   *   up, down, left or right
   * - start is in the start position
   * - end is in a Safe position.
   * If it is, returns a triple (true, duration, ""),
   *   where duration is the cumulative duration (~weight) of the path.
   * If it is not, returns a triple (false, 0, msg), where msg is
   *   where msg is a string explaining a reason for the invalidity.
   */
  def isValidEscapePath(path: Seq[(Int, Int)]): (Boolean, Int, String) = {
    val p = path.toIndexedSeq
    if(p.length < 2)
      return (false, 0, "The path should have at least two positions")
    if(apply(p.head) != Start)
      return(false, 0, "The path should start with a start block")
    if(apply(p.last) != Safe)
      return(false, 0, "The path should end in a safe block")
    var duration = 0
    for(i <- 0 until p.length) {
      val coord = p(i)
      if(!isValidCoord(coord))
        return (false, 0, s"The path has an invalid coordinate $coord")
      val block = apply(coord)
      duration += block.duration
    }
    for(i <- 1 until p.length) {
      val prev = p(i-1)
      val current = p(i)
      if(!neighbours(prev).contains(current))
        return (false, 0, s"The coordinate $current is not a valid neighbour of the coordinate $prev")        
    }
    return (true, duration, "")
  }

  /** Output a string representation of the world */
  override def toString: String = {
    m.map(row => row.map(b => b.charRepr).mkString("")).mkString("\n")
  }
}

object World {
  /** Read a string representation of the world */
  def apply(stringRepr: String): World = {
    val rows = stringRepr.split("\n").map(_.trim).filter(_ != "")
    val w = rows.map(row => row.map(c => Block.charToBlock(c)))
    new World(w)
  }
}
