/* Author: Tommi Junttila, Aalto University.
 * Only for the use on the Aalto course CS-A1140.
 * Redistribution not allowed.
 */

package maze

/**
 * A mutable class for rows * columns mazes.
 * When created, the maze will have all the walls.
 * The walls can be broken with the breakWall method. Once broken, they cannot be rebuild.
 * The wall breaks are recorded in a maze object and
 * the maze creation process can then be replayed with the GUI class (see testandVisualize.scala).
 */
sealed class Maze(val rows: Int, val columns: Int) {
  require(rows >= 1)
  require(columns >= 1)

  import Maze._
  import Maze.Direction._

  /* To play the animation, we record the construction steps here */
  private val breaks = scala.collection.mutable.ArrayBuffer[(Int, Int, Direction)]()
  def getBreaks = breaks.toList

  private val horizontalWalls = Array.fill[Boolean](rows + 1, columns)(true)
  private val verticalWalls = Array.fill[Boolean](rows, columns + 1)(true)
  /* Entry and exit to the maze */
  horizontalWalls(0)(0) = false
  horizontalWalls(rows)(columns - 1) = false

  /**
   * Check whether the vertical wall at the "Left" direction of the cell at (row,column) is standing.
   * The unbreakable leftmost walls are at column==0 and the rightmost at column==columns
   */
  def hasVerticalWall(row: Int, column: Int): Boolean = {
    require(0 <= row && row < rows)
    require(0 <= column && column <= columns)
    verticalWalls(row)(column)
  }

  /**
   * Check whether the horizontal wall at the "Down" direction of the cell at (row,column) is standing.
   * The unbreakable bottom walls are at row==0 and the topmost at row==rows
   */
  def hasHorizontalWall(row: Int, column: Int): Boolean = {
    require(0 <= row && row <= rows)
    require(0 <= column && column < columns)
    horizontalWalls(row)(column)
  }

  /**
   * Check whether the wall at the direction "dir" of the cell at (row,column) is standing.
   */
  def hasWall(row: Int, column: Int, dir: Direction): Boolean = {
    require(0 <= row && row < rows)
    require(0 <= column && column < columns)
    dir match {
      case Up => horizontalWalls(row + 1)(column)
      case Down => horizontalWalls(row)(column)
      case Left => verticalWalls(row)(column)
      case Right => verticalWalls(row)(column + 1)
    }
  }

  /** Break the wall in the direction "dir" from the cell at position (row, column). */
  def breakWall(row: Int, column: Int, dir: Direction) = {
    require(0 <= row && row < rows)
    require(0 <= column && column < columns)

    dir match {
      case Up => {
        require(!(row == rows - 1), "Can't break top walls")
        require(horizontalWalls(row + 1)(column), "Can't break walls that are already broken")
        horizontalWalls(row + 1)(column) = false
      }
      case Down => {
        require(!(row == 0), "Can't break bottom walls")
        require(horizontalWalls(row)(column), "Can't break walls that are already broken")
        horizontalWalls(row)(column) = false
      }
      case Left => {
        require(!(column == 0), "Can't break leftmost walls")
        require(verticalWalls(row)(column), "Can't break walls that are already broken")
        verticalWalls(row)(column) = false
      }
      case Right => {
        require(!(column == columns), "Can't break rightmost walls")
        require(verticalWalls(row)(column + 1), "Can't break walls that are already broken")
        verticalWalls(row)(column + 1) = false
      }
    }

    /* Record the wall breaks for replaying */
    breaks.append((row, column, dir))
  }

  /* Textual representation of the maze */
  override def toString: String = {
    val s = new StringBuilder()
    s ++= horizontalWalls(rows).map(b => if (b) "-" else " ").mkString("+", "+", "+") + "\n"
    for (row <- rows - 1 to 0 by -1) {
      s ++= verticalWalls(row).map(b => if (b) "|" else " ").mkString(" ") + "\n"
      s ++= horizontalWalls(row).map(b => if (b) "-" else " ").mkString("*", "+", "*") + "\n"
    }
    s.toString
  }

  /**
   * Check that one can get to all cells from cell at (0,0).
   */
  def checkReachable: Boolean = {
    val seen = Array.fill(rows, columns)(false)
    val queue = scala.collection.mutable.Queue[(Int, Int)]()
    queue.enqueue((0, 0))
    seen(0)(0) = true
    while (queue.nonEmpty) {
      val (r, c) = queue.dequeue
      assert(seen(r)(c))
      if (r > 0 && !seen(r - 1)(c) && !hasWall(r,c,Down)) { queue.enqueue((r - 1, c)); seen(r - 1)(c) = true }
      if (r < rows - 1 && !seen(r + 1)(c) && !hasWall(r, c, Up)) { queue.enqueue((r + 1, c)); seen(r + 1)(c) = true }
      if (c > 0 && !seen(r)(c - 1) && !hasWall(r, c, Left)) { queue.enqueue((r, c - 1)); seen(r)(c - 1) = true }
      if (c < columns - 1 && !seen(r)(c + 1) && !hasWall(r, c, Right)) { queue.enqueue((r, c + 1)); seen(r)(c + 1) = true }
    }
    seen.forall(rowArr => rowArr.forall(_ == true))
  }
}

object Maze {
  /** Enumeration "type" for directions */
  object Direction extends Enumeration {
    type Direction = Value
    val Up, Down, Left, Right = Value
  }
}

