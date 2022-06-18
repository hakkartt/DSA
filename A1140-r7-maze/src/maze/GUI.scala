/* Author: Tommi Junttila, Aalto University.
 * Only for the use on the Aalto course CS-A1140.
 * Redistribution not allowed.
 */

package maze

import scala.swing._
import java.awt.Color

class GUI(val originalMaze: Maze) extends SimpleSwingApplication {
  val preferredWallThickness = 6
  val preferredCellSize = 20
  def rows = originalMaze.rows
  def columns = originalMaze.columns
  var quitAnimate = false
  private val maze = new Maze(rows, columns)
  private val filled = Array.fill(rows, columns)(false)
  val mazePanel = new Panel {
    background = Color.black
    preferredSize = new Dimension(maze.columns * (preferredWallThickness + preferredCellSize) + preferredWallThickness,
      maze.rows * (preferredWallThickness + preferredCellSize) + preferredWallThickness)

    override protected def paintComponent(g: Graphics2D) {
      super.paintComponent(g)
      val width = this.size.width
      val height = this.size.height
      val vWallThickness = width.toDouble / (maze.rows + maze.rows * preferredCellSize.toDouble / preferredWallThickness + 1)
      val hWallThickness = height.toDouble / (maze.rows + maze.rows * preferredCellSize.toDouble / preferredWallThickness + 1)
      val wallThickness = (hWallThickness min vWallThickness) max 2.0
      val vCellSize = (height.toDouble - (maze.rows + 1) * wallThickness) / maze.rows
      val hCellSize = (width.toDouble - (maze.columns + 1) * wallThickness) / maze.columns

      def xInLeft(c: Int): Int = {
        (c * (wallThickness + hCellSize) + wallThickness).toInt
      }
      def xInRight(c: Int): Int = {
        (c * (wallThickness + hCellSize) + wallThickness + hCellSize).toInt
      }
      def xOutLeft(c: Int): Int = if (c == 0) 0 else xInRight(c - 1) + 1
      def xOutRight(c: Int): Int = if (c == columns - 1) width else xInLeft(c + 1) - 1
      def yInBot(r: Int): Int = {
        height - 1 - (r * (wallThickness + vCellSize) + wallThickness).toInt
      }
      def yInTop(r: Int): Int = {
        height - 1 - (r * (wallThickness + vCellSize) + wallThickness + vCellSize).toInt
      }
      def yOutBot(r: Int): Int = if (r == 0) height else yInTop(r - 1) - 1
      def yOutTop(r: Int): Int = if (r == rows - 1) 0 else yInBot(r + 1) + 1
      g.setStroke(new java.awt.BasicStroke(0))
      for (r <- 0 until rows) {
        for (c <- 0 until columns) {
          if (filled(r)(c)) g.setColor(Color.white)
          else g.setColor(new Color(200, 200, 255))
          g.fillRect(xInLeft(c), yInTop(r), xInRight(c) - xInLeft(c) + 1, yInBot(r) - yInTop(r) + 1)
        }
      }
      g.setColor(Color.black)
      g.setStroke(new java.awt.BasicStroke(wallThickness.toInt))
      for (r <- 0 until maze.rows) {
        for (c <- 0 to maze.columns) {
          if (maze.hasVerticalWall(r, c)) {
            g.setColor(Color.black)
            g.fillRect(xOutLeft(c), yOutTop(r), xInLeft(c) - xOutLeft(c), yOutBot(r) - yOutTop(r))
          } else {
            g.setColor(Color.white)
            g.fillRect(xOutLeft(c), yInTop(r), xInLeft(c) - xOutLeft(c), yInBot(r) - yInTop(r) + 1)
          }
        }
      }
      for (c <- 0 until maze.columns) {
        for (r <- 0 to maze.rows) {
          if (maze.hasHorizontalWall(r, c)) {
            g.setColor(Color.black)
            g.fillRect(xOutLeft(c), yInBot(r) + 1, xOutRight(c) - xOutLeft(c) + 1, yOutBot(r) - yInBot(r) + 1)
          } else {
            g.setColor(Color.white)
            g.fillRect(xInLeft(c), yInBot(r) + 1, xInRight(c) - xInLeft(c) + 1, yOutBot(r) - yInBot(r) + 1)
          }
        }
      }
    }
  }
  def top = new MainFrame {
    contents = mazePanel
    def closer() {
      quitAnimate = true
    }
  }
  def animate(delayInMS: Int = 10) = {
    import Maze.Direction._
    for ((r, c, d) <- originalMaze.getBreaks if(!quitAnimate)) {
      Thread.sleep(delayInMS)
      maze.breakWall(r, c, d)
      filled(r)(c) = true
      d match {
        case Up => filled(r + 1)(c) = true
        case Down => filled(r - 1)(c) = true
        case Right => filled(r)(c + 1) = true
        case Left => filled(r)(c - 1) = true
      }
      mazePanel.repaint()
    }
  }
  def go() {
    quitAnimate = false
    if(top.size == new Dimension(0, 0)) top.pack()
    top.centerOnScreen()
    top.visible = true
    animate()
  }
}
