/* Author: Tommi Junttila, Aalto University.
 * Only for the use on the Aalto course CS-A1140.
 * Redistribution not allowed.
 */

package escape

import scala.swing._
import java.awt.Color

class GUI(val world: World, path: Seq[(Int,Int)]) extends SimpleSwingApplication {
  val colorNormal = Map(Start -> new Color(50, 50, 255),
                        Safe -> new Color(150, 150, 255),
                        Corridor -> new Color(200, 200, 200),
                        Swamp -> new Color(50, 200, 50),
                        Door -> new Color(200, 50, 200),
                        Wall -> new Color(0,0,0))
  val preferredBlockSize = 30
  val rows = world.nofRows
  val columns = world.nofColumns
  var quitAnimate = false
  private val filled = Array.fill(rows, columns)(false)
  val mazePanel = new Panel {
    background = Color.black
    preferredSize = new Dimension(columns * preferredBlockSize,
                                  rows * preferredBlockSize)

    override protected def paintComponent(g: Graphics2D) {
      super.paintComponent(g)
      val width = this.size.width
      val height = this.size.height
      val cellWidth = width.toDouble / columns
      val cellHeight = height.toDouble / rows

      def xOutLeft(c: Int): Int = ((c * width.toDouble) / columns).toInt
      def xOutRight(c: Int): Int = xOutLeft(c+1)
      val xDelta = 0.15 * width.toDouble / columns
      def xInLeft(c: Int): Int = ((c * width.toDouble) / columns + xDelta).toInt
      def xInRight(c: Int): Int = (((c+1) * width.toDouble) / columns - xDelta).toInt
      val yDelta = 0.15 * height.toDouble / rows
      def yOutTop(r: Int): Int = ((r * height.toDouble) / rows).toInt
      def yOutBot(r: Int): Int = yOutTop(r+1)
      def yInTop(r: Int): Int = ((r * height.toDouble) / rows + yDelta).toInt
      def yInBot(r: Int): Int = (((r+1) * height.toDouble) / rows - yDelta).toInt
      g.setStroke(new java.awt.BasicStroke(0))
      for (r <- 0 until rows) {
        for (c <- 0 until columns) {
          val block = world(r, c)
          if (filled(r)(c)) {
            g.setColor(Color.white)
            g.fillRect(xOutLeft(c), yOutTop(r),
                       xOutRight(c) - xOutLeft(c), yOutBot(r) - yOutTop(r))
            g.setColor(colorNormal(block))
            g.fillRect(xInLeft(c), yInTop(r),
                       xInRight(c) - xInLeft(c), yInBot(r) - yInTop(r))
          } else {
            g.setColor(colorNormal(block))
            g.fillRect(xOutLeft(c), yOutTop(r),
                       xOutRight(c) - xOutLeft(c), yOutBot(r) - yOutTop(r))
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
  def animate(delayInMS: Int = 100) = {
    var delay = delayInMS * 0
    for((r, c) <- path if(!quitAnimate)) {
      Thread.sleep(delay)
      val block = world(r, c)
      delay = delayInMS * block.duration
      filled(r)(c) = true
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
