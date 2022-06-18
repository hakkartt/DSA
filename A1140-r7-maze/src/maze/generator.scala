/* Author: Tommi Junttila, Aalto University.
 * Only for the use on the Aalto course CS-A1140.
 * Redistribution not allowed.
 */

package maze

object generator {
  import Maze.Direction._

  /**
   * Generate a (pseudo-random) maze of given dimensions.
   * The maze must not have any cycles and all the cells must be reachable
   * from the entry cell at (0,0).
   */
  def generate(rows: Int, columns: Int, seed: Int = System.nanoTime.toInt): Maze = {
    val rand = new scala.util.Random(seed)
    val maze = new Maze(rows, columns)
    
    val vert = Array.fill[(Boolean, (Int, Int))](rows+1, columns+1)((false, (-1, -1)))
    var curr = (0,0)   
    vert(0)(0) = (true, (-1,-1))
    
    def next(from: (Int, Int)): Option[((Int, Int), Direction)] = {
      // Possible directions
      val y = scala.collection.mutable.Buffer(Up, Down, Left, Right)
      
      if (from == (-1,-1)) {
        return None
      } 
      
      if (from._1 == rows-1) y -= Up
      if (from._2 == columns-1) y -= Right
      if (from._1 == 0) y -= Down
      if (from._2 == 0) y -= Left
      
      if (y.contains(Up) && vert(direction((from._1, from._2), Up)._1)(direction((from._1, from._2), Up)._2)._1) y -= Up
      if (y.contains(Right) && vert(direction((from._1, from._2), Right)._1)(direction((from._1, from._2), Right)._2)._1) y -= Right
      if (y.contains(Down) && vert(direction((from._1, from._2), Down)._1)(direction((from._1, from._2), Down)._2)._1) y -= Down
      if (y.contains(Left) && vert(direction((from._1, from._2), Left)._1)(direction((from._1, from._2), Left)._2)._1) y -= Left
      
      if (y.isEmpty) {
        next(vert(from._1)(from._2)._2)
      } 
      else {
        // decide next direction from possible directions randomly
        val dir = y(rand.nextInt(y.size))
        Some(from, dir)
      }
    }
    
    def direction(from: (Int, Int), dir: Direction): (Int, Int) = {
      dir match {
        case Up => (from._1 +1, from._2)
        case Right => (from._1, from._2+1)
        case Down => (from._1 -1, from._2)
        case Left => (from._1, from._2-1)
      }
    }
    
    def generate(): Unit = {
      var i = curr
    
      while (true) {
        val x = next(i)
        if (x == None) return
        maze.breakWall(x.get._1._1, x.get._1._2, x.get._2)
        vert(direction((x.get._1._1, x.get._1._2), x.get._2)._1)(direction((x.get._1._1, x.get._1._2), x.get._2)._2) = (true, (i._1, i._2))
        i = direction((x.get._1._1, x.get._1._2), x.get._2)
      }
    }
    
    generate()
    
    maze
    
  }
}
