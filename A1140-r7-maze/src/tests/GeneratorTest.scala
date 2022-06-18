package tests
import org.scalatest._

import maze._

class GeneratorSpec extends FlatSpec with Matchers {
  "The generator object" should "generate mazes in which all cells are reachable from (0,0)" in {
    val maze = generator.generate(20, 30, 2105)
    maze.checkReachable should be (true)
  }

  /**
   * Check for acyclicity is not provided, feel free to write one if you like.
   * Acyclicity will be tested in the home assignment grader.
   */
}
