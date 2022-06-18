package maze

/**
 * A small helper that generates a random maze,
 * outputs it to the console, and then
 * visualizes its creation in a swing application.
 */
object testAndVisualize {

  def main(args: Array[String]): Unit = {
    val maze = generator.generate(20, 50)
    Console.println(maze)
    val gui = new GUI(maze)
    gui.go()
  }
}
