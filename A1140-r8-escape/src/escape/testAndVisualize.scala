package escape

/**
 * A small helper that visualizes the escape route in a Swing application.
 */
object testAndVisualize {
  val worldString2 = """
@@@@@@@@@@@@@@@@
@WSS..WSWWWWWSS@
@W.WWWWSW..D..S@
@W.DD....*WWW.W@
@SSW.WWWW..W..W@
@W...D..WS.DSDW@
@WWWWWWDWWWW..W@
@@@@@@@@@@@@@@@@
  """
  val worldString = """
@@@@@@@@@@@@@@@@@@@@@@@@@@@
@SWDWWWDWSWWSWWWWWWWWWWDWW@
@S......D.........D..S...W@
@WSSDDSWWWWWWWWDWWSWWWDW.W@
@W.W.......S...D...DSD.S.W@
@WDS.WWWWSWWWWSWWSWSWW.W.W@
@D.WSW...S......SD.D.WSW.W@
@W.D.W.WWWWWDWWDWWWW.W.W.D@
@D.W.W.D...S......SW.W.W.W@
@W.W.WDW.WWWSWWWWW.W.W.DSW@
@WDW.W.W.W.D*.D..W.W.WDD.W@
@WDW.W.SSWWWWWWWSD.SDW.W.W@
@W.W.S.W...........W.W.W.W@
@W.WDW.WWSWWWWWWWWWW.D.W.S@
@D.W.W.....D.DS.....SW.W.W@
@WSS.DWWDWWWWWWWWWWWWS.W.W@
@W.WS.......S..........W.D@
@W.WSWWWWWWWDWWWWSWWWWWW.W@
@W.D....SS..DDD.S........W@
@WWWWWWWWWWWWWWWWWWWWWWWDW@
@@@@@@@@@@@@@@@@@@@@@@@@@@@
"""

  def main(args: Array[String]): Unit = {
    val world = World(worldString)
    Console.println(world)
    val path = solver.solve(world)
    assert(world.isValidEscapePath(path)._1 == true)
    println(world.isValidEscapePath(path))
    val gui = new GUI(world, solver.solve(world))
    gui.go()
  }
}
