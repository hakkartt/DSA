package tests
import org.scalatest._

import java.io.{ File, FileReader, BufferedReader, IOException }
import scala.util.Random
import escape._

class EscapeSpec extends FlatSpec with Matchers {
  val testWorlds = List(
    ("""
@@@@@@@@@@
@WSS..WSW@
@W.WWWWDW@
@W.D*...W@
@SSW.WWWW@
@W...D..W@
@WDWWWWDW@
@@@@@@@@@@
    """, 7)
    ,
    ("""
@@@@@@@@@@
@WSS..WSW@
@W.WWWWDW@
@W.D*....@
@SSW.WWWW@
@W...D..W@
@WDWWWWDW@
@@@@@@@@@@
    """, 4)
    ,
    ("""
@@@@@@@@@@
@WSS..WSW@
@W.WWWWDW@
@W.D*...W@
@SSW.WWWW@
@W...D..W@
@WWWWWWDW@
@@@@@@@@@@
    """, 10)
    ,
    ("""
@@@@@@@@@@@@@@@@@@@@@@@@@@@
@WWDWWWWWWWWWWWWSWWWWWWWWW@
@W.D................S..D.W@
@WSWWWWWWWWWWWWDDWWWWWWW.W@
@D.W.D.......D.D..D...DW.D@
@SDW.WWWWWWWWDWWWWWWWW.W.W@
@W.D.W..........D...DW.W.W@
@W.W.W.WWWWWWWWWWWWS.DSD.W@
@DSWDW.W.DD.S...D..W.W.WSW@
@W.W.W.W.WWWWWDSWWDW.WSS.W@
@WDW.WDW.W...*...W.WSW.S.W@
@W.W.W.W.WWWDWWDWW.WSWSW.W@
@D.WDD.W.S.........W.WDW.W@
@W.S.W.WSWWDWDWDWWWW.W.S.W@
@W.S.D..SD.......SS..S.W.W@
@W.S.SWWDWWSWSWWWSSWWW.W.S@
@WDW...S...S.........S.W.W@
@W.DWWWSWWWSWDWWWWSWSDWW.W@
@W..........D...DS.......W@
@WWSWWDWWWWSSWWWSSWWWWWWWW@
@@@@@@@@@@@@@@@@@@@@@@@@@@@
     """, 34)
    ,
    ("""
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
""", 42)
    )

  "The World.isValidEscapePath method" should "work correctly" in {
    val world = World(testWorlds.head._1)
    val duration = testWorlds.head._2
    world.isValidEscapePath(List((3,4),(3,3),(2,3),(1,3),(0,3))) should be ((true, 31, ""))
    world.isValidEscapePath(List((3,4),(0,0))) should be ((false, 0, "The coordinate (0,0) is not a valid neighbour of the coordinate (3,4)"))
    val p = solver.solve(world)
    p should be (Seq((3,4), (4,4), (5,4), (5,3), (5,2), (6,2), (7,2)))
    world.isValidEscapePath(p) should be ((true, 7, ""))
  }

  "The Solver.solve method" should "work correctly" in {
    for((worldString, shortestDuration) <- testWorlds) {
      val world = World(worldString)
      val path = solver.solve(world)
      withClue("On the world"+worldString) {
        world.isValidEscapePath(path) should be ((true, shortestDuration, ""))
      }
    }
  }
}
