package tests
import org.scalatest._

import change._

class ChangeSpec extends FlatSpec with Matchers {
  "The change.count method" should "work correctly" in {
    count(10, Set(5,1)) should be (3)
    count(10, Set(10,5,1)) should be (4)
    count(50, Set(100,50,25,10,5,1)) should be (50)
    count(100, Set(50,25,10,5,1)) should be (292)
    count(100, Set(100,50,25,10,5,1)) should be (293)
    count(200, Set(100,50,25,10,5,1)) should be (2728)
    count(1000, Set(100,50,25,10,5,1)) should be (2103596)
    count(31, Set(1,2,4,8,16,32,64)) should be (166)
    count(32, Set(1,2,3,5,7,11,13)) should be (859)
    count(10000, Set(100,50,25,10,5,1)) should be (139946140451L)
    count(989, Set(2,4,8,16,32,64)) should be (0)
  }
}
