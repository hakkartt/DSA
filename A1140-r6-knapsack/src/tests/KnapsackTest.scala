package tests
import org.scalatest._

import timer._
import knapsack._

class KnapsackSpec extends FlatSpec with Matchers {
  val rand = new scala.util.Random(2016)
  // Some very small test instances
  val instances = Seq(
    (10, Seq((5,10), (4,40), (6,30), (3,50)), 90),
    (400, Seq((9,150), (13,35), (153,200), (50,160),
              (15,60), (68,45), (27,60), (39,40), (23,30),
              (52,10), (11,70), (32,30), (24,15), (48,10),
              (73,40), (42,70), (43,75), (22,80), (7,20),
              (18,12), (4,50), (30,10)), 
     1030)
  ).map({case(maxW, items, sol) =>
    (maxW, items.map(p => Item(p._1, p._2)), sol)})

  // A generator for larger test instances
  def genInstance(n: Int, weightRange: Int, valueRange: Int): Seq[Item] = {
    Seq.tabulate[Item](n)(i => 
      Item(rand.nextInt(weightRange) + 1, rand.nextInt(valueRange) + 1))
  }
  
  // Some helpers for computing the weight and value sums of item sequences
  def weight(items: Seq[Item]): Int = items.map(_.weight).sum
  def value(items: Seq[Item]): Int = items.map(_.value).sum

  "The knapsack.solveRecursive method" should "work correctly" in {
    for ((maxWeight, items, correctValue) <- instances) {
      val (solValue, solItems) = solveRecursive(maxWeight, items)
      val itemsSet = items.toSet
      for (solItem <- solItems) 
        itemsSet.contains(solItem) should be (true)
      weight(solItems) should be <= (maxWeight)
      solValue should be (value(solItems))
      solValue should be (correctValue)
    }
  }
  
  "The knapsack.solveDynProg method" should "work correctly on very small instances" in {
    for ((maxWeight, items, correctValue) <- instances) {
      val (solValue, solItems) = solveDynProg(maxWeight, items)
      val itemsSet = items.toSet
      for (solItem <- solItems) 
        itemsSet.contains(solItem) should be (true)
      weight(solItems) should be <= (maxWeight)
      solValue should be (value(solItems))
      solValue should be (correctValue)
    }
  }

  it should "work correctly on smallish random instances" in {
    val nofTests = 100
    val nofItems = 6
    val itemWeight = 50
    val itemValue = 100
    val maxWeight = 500
    for(testNum <- 1 to nofTests) {
      val items = genInstance(nofItems, itemWeight, itemValue)
      // Run the recursive and dynamic programming algos
      val (valRecursive, solRecursive) = solveRecursive(maxWeight, items)
      val (valDynProg, solDynProg) = solveDynProg(maxWeight, items)
      // Check the solution of the dynamic programming algo
      withClue(f"On an instance with maxWeight=$maxWeight and items="+items+":") {
        weight(solDynProg) should be <= (maxWeight)
        valDynProg should be (value(solDynProg))
        valDynProg should be (valRecursive)
      }
    }
  }
  
  it should "be at least 1000 times faster than the solveRecursive method on 25 items having max weight of 500" in {
    val targetSpeedup = 1000.0
    val nofTests = 10
    val nofItems = 25
    val itemWeight = 50
    val itemValue = 100
    val maxWeight = 500
    //var timeDynProgCumu = 0.0
    //var timeRecursiveCumu = 0.0
    val speedups = new scala.collection.mutable.ArrayBuffer[Double]()
    println("Doing performance tests")
    for(testNum <- 1 to nofTests) {
      println(s" Test $testNum")
      val items = genInstance(nofItems, itemWeight, itemValue)
      // Run the recursive and dynamic programming algos
      println("  Running the recursive reference algorithm")
      val ((valRecursive, solRecursive), timeRecursive) = measureCpuTime {solveRecursive(maxWeight, items) }
      println(f"   Time: $timeRecursive%.3g")
      println("  Running your dynamic programming algorithm")
      val ((valDynProg, solDynProg), timeDynProg) = measureCpuTime {solveDynProg(maxWeight, items) }
      println(f"   Time: $timeDynProg%.3g")
      // Check the solution of the dynamic programming algo
      weight(solDynProg) should be <= (maxWeight)
      valDynProg should be (value(solDynProg))
      valDynProg should be (valRecursive)
      val speedup = timeRecursive / timeDynProg
      println(f"  Speedup: $speedup%.2f")
      speedups.append(speedup)
    }
    val sortedSpeedups = speedups.sorted
    val medianSpeedup = sortedSpeedups(nofTests / 2)
    println(f"The median of speedups: $medianSpeedup%.2f")
    val scale = (medianSpeedup / targetSpeedup) max 0.0
    println(f"""Your algorithm reached ${scale*100}%.1f%% of the full points speedup of $targetSpeedup""")
    println(f""" NOTE: the speedup depends on the machine (CPU, load etc) and
 you get some points even if your code does not reach
 the full points speedup requirement""")
    medianSpeedup should be >= (targetSpeedup)
  }
}
