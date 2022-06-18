/* Author: Tommi Junttila, Aalto University.
 * Only for the use on the Aalto course CS-A1140.
 * Redistribution not allowed.
 */

package tests
import org.scalatest._

import timer._
import resourceAnalyzer._

class ResourceAnalyzerSpec extends FlatSpec with Matchers {
  val rand = new scala.util.Random(2105)

  val simpleTestInstances = Seq(
    (List(Task(1000,1000),Task(1010,1000),Task(1999,1000)), 3)
    ,(List(Task(1000,1000),Task(1010,1000),Task(2000,1000)), 2)
    ,(List(Task(1,2),Task(1,2),Task(3,2),Task(3,1),Task(3,3)), 3)
    ,(List(Task(1,2),Task(1,2),Task(1,3),Task(3,1),Task(5,3)), 3)
    ,(List(Task(154,437),Task(233,409),Task(872,378)), 2)
    ,(List(Task(415,433),Task(468,85),Task(682,903)), 2)
    ,(List(Task(415,433),Task(468,214),Task(682,903)), 2)
    ,(List(Task(415,433),Task(468,215),Task(682,903)), 3)
  )

  "The class ResourceAnalyzerSlow" should "work correctly" in {
    for((instance, solution) <- simpleTestInstances) {
      withClue("On instance "+instance) {
        val analyzer = new ResourceAnalyzerSlow()
        for(task <- instance) {
          analyzer.newTask(task)
          analyzer.minResources
        }
        analyzer.minResources should be (solution)
      }
    }
  }
  
  "The class ResourceAnalyzerFast" should "work correctly" in {
    for((instance, solution) <- simpleTestInstances) {
      withClue("On instance "+instance) {
        val analyzer = new ResourceAnalyzerFast()
        for(task <- instance) {
          analyzer.newTask(task)
          analyzer.minResources
        }
        analyzer.minResources should be (solution)
      }
    }
  }

  it should "be at least ten times faster than ResourceAnalyzerSlow on inputs 50000 tasks spanning 10000 time units" in {
    val nofTasks = 50000
    val timeSpan = 10000
    val nofTests = 15
    var timeCumu = 0.0
    var timeSlowCumu = 0.0
    println("Doing performance tests")
    for(testNum <- 1 to nofTests) {
      val instance = Seq.tabulate[Task](nofTasks)(i => {
        val start = rand.nextInt(timeSpan-500)
        val end = rand.nextInt(500)+1
        Task(start, end)
      }).sortBy(_.start)
      val (solutionSlow, timeSlow) = measureCpuTime {
        val analyzer = new ResourceAnalyzerSlow()
        for(task <- instance) {
          analyzer.newTask(task)
          analyzer.minResources
        }
        analyzer.minResources
      }
      val (solution, time) = measureCpuTime {
        val analyzer = new ResourceAnalyzerFast()
        for(task <- instance) {
          analyzer.newTask(task)
          analyzer.minResources
        }
        analyzer.minResources
      }
      println(f" time vs timeSlow: $time%.3g vs $timeSlow%.3g")
      solution should be (solutionSlow)
      timeCumu += time
      timeSlowCumu += timeSlow
    }
    val timeAvg = timeCumu / nofTests
    val timeSlowAvg = timeSlowCumu / nofTests
    println(f" timeAvg vs timeSlowAvg: $timeAvg%.3g vs $timeSlowAvg%.3g")
    val requiredSpeedup = 5.0
    val speedup = timeSlowAvg / timeAvg
    println(f" speedup: $speedup%.3g")
    speedup should be >= (requiredSpeedup)
  }
}
