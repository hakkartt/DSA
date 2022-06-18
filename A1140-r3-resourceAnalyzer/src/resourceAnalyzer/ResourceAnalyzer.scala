/* Author: Tommi Junttila, Aalto University.
 * Only for the use on the Aalto course CS-A1140.
 * Redistribution not allowed.
 */

package resourceAnalyzer

/**
 * An abstract base class for different resource analyzers.
 * Do not modify.
 */
abstract class ResourceAnalyzer {
  /*
   * Introduce a new Task "task" running in the half-closed time interval
   * [task.start,task.end).
   * The tasks shall be introduced in non-decreasing order
   * with respect to their start times.
   */
  def newTask(task: Task)
  /*
   * Return the minimum amount of resources required by
   * the input sequence so far.
   * This should be a really fast constant time operation.
   * */
  def minResources: Int
}


/*
 * A slow reference implementation using an ArrayBuffer and filtering.
 */
class ResourceAnalyzerSlow extends ResourceAnalyzer {
  // For input validation only
  protected var prevStart = -1L
  // Stores the end times of the tasks that are currently "live"
  protected var liveTasksEndTime = scala.collection.mutable.ArrayBuffer[Long]()
  // The maximum load seen so far
  protected var minResources_ = 0
  def newTask(task: Task) = {
    // Input validation: the tasks are given in the order of start time
    require(prevStart <= task.start)
    prevStart = task.start
    // Remove tasks that end before the new start time
    liveTasksEndTime = liveTasksEndTime.filter(_ > task.start)
    // Add the new task end time to the buffer
    liveTasksEndTime.append(task.end)
    // Update the maximum load variable
    minResources_ = minResources_ max liveTasksEndTime.length
  }
  def minResources: Int = minResources_
}


/*
 * A faster implementation using a scala.collection.mutable.PriorityQueue.
 */
import scala.collection.mutable.PriorityQueue

class ResourceAnalyzerFast extends ResourceAnalyzer {
  // For input validation only
  protected var prevStart = -1L
  // Introduce your internal data structures here.
  protected var minResources_ = 0
  protected var endTime = PriorityQueue[Long]()(Ordering.by(-_))

  def newTask(task: Task) = {
    // Input validation: the tasks are given in the order of start time
    require(prevStart <= task.start)
    prevStart = task.start
    
    // removing the ended tasks
    while (!endTime.isEmpty && task.start >= endTime.head) { endTime.dequeue() }
    
    endTime.enqueue(task.end)
    minResources_ = endTime.size max minResources_    
  }
  
  def minResources: Int = minResources_
  
}
