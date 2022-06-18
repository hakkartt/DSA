/* Author: Tommi Junttila, Aalto University.
 * Only for the use on the Aalto course CS-A1140.
 * Redistribution not allowed.
 */

package resourceAnalyzer

/**
 * A simple immutable class for task information.
 * A task starts at the beginning of the time unit "start" and
 * runs "duration" time units.
 * Thus it is running during the half-closed interval [start,start+duration).
 */
case class Task(val start: Long, val duration: Long) {
  require(start >= 0, "The starting time of the task should be non-negative")
  require(duration > 0, "The duration of the task should be positive")
  def end: Long = start + duration
  override def toString: String = s"Task[$start,$end)"
}
