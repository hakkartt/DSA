package movingMinMax

abstract class Filter[A <% Ordered[A]](val windowSize: Int) {
  /**
   * Insert the value to the moving window, remove the earliest one, and
   * return the minimum and maximum values in the current window.
   */
  def insert(value: A): (A, A)
}


/**
 * The slow moving min-max filter class given as a reference.
 * Each "insert" call takes time \Omega(windowSize).
 * Do not modify the code in this class.
 */
class SlowFilter[A <% Ordered[A]](windowSize: Int) extends Filter[A](windowSize) {
  val queue = new scala.collection.mutable.Queue[A]()
  
  /**
   * Insert the value to the moving window, remove the earliest one, and
   * return the minimum and maximum values in the current window.
   */
  def insert(value: A): (A, A) = {
    // If the window (first-in-first-out queue) is full,
    // remove the oldest value in it
    if(queue.size == windowSize)
      queue.dequeue
    // Insert the new value in the window
    queue.enqueue(value)
    // Find the smallest and largest values in the window.
    // Takes linear time w.r.t. the windowsize.
    // The following is faster than just saying (queue.min, queue.max)
    // because only one pass over the queue is made.
    var min = queue.head
    var max = queue.head
    for(v <- queue) {
      if(v < min) min = v
      if(v > max) max = v
    }
    (min, max)
  }
}


/**
 * The faster moving min-max filter class.
 * Each "insert" should run in time O(log windowSize).
 */
class FastFilter[A <% Ordered[A]](windowSize: Int) extends Filter[A](windowSize) {
  val queue = new scala.collection.mutable.Queue[A]()
  /* Insert additional data structures here */
  val map = new java.util.TreeMap[A, Int]()
  /**
   * Insert the value to the moving window, remove the earliest one, and
   * return the minimum and maximum values in the current window.
   */
  def insert(value: A): (A, A) = {
    // Follow the same logic as in SlowFilter.insert but
    // use ordered maps, in addition to the queue,
    // to keep track of the values in the current window.
    // As ordered maps such as Java's and Scala's TreeMaps allow
    // for searching, inserting, and finding the minimum and maximum keys
    // in logarithmic time, the whole method can be made to work
    // in logarithmic time w.r.t. the windowSize.
    // You only have to figure out what information to use as the keys and
    // as the values in the map, and how to maintain this information
    // when a value is removed from or inserted to the window/queue.
    // Hint: ordered sets are probably not sufficient for tracking the queue
    // contents because the window may contain multiple occurrences of a value.
    
    // If size limit gets topped -> dequeue
    if (queue.size == windowSize){
      val first = queue.dequeue()
      if (map.get(first) > 1) {
        map.put(first, map.get(first) - 1)
      } 
      else {
        map.remove(first)
      }
    }
    
    // Insert the new value in
    queue.enqueue(value)
    
    if (map.get(value).isInstanceOf[Int]) map.put(value, map.get(value) + 1)
    else map.put(value, 1)
    
    
    (map.firstKey, map.lastKey)
    
  }
}
