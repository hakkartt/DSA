/* Author: Tommi Junttila, Aalto University.
 * Only for the use on the Aalto course CS-A1140.
 * Redistribution not allowed.
 */

package object radixSort {
  /**
   * Least-significant-digit-first radix sort for integer arrays.
   * Sorts the argument array in ascending order.
   * Assumes that all the integers are non-negative.
   * Interpret 'digit' as 'byte' (8 bits) here and use bit-level operations
   *  such as shifting (>> and <<) and masking (&) to extract 'digits'.
   */
  def lsdRadixSort(a: Array[Int]): Unit = {
    val N = a.length
    if(N <= 1) return
    val maxDigits = 4
    
    var aux = new Array[Int](256)
    val secondary = new Array[Int](N)
    val third = new Array[Int](N)
    
    var work = a
    
    var i = 0
    
    //----------------------------1
    while (i < N) {
      aux(work(i).<<(24).>>>(24)) += 1
      i += 1
    }
    
    i = 1
    aux(0) = aux(0)-1
    while (i < aux.size) {    
      aux(i) = (aux(i)+aux(i-1))
      i += 1
    }

    i = N-1
    while (i >= 0) {  
      secondary(aux(work(i).<<(24).>>>(24))) = work(i)
      aux(work(i).<<(24).>>>(24)) -= 1
      i -= 1
    }
    
    //-----------------------------2
    i = 0
    aux = new Array[Int](256)
    work = secondary
    //-----------------------------

   
    
    while (i < N) {
      aux(work(i).<<(16).>>>(24)) += 1
      i += 1
    }
    
    i = 1
    aux(0) = aux(0)-1
    while (i < aux.size) {    
      aux(i) = (aux(i)+aux(i-1))
      i += 1
    }

    i = N -1
    while (i >= 0) {  
      third(aux(work(i).<<(16).>>>(24))) = work(i)
      aux(work(i).<<(16).>>>(24)) -= 1
      i -= 1
    }
    
    //-----------------------------3
    i = 0
    aux = new Array[Int](256)
    work = third
    //-----------------------------
    
    while (i < N) {
      aux(work(i).<<(8).>>>(24)) += 1
      i += 1
    }
    
    i = 1
    aux(0) = aux(0)-1
    while (i < aux.size) {    
      aux(i) = (aux(i)+aux(i-1))
      i += 1
    }
 
    i = N-1
    while (i >= 0) {  
      secondary(aux(work(i).<<(8).>>>(24))) = work(i)
      aux(work(i).<<(8).>>>(24)) -= 1
      i -= 1
    }
    
    //-----------------------------
    i = 0
    aux = new Array[Int](256)
    work = secondary
    //-----------------------------
    
     while (i < N) {
      aux(work(i).<<(0).>>>(24)) += 1
      i += 1
    }
    
    i = 1
    aux(0) = aux(0)-1
    while (i < aux.size) {    
      aux(i) = (aux(i)+aux(i-1))
      i += 1
    }

    i = N-1
    while (i >= 0) {  
      a(aux(work(i).<<(0).>>>(24))) = work(i)
      aux(work(i).<<(0).>>>(24)) -= 1
      i -= 1
    }

  }
}