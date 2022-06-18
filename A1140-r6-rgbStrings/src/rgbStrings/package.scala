package object rgbStrings {
  /**
   * Count the number of all strings of length n over
   * the characterst 'r', 'g', and 'b'
   * in which it holds that
   * 'r' is never next to 'g' and vise versa.
   * For instance, when 'length' is 3, then the answer is 17 (
   * the strings are rrr, rrb, rbr, rbg, rbb, ggg, ggb, gbr, gbg, rbb,
   * brr, brb, bgg, bgb, bbr, bbg, and bbb ).
   * Note: the numbers can grow VERY large => use BigInt in all the computations.
   * Note: we are only interested in the number of such strings and one should NOT explicitly generate any of them. Thus your code should NOT contain any vars or vals of type String or Char.
   */
  
  def count(n: Int): BigInt = {
    require(n > 0)
    
    // r(1) = 1
    // g(1) = 1
    // b(1) = 1
    //
    // r(i) = r(i-1) + b(i-1)
    // g(i) = g(i-1) + b(i-1)
    // b(i) = b(i-1) + r(i-1) + g(i-1)
    
    val R = scala.collection.mutable.Map[Int, BigInt]()
    val G = scala.collection.mutable.Map[Int, BigInt]()
    val B = scala.collection.mutable.Map[Int, BigInt]()
    
    def r(i: Int): BigInt = R.getOrElseUpdate(i, {
      if (i==1) 1
      else r(i-1) + b(i-1)
    })
    
    def g(i: Int): BigInt = G.getOrElseUpdate(i, {
      if (i==1) 1
      else g(i-1) + b(i-1)
    })
    
    def b(i: Int): BigInt = B.getOrElseUpdate(i, {
      if (i==1) 1 else b(i-1) + r(i-1) + g(i-1)
    })
    
    r(n) + g(n) + b(n)
    
  }
}
