package object change {
  
  def count(amount: Int, denominations: Set[Int]): Long = {
    require(amount > 0)
    require(denominations.forall(d => d > 0))
    
    var d = denominations.toSeq
    var a = Array.ofDim[Long](amount + 1)
    a(0) = 1
    var i = 0
    // initialize change
    var c = 0
    while (i < d.length) {
      c = d(i)
      while (c <= amount) {
        a(c) += a(c - d(i))
        c += 1
      }
      i += 1
    }
    
    a.last
    
  }
}
