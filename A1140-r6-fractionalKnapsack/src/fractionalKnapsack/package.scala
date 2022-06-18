package object fractionalKnapsack {
  /**
   * Solves fractional knapsack problem instances.
   * Input
   * - maxAmount is the maximum amount that the container can hold
   * - materials gives the available materials
   * Output is a pair with
   * - the maximum profit we can get
   * - and a sequence of pairs of form (a,m) describing how the maximum
   *   profit can be get, a pair (a,m) stating that we should include
   *   the amount a of material m
   */
  def solve(maxAmount: Double, materials: Seq[Material]): (Double, Seq[(Double, Material)]) = {
    require(maxAmount >= 0.0)
    
    val n = materials.size
    var x: Double = 0
    var w: Double = 0
    val sorted = materials
    .sortBy(_.valuePerUnit * -1)
    .map(n => (0: Double, n))
    .toArray
    var i = 0
    while (i < n) {
      val y = sorted(i)._2
      if (w + y.amount <= maxAmount) {
        sorted(i) = ((y.amount, y))
        x += y.amount * y.valuePerUnit
        w += y.amount
      } 
      else {
        val z = maxAmount - w
        sorted(i) = ((z, y))
        x += z * y.valuePerUnit
        w += z
        return((x, sorted.takeWhile(_._1 != 0)))
      }
      i += 1
    }
    ((x, sorted.takeWhile(_._1 != 0)))
  }
}
