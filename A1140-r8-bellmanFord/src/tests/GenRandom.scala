package tests

import scala.collection.mutable.ArrayBuffer

object genRandom {
  def gen(nofVertices: Int, nofEdges: Int, minWeight: Int, maxWeight: Int, seed: Int = scala.util.Random.nextInt()): Seq[(Int,Int,Int)] = {
    require(nofVertices >= 2)
    require(nofEdges >= nofVertices)
    require(minWeight <= maxWeight)
    val rand = new PRNG(seed)
    val edges = new ArrayBuffer[(Int,Int,Int)]()
    for(v <- 1 until nofVertices) edges += ((0, 0, v))
    while(edges.length < nofEdges) {
      val s = rand.nextInt(nofVertices-1)+1
      val t = rand.nextInt(nofVertices-1)+1
      val w = rand.nextInt(maxWeight-minWeight+1)+minWeight
      if(s != t)
        edges += ((s, w, t))
    }
    edges.toList
  }
}
