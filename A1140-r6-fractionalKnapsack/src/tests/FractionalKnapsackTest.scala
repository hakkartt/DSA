package tests
import org.scalatest._
import scala.util.Random
import timer._
import org.scalactic.TolerantNumerics

import fractionalKnapsack._


class FractionalKnapsackSpec extends FlatSpec with Matchers {
  val epsilon = 1e-4f
  val rand = new scala.util.Random(2407)

  implicit val doubleEq = TolerantNumerics.tolerantDoubleEquality(epsilon)
  
  val instances = List(
    (10.0, Seq(("water",5.0,10.0),("oil",4.0,40.0),
               ("cola",6.0,30.0),("gasoline",3.0,50.0)),
     400.0)
    ,(400.0, Seq(("m1", 9.0,150.0),("m2", 13.0,35.0),("m3", 153.0,200.0),
                 ("m4", 50.0,160.0),("m5", 15.0,60.0),("m6", 68.0,45.0),
                 ("m7", 27.0,60.0),("m8", 39.0,40.0),("m9", 23.0,30.0),
                 ("m10", 52.0,10.0),("m11", 11.0,70.0),("m12", 32.0,30.0),
                 ("m12", 24.0,15.0),("m13", 48.0,10.0),("m14", 73.0,40.0),
                 ("m15", 42.0,70.0),("m16", 43.0,75.0),("m17", 22.0,80.0),
                 ("m18", 7.0,20.0),("m19", 18.0,12.0),("m20", 4.0,50.0),
                 ("m21", 30.0,10.0)), 
      52445.0)
    ,(17.0,Seq(("m0",4.0,11.0),("m1",7.0,4.0),("m2",1.0,20.0),("m3",9.0,14.0),("m4",2.0,9.0),("m5",9.0,3.0),("m6",2.0,20.0)),239.0)
    ,(38.0,Seq(("m0",3.0,3.0),("m1",8.0,6.0),("m2",10.0,19.0),("m3",8.0,19.0),("m4",4.0,1.0),("m5",10.0,6.0),("m6",4.0,16.0),("m7",8.0,1.0),("m8",3.0,9.0),("m9",7.0,11.0),("m10",1.0,14.0),("m11",1.0,15.0),("m12",10.0,13.0)),609.0)
    ,(19.0,Seq(("m0",2.0,12.0),("m1",9.0,5.0),("m2",1.0,14.0),("m3",7.0,7.0),("m4",9.0,6.0),("m5",2.0,17.0),("m6",8.0,9.0)),186.0)
    ,(34.0,Seq(("m0",2.0,5.0),("m1",5.0,15.0),("m2",7.0,8.0),("m3",10.0,4.0),("m4",5.0,15.0),("m5",6.0,7.0),("m6",7.0,1.0),("m7",4.0,11.0),("m8",8.0,6.0),("m9",5.0,18.0),("m10",9.0,12.0)),440.0)
    ,(24.0,Seq(("m0",3.0,13.0),("m1",5.0,12.0),("m2",1.0,17.0),("m3",9.0,1.0),("m4",7.0,2.0),("m5",5.0,12.0),("m6",5.0,6.0),("m7",9.0,19.0),("m8",4.0,13.0)),363.0)
    ,(22.0,Seq(("m0",10.0,17.0),("m1",9.0,1.0),("m2",10.0,6.0),("m3",2.0,2.0),("m4",5.0,10.0),("m5",9.0,13.0)),317.0)
    ,(34.0,Seq(("m0",8.0,5.0),("m1",10.0,20.0),("m2",10.0,3.0),("m3",9.0,14.0),("m4",2.0,10.0),("m5",1.0,13.0),("m6",2.0,12.0),("m7",2.0,19.0),("m8",7.0,3.0),("m9",1.0,1.0),("m10",5.0,20.0),("m11",3.0,9.0),("m12",3.0,10.0),("m13",5.0,8.0)),551.0)
    ,(23.0,Seq(("m0",3.0,18.0),("m1",5.0,16.0),("m2",4.0,9.0),("m3",8.0,6.0),("m4",3.0,6.0),("m5",5.0,2.0),("m6",9.0,17.0),("m7",9.0,5.0)),335.0)
    ,(21.0,Seq(("m0",6.0,2.0),("m1",1.0,10.0),("m2",10.0,12.0),("m3",3.0,17.0),("m4",5.0,1.0),("m5",2.0,18.0),("m6",2.0,14.0),("m7",5.0,20.0),("m8",4.0,17.0),("m9",5.0,6.0)),343.0)
    ,(27.0,Seq(("m0",6.0,10.0),("m1",2.0,14.0),("m2",6.0,1.0),("m3",1.0,6.0),("m4",8.0,11.0),("m5",9.0,12.0),("m6",6.0,2.0),("m7",6.0,13.0),("m8",10.0,10.0)),322.0)
  ).map({case (maxAmount, objects, optimal) => {
    (maxAmount, objects.map({case (n,a,p) => Material(n,a,p)}), optimal)
  }})

  
  "The fractionalKnapsack.solve method" should "work correctly" in {
    for ((maxAmount, materials, optimal) <- instances) {
      val (solValue, solAmounts) = solve(maxAmount, materials)
      val existing = materials.toSet
      var value = 0.0
      var amountSum = 0.0
      solAmounts.length should be <= (materials.length)
      for ((amount, material) <- solAmounts) {
        amount should be <= (material.amount)
        existing.contains(material) should be (true)
        amountSum += amount
        value  += amount * material.valuePerUnit
      }
      withClue("On instance ($maxAmount, $materials):") {
        amountSum should be <= (1.0 + epsilon) * maxAmount
        solValue should be (value +- epsilon)
        value should be (optimal +- epsilon)
      }
    }
  }

  def genInstance(n: Int, amountRange: Int, valueRange: Int): Seq[Material] = {
    Seq.tabulate[Material](n)(i => 
      Material("m"+i, rand.nextInt(amountRange) + 1, rand.nextInt(valueRange) + 1))
  }

  it should "be efficient" in {
    val nofTests = 10
    val n = 100000
    var cumuTime = 0.0
    for(t <- 1 to nofTests) {
      println(f"Test $t")
      val materials = genInstance(n, 20, 20)
      val maxAmount = (materials.map(m => m.amount).sum / 2).toInt.toDouble
      val (sol, time) = timer.measureCpuTime {solve(maxAmount, materials) }
      println(f" Time $time%.3g")
      cumuTime += time
    }
    println(f"Cumulative time $cumuTime%.3g")
    cumuTime should be <= (2.0)
  }
}
