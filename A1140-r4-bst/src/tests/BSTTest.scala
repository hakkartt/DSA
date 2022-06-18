package tests
import org.scalatest._
import scala.reflect.ClassTag
import scala.collection.mutable.{Set,Map}

import bst._

class BSTSpec extends FlatSpec with Matchers {
  val rand = new scala.util.Random(2105)

  /* A helper for getting all the (key,value) pairs in the treemap */
  def treeToSet[Key: ClassTag,Value](tree: TreeMap[Key,Value]): Set[(Key,Value)] = {
    val result = Set[(Key,Value)]()
    def inner(node: Node[Key,Value]): Unit = {
      if(node == null) return
      result += ((node.key,node.value))
      inner(node.left)
      inner(node.right)
    }
    inner(tree.root)
    result
  }

  def randIntStringArray(n: Int, maxValue: Int): Array[(Int,String)] = {
    require(n >= 0)
    Array.tabulate[(Int,String)](n)(i => (rand.nextInt(maxValue), "v"+i))
  }

  "The TreeMap class" should "implement insert correctly" in {
    val nofTests = 100
    val nofPairs = 15
    val maxKeyValue = 20
    for(test <- 1 to nofTests) {
      val pairs = randIntStringArray(nofPairs, maxKeyValue)
      // A reference for recording the keys in the tree: a mutable set
      val refMap = Map[Int,String]()
      // Start from the empty tree and insert keys one-by-one
      val tree = new TreeMap[Int,String]()
      for(i <- 0 until pairs.length) {
        val (key,value) = pairs(i)
        // Insert the (key,value) pair
        val oldValue = tree.insert(key, value)
        withClue("After inserting "+pairs.take(i+1).mkString(",")+", the tree is:\n"+tree.prettyString+":") {
          // The return value should be correct
          oldValue should be (refMap.get(key))
          refMap(key) = value
          // The size of the tree should be correct
          tree.size should be (refMap.size)
          // The keys in the tree should be correct
          treeToSet(tree) should be (refMap.toSet)
          // The BST property should hold in the tree
          tree.isValidBST() should be (true)
        }
      }
    }
  }

  it should "implement get correctly" in {
    val nofTests = 100
    val nofPairs = 15
    val maxKeyValue = 20
    for(test <- 1 to nofTests) {
      val pairs = randIntStringArray(nofPairs, maxKeyValue)
      // A reference for recording the keys in the tree: a mutable set
      val refMap = Map[Int,String]()
      // Start from the empty tree and insert keys one-by-one
      // After each insert, the get method should work correctly
      val tree = new TreeMap[Int,String]()
      for(i <- 0 until pairs.length) {
        val (key,value) = pairs(i)
        // Insert the (key,value) pair
        val oldValue = tree.insert(key, value)
        withClue("After inserting "+pairs.take(i+1).mkString(",")+", the tree is:\n"+tree.prettyString+":") {
          // The return value should be correct
          oldValue should be (refMap.get(key))
          refMap(key) = value
          // The size of the tree should be correct
          tree.size should be (refMap.size)
          // The keys in the tree should be correct
          treeToSet(tree) should be (refMap.toSet)
          // The BST property should hold in the tree
          tree.isValidBST() should be (true)
          // The get method should work properly
          for(k <- 0 to maxKeyValue)
            withClue("when getting the value for the key "+k+":") {
              tree.get(k) should be (refMap.get(k))
            }
        }
      }
    }
  }

  it should "implement min correctly" in {
    val nofTests = 100
    val nofPairs = 15
    val maxKeyValue = 20
    for(test <- 1 to nofTests) {
      val pairs = randIntStringArray(nofPairs, maxKeyValue)
      // A reference for recording the keys in the tree: a mutable set
      val refMap = Map[Int,String]()
      // Start from the empty tree and insert keys one-by-one
      // After each insert, the min method should work correctly
      val tree = new TreeMap[Int,String]()
      for(i <- 0 until pairs.length) {
        val (key,value) = pairs(i)
        // Insert the (key,value) pair
        val oldValue = tree.insert(key, value)
        withClue("After inserting "+pairs.take(i+1).mkString(",")+", the tree is:\n"+tree.prettyString+":") {
          // The return value should be correct
          oldValue should be (refMap.get(key))
          refMap(key) = value
          // The size of the tree should be correct
          tree.size should be (refMap.size)
          // The keys in the tree should be correct
          treeToSet(tree) should be (refMap.toSet)
          // The BST property should hold in the tree
          tree.isValidBST() should be (true)
          // The min method should work properly
          tree.min should be (Some(refMap.keys.min))
        }
      }
    }
  }

  it should "implement ceiling correctly" in {
    val nofTests = 100
    val nofPairs = 15
    val maxKeyValue = 20
    for(test <- 1 to nofTests) {
      val pairs = randIntStringArray(nofPairs, maxKeyValue)
      // A reference for recording the keys in the tree: a mutable set
      val refMap = Map[Int,String]()
      // Start from the empty tree and insert keys one-by-one
      // After each insert, the ceiling method should work correctly
      val tree = new TreeMap[Int,String]()
      for(i <- 0 until pairs.length) {
        val (key,value) = pairs(i)
        // Insert the (key,value) pair
        val oldValue = tree.insert(key, value)
        withClue("After inserting "+pairs.take(i+1).mkString(",")+", the tree is:\n"+tree.prettyString+":") {
          // The return value should be correct
          oldValue should be (refMap.get(key))
          refMap(key) = value
          // The size of the tree should be correct
          tree.size should be (refMap.size)
          // The keys in the tree should be correct
          treeToSet(tree) should be (refMap.toSet)
          // The BST property should hold in the tree
          tree.isValidBST() should be (true)
          // The ceiling method should work properly
          for(k <- -2 to maxKeyValue+2) {
            val tmp = refMap.keys.filter(_ >= k) //SLOOOW
            val ref = if(tmp.isEmpty) None else Some(tmp.min)
            withClue("when getting the ceiling for the key "+k+":") {
              tree.ceiling(k) should be (ref)
            }
          }
        }
      }
    }
  }

  it should "implement remove correctly" in {
    val nofTests = 100
    val nofPairs = 15
    val maxKeyValue = 20
    for(test <- 1 to nofTests) {
      val pairs = randIntStringArray(nofPairs, maxKeyValue)
      // A reference for recording the keys in the tree: a mutable set
      val refMap = Map[Int,String]()
      // Start from the empty tree and insert keys one-by-one
      val tree = new TreeMap[Int,String]()
      for(i <- 0 until pairs.length) {
        val (key,value) = pairs(i)
        // Insert the (key,value) pair
        val oldValue = tree.insert(key, value)
        withClue("After inserting "+pairs.take(i+1).mkString(",")+", the tree is:\n"+tree.prettyString+":") {
          // The return value should be correct
          oldValue should be (refMap.get(key))
          refMap(key) = value
          // The size of the tree should be correct
          tree.size should be (refMap.size)
          // The keys in the tree should be correct
          treeToSet(tree) should be (refMap.toSet)
          // The BST property should hold in the tree
          tree.isValidBST() should be (true)
        }
      }
      // Remove the keys one-by-one,
      // the tree should be consistent after each remove
      val removes = rand.shuffle(refMap.keys.toSeq)
      for(i <- 0 until removes.length) {
        val key = removes(i)
        assert(refMap.get(key) != None)
        // Remove the key
        val oldValue = tree.remove(key)
        oldValue should be (refMap.get(key))
        refMap.remove(key)
        withClue("After inserting "+pairs.mkString(",")+" and removing the keys "+removes.take(i+1).mkString(",")+", the tree is:\n"+tree.prettyString+":") {
          // The size of the tree should be correct
          tree.size should be (refMap.size)
          // The keys in the tree should be correct
          treeToSet(tree) should be (refMap.toSet)
          // The BST property should hold in the tree
          tree.isValidBST() should be (true)
        }
      }
    }
  }
}
