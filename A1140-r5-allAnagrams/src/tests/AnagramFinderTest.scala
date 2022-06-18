package tests
import org.scalatest._

import scala.util.Random
import allAnagrams._

class AllAnagramsSpec extends FlatSpec with Matchers {
  val kotusWords = kotusReader("data/kotus-sanalista-v1.zip")
  val rand = new scala.util.Random(5678)
  def randomPermute(word: String): String = {
    rand.shuffle(word.toList).mkString("")
  }

  "The find method" should "work correctly" in {
    val testCases = List(("latistua", Set("latistua", "altistua", "sulittaa", "lasittua", "istualta", "tulistaa"))
                         ,("kaars", Set("sarka", "raksa", "rakas", "askar"))
                         ,("takko",Set("tokka", "kotka", "katko"))
                       )
    println("Evaluating correctness with some test cases")
    val (finder, initTime) = timer.measureCpuTime {
      new AnagramFinder(kotusWords)
    }
    println(f" Anagram finder initialization took $initTime%.3g seconds")
    for((word, correct) <- testCases) {
      val result = finder.find(word)
      withClue(s"With word '$word':") {
        result should be (correct)
      }
    }
  }

  it should "be efficient" in {
    println("Evaluating efficiency")
    val nofQueries = 100000
    val queries = (0 until nofQueries).map(i => randomPermute(kotusWords(rand.nextInt(kotusWords.length))))
    val (finder, initTime) = timer.measureCpuTime {
      new AnagramFinder(kotusWords)
    }
    println(f" Anagram finder initialization took $initTime%.3g seconds")
    initTime should be < (2.0)
    println(f" Perfoming $nofQueries queries")
    val (r, time) = timer.measureCpuTime {
      for(query <- queries) {
        val result = finder.find(query)
      }
    }
    println(f"Perfomed $nofQueries queries in $time%.3g seconds")
    time should be <= (1.0)
  }
}
