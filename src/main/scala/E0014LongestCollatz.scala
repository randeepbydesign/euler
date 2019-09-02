package main.scala

import java.time.LocalDateTime
import java.time.temporal.{ChronoField, TemporalField}

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

/**
  * find longest collatz chain as defined at https://projecteuler.net/problem=14
  */
object LongestCollatz {
  def apply(upperbound: Long) = {
    bruteForce(upperbound)
  }

  /**
    * Method for solving collatz problem preserving paths to prevent unnecessary computation
    * @param upperbound
    * @return
    */
  def savePaths(upperbound: Long) = {
    var cLengths = scala.collection.mutable.Map(1l -> 1)
    for(i <- 3l to upperbound by 2) {
      var j = nextTerm(i)
      var length: Int = 1
      while(!cLengths.contains(j)) {
        j = nextTerm(j)
        length = length + 1
      }
      cLengths(i) = length + cLengths(j)
    }
    cLengths.maxBy(_._2)
  }

  /**
    * solution using tail recursion
    * @param upperbound
    * @return
    */
  def byTailRecursion(upperbound: Long) = {
    (1l to upperbound)
      .map(x => (x, tailRec(x)))
      .maxBy(_._2)
  }

  @tailrec
  def tailRec(term: Long, length: Int = 0): Int =
    term match {
      case x if x == 1l => length + 1
      case x => tailRec( nextTerm(x), length+1)
    }

  /**
    * Brute force solution
    * @param upperBound
    * @return
    */
  def bruteForce(upperBound: Long) = (1l to upperBound)
    .map(x => (x, toCollatzSequence(x).length))
    .maxBy(_._2)

  def toCollatzSequence(start: Long): ListBuffer[Long] = {
    var term = start
    var retList = ListBuffer(start)
    while(term > 1) {
      term = nextTerm(term)
      retList.addOne(term)
    }
    retList.addOne(1l)
    retList
  }

  def nextTerm(term: Long): Long =
    if(term % 2 == 0) term/2 else (3 * term) + 1

  def range(upperBound: Long) = {
    val start = upperBound / 2
    (if (start % 2 == 0) start + 1 else start) to upperBound by 2
  }

}

object Solver extends App {

  var now = LocalDateTime.now()
  println("tail recursion yields: " + LongestCollatz.byTailRecursion(1000000) + " in " +
    (LocalDateTime.now().getLong(ChronoField.MILLI_OF_DAY) - now.getLong(ChronoField.MILLI_OF_DAY)))

  now = LocalDateTime.now()
  println("path cache yields: " + LongestCollatz.savePaths(1000000) + " in " +
    (LocalDateTime.now().getLong(ChronoField.MILLI_OF_DAY) - now.getLong(ChronoField.MILLI_OF_DAY)))

  now = LocalDateTime.now()
  println("brute force yields: " + LongestCollatz.bruteForce(1000000) + " in " +
    (LocalDateTime.now().getLong(ChronoField.MILLI_OF_DAY) - now.getLong(ChronoField.MILLI_OF_DAY)))
}