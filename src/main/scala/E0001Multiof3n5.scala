package main.scala

/**
  * Returns the sum of multiples of the list of input numbers below a specified limit
  */
object MultipleSolver {
  def apply(limit: Int, multipleList: List[Int]) =
    multipleList.map(factor => Range(factor, limit, factor))
      .flatten.distinct.fold(0)(_ + _)
}

object Solver0001 extends App {
  val result = MultipleSolver(1000, List(3, 5))
  println("Solution is " + result)
}