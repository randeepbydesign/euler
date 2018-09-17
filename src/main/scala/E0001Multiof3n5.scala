package main.scala

object MultipleSolver {
  def apply(limit: Int, multipleList: List[Int]) =
    multipleList.map(factor => Range(factor, limit, factor))
      .flatten.distinct.fold(0)(_ + _)
}

object Solver0001 extends App {
  val result = MultipleSolver(1000, List(3, 5))
  println("Solution is " + result)
}