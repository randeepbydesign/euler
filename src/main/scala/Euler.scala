package main.scala


class Pythag {
  def solve(goal: Int): Option[(Int, Int, Int)] =
    (for {
      a <- Stream.range(goal/2, 1, -1)
      if (a < goal)
      b <- a to 1 by -1
      c <- Some(goal - a - b)
    } yield (a, b, c))
      .find { case (a, b, c) => {
        (math.pow(a, 2) + math.pow(b, 2) == math.pow(c, 2))
      } }
}

object Runner extends App {
  val p = new Pythag().solve(1000)
  println("p is " + p)

  val x = (Range(3, 1000, 3).toSet union Range(5, 1000, 5).toSet)
    .fold(0)((a, b) => a + b)
  println("fold res is " + x)
}