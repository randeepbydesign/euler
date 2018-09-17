package main.scala

object PythagTriplet {

  def apply(goal: Int): Option[(Int, Int, Int)] =
    (for {
      a <- Stream.range(goal / 2, 1, -1) //calculating the upperbound more scientifically would benefit performance greatly
      if (a < goal)
      b <- a to 1 by -1
      c <- Some(goal - a - b)
    } yield (a, b, c))
      .find {
        case (a, b, c) => {
          (math.pow(a, 2) + math.pow(b, 2) == math.pow(c, 2))
        }
      }
}

object Solver0009 extends App {
  println("Solution is " + PythagTriplet(1000))
}