package chapter3

import chapter3.Exercises.List

object Main extends App{
  println(Exercises.x)
  println(Exercises.e8)

  println(List.length(List(1,2,3,4,5)))

  println(List.foldLeft(List(4,3,2,1,5,6), 0)(_ + _))
}
