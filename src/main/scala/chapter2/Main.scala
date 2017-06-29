package chapter2
object Main extends App{
  println(Exercises.fib(2))
  println(Exercises.fib(3))
  println(Exercises.fib(4))
  println(Exercises.fib2(5))
  println(Exercises.fib2(6))
  println(Exercises.fib2(7))

  println(Exercises.isSorted(Array(1,2,2,2,3,4,5,6), (x:Int ,y: Int) => x <= y))
  println(Exercises.isSorted(Array(1,2,2,2,3,7,5,6), (x:Int ,y: Int) => x <= y))

}
