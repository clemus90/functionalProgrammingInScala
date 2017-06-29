package chapter2

object Exercises {
  def fib(n: Int): Int = {
    if(n <= 1) n
    else fib(n-1) + fib(n-2)
  }

  /**
    * tail recursive version
    */
  def fib2(n: Int): Int = {
    @annotation.tailrec
    def loop(remaining:Int, iMinus1:Int, iMinus2:Int):Int = {
      if(remaining==0) iMinus1 + iMinus2
      else loop(remaining-1, iMinus1 + iMinus2, iMinus1)
    }
    if(n<=1)n
    else loop(n-2,1,0)
  }

  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean = {
      if(n >= as.length) true
      else if(ordered(as(n-1), as(n))) loop(n+1)
      else false
    }
    if(as.length <= 1) true
    else loop(1)
  }

  def partial1[A,B,C](a: A, f: (A,B) => C): B => C = (b: B) => f(a, b)

  def curry[A,B,C](f: (A,B) => C): A => (B=>C) = (a: A) => partial1(a, f)

  def uncurry[A,B,C](f: A => B => C): (A,B) => C = (a: A, b: B) => f(a)(b)

  def compose[A,B,C](f: B=>C, g: A=>B): A => C = (a:A) => f(g(a))
}
