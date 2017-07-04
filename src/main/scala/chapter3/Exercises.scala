package chapter3

object Exercises {

  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail:List[A]) extends List[A]

  object List{
    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x,xs) => x + sum(xs)
    }

    def product(ds: List[Double]): Double = ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x, xs) => x * product(xs)
    }

    def apply[A](as: A*): List[A] =
      if(as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

    def foldRight[A,B](as: List[A], z:B)(f:(A, B) => B):B = as match{
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

    /**
      * Exercise 3.2
      */
    def tail[A](xs: List[A]) = xs match {
      case Cons(h, t) => t
      case Nil => Nil
    }

    /**
      * Exercise 3.3
      */
    def setHead[A](h: A, xs: List[A]) = xs match {
      case Cons(_, xs) => Cons(h, xs)
      case _ => Cons(h, Nil)
    }

    /**
      * Exercise 3.4
      */
    def drop[A](l: List[A], n: Int): List[A] = if(n== 0) l
    else l match {
      case Nil => Nil
      case Cons(x, xs) => drop(xs, n-1)
    }

    /**
      * Exercise 3.5
      */
    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
      case Nil => Nil
      case Cons(x, xs) => if(f(x)) dropWhile(xs, f) else l
    }

    /**
      * Exercise 3.6
      */
    def init[A](l: List[A]): List[A] = l match {
      case Nil => Nil
      case Cons(h, Nil) => Cons(h, Nil)
      case Cons(h, t) => Cons(h, init(t))
    }

    /**
      * Exercise 3.9
      */
    def length[A](as:List[A]): Int = foldRight(as, 0)((_, acc) => acc + 1)

    /**
      *
      * Exercise 3.10
      */
    @annotation.tailrec
    def foldLeft[A,B](as:List[A], z:B)(f: (B,A) => B):B = as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

    /**
      * Exercise 3.11
      */
    def sum2(lst: List[Int])= foldLeft(lst, 0)(_ + _)
    def product2(lst:List[Double]) = foldLeft(lst, 1.0)(_ * _)
    def length2[A](lst:List[A]) = foldLeft(lst, 0)((acc, _) => acc + 1)

    /**
      * Exercise 3.12
      */
    def reverse[A](lst: List[A]) = foldLeft(lst, Nil:List[A])((acc, x) => Cons(x, acc))

    /**
      * Exercise 3.13
      */
    def foldRightViaFoldLeft[A,B](lst: List[A], z: B)(f: (A,B) => B):B = {
      foldLeft(lst, (b:B) => b)((g, a) => b => f(a, b))(z)
    }

    def foldLeftViaFoldRight[A,B](lst: List[A], z: B)(f: (B,A) => B):B = {
      foldRight(lst, (b:B) => b)((a, g) => b => (f(b,a)))(z)
    }

    /**
      * Exercise 3.14
      */
    def append[A](l: List[A], r: List[A]) = {
      foldRight(l, r)(Cons(_,_))
    }

    /**
      * Exercise 3.15
      */
    def concat[A](lst: List[List[A]]): List[A] = {
      foldLeft(lst, List():List[A])(append)
    }

  }

  /**
    * Exercise 3.1
    */
  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + List.sum(t)
    case _ => 101
  }

  /**
    * Exercise 3.8
    */
  val e8 = List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))

}