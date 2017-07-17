package chapter5

import Stream._
trait Stream[+A] {

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def toListTailRec: List[A] = {
    def loop(acc: List[A], curr: Stream[A]):List[A] = curr match {
      case Empty => acc
      case Cons(h, t) => loop(h() :: acc, t())
    }
    loop(List(), this).reverse
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }
  def take(n: Int): Stream[A] = this match {
    case Cons(h,_) if (n == 1) => cons(h(), Empty)
    case Cons(h, t) if (n > 1) => cons(h(), t().take(n-1))
    case Empty => Empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h, _) if (n == 0) => this
    case Cons(h, t) if (n > 0) => t().drop(n-1)
    case Empty => Empty
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if (p(h())) => cons(h(), t().takeWhile(p))
    case _ => this
  }

  def takeWhileUsingFold(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else empty)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b )

  def headOption: Option[A] =
    foldRight[Option[A]](None)((h,_) => Some(h))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def map[B](f: A => B): Stream[B] =
  foldRight(empty[B])((h,t) => cons(f(h), t))

  def filter[B](f: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) => if(f(h)) cons(h,t) else t)

  def append[B>:A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h,t) => cons(h,t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h,t) => f(h).append(t))

  def mapUnfold[B](f: A => B): Stream[B] = unfold(this){
      case Cons(h,t) => Some((f(h()), t()))
      case Empty => None
  }

  def takeUnfold(n: Int): Stream[A] = unfold((this, n)){
    case (Empty, _) => None
    case (Cons(h,t), i) => if(i == n) None else Some(h(), (t(), i+1))
  }

  def takeWhileUnfold(p: A => Boolean): Stream[A] = unfold(this){
    case (Empty, _) => None
    case Cons(h,t) => if(!p(h())) None else Some(h(), t())
  }

  def zipWith[B,C](other: Stream[B])(f: (A,B) => C): Stream[C] = unfold((this, other)){
    case (Empty, _) => None
    case (_, Empty) => None
    case (Cons(h1,t1), Cons(h2,t2)) => Some(f(h1(),h2()), (t1(), t2()))
  }

  def zipAll[B](other: Stream[B]): Stream[(Option[A], Option[B])] = unfold((this, other)){
    case (Empty, Empty) => None
    case (Empty, Cons(h,t)) => Some((None, Some(h())), (empty, t()))
    case (Cons(h,t), Empty) => Some((Some(h()), None), (t(), empty))
    case (Cons(h1,t1), Cons(h2,t2)) => Some((Some(h1()),Some(h2())), (t1(), t2()))
  }

  def startsWith[B](s: Stream[B]): Boolean = ???
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = {
    lazy val tail:Stream[A] = Cons(() => a, () => tail)
    tail
  }
  def from(n: Int): Stream[Int] = cons(n , from(n+1))

  def fibs(): Stream[Int] = {
    def loop(current: Int, nMinus1: Int): Stream[Int] = cons(current, loop(current + nMinus1, current))
    cons(0, cons (1, loop(1 + 0, 1)))
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((h,s)) => cons(h, unfold(s)(f))
    case None => empty
  }

  def fibsUnfold(): Stream[Int] = unfold(0,1){case (c,n) => Some(c,(n, n+c))}
  def fromUnfold(n: Int): Stream[Int] = unfold(n)(x => Some(x, x+1))
  def constantUnfold[A](n: A): Stream[A] = unfold(n)(x => Some(x,x))
  def onesUnfold(): Stream[Int] = unfold(1)(_ => Some(1,1))
}