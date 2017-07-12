package chapter3

sealed trait Tree[+A]
case class Leaf[A](value:A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(x) => x
    case Branch(l,r) => maximum(l) max maximum(r)
  }

  def depth(t: Tree[Int]): Int = t match {
    case Leaf(x) => 1
    case Branch(l,r) => 1 + depth(l) max depth(r)
  }

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(x) => Leaf(f(x))
    case Branch(l,r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A,B](t: Tree[A])(f: A => B)(g:(B,B) => B): B = t match {
    case Leaf(x) => f(x)
    case Branch(l,r) => g(fold(t)(f)(g), fold(t)(f)(g))
  }

  def size2(t:Tree[Int]): Int = fold(t)(x => 1)(1 + _ + _)
  def maximum2(t:Tree[Int]): Int = fold(t)(x => x)(_ max _)
  def depth2(t:Tree[Int]): Int = fold(t)(x => 1)((d1, d2) => 1 + (d1 max d2))
  def map2[A,B](t: Tree[A])(f: A => B): Tree[B] = fold[A,Tree[B]](t)(x => Leaf(f(x)))((l,r) => Branch(l, r))
}
