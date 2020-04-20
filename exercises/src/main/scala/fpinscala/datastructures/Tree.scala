package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(n)      => f(n)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))

  }
  // def size[A](t: Tree[A]): Int = t match {
  //   case Leaf(_)      => 1 // A => B
  //   case Branch(l, r) => 1 + size(l) + size(r) Tree[A], Tree[A] => 1 +
  // }

  def size[A](t: Tree[A]): Int =
    fold(t) { n => 1 } { (l, r) => 1 + l + r }

  // def maximum(t: Tree[Int]): Int = t match {
  //   case Leaf(v)      => v
  //   case Branch(l, r) => maximum(l) max maximum(r)
  // }

  def maximum(t: Tree[Int]): Int = fold(t)(n => n)((l, r) => l max r)

  // def depth(t: Tree[Int]): Int = t match {
  //   case Leaf(_)      => 1
  //   case Branch(l, r) => (1 + depth(l)) max (1 + depth(r))
  // }

  def depth(t: Tree[Int]): Int = fold(t)(n => 1)((l, r) => (1 + l) max (1 + r))

  // def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
  //   case Leaf(x)      => Leaf(f(x))
  //   case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  // }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(n => Leaf(f(n)): Tree[B])(Branch(_, _))
}
