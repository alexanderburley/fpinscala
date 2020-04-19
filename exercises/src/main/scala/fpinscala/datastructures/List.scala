package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int =
    ints match { // A function that uses pattern matching to add up a list of integers
      case Nil => 0 // The sum of the empty list is 0.
      case Cons(x, xs) =>
        x + sum(
          xs
        ) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
    }

  def product(ds: List[Double]): Double = ds match {
    case Nil          => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs)  => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _)))          => x
    case Nil                                   => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t)                            => h + sum(t)
    case _                                     => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil        => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil         => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(
      _ * _
    ) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  // Alternatively should throw exception
  def tail[A](l: List[A]): List[A] = l match {
    case Cons(_, t) => t
    case Nil        => Nil
  }

  def head[A](l: List[A]): A = l match {
    case Cons(h, _) => h
    case Nil        => throw new Exception
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Cons(_, t) => Cons(h, t)
    case Nil        => Cons(h, Nil)
  }

  def drop[A](l: List[A], n: Int): List[A] =
    if (n < 1) l
    else drop(tail(l), n - 1)

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _                  => l

  }

  def filter[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) =>
      if (f(h)) dropWhile(t, f)
      else Cons(h, dropWhile(t, f))
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil          => Nil
    case Cons(_, Nil) => Nil
    case Cons(h, t)   => Cons(h, init(t))
  }

  // def length[A](l: List[A]): Int = l match {
  //   case Nil        => 0
  //   case Cons(_, t) => 1 + length(t)
  // }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, acc) => acc + 1)

  @annotation.tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil        => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def sum3(ns: List[Int]) = foldLeft(ns, 0)(_ + _)

  def product3(ns: List[Double]) = foldLeft(ns, 1.0)(_ * _)

  def length3[A](ns: List[A]): Int = foldLeft(ns, 0)((acc, _) => acc + 1)

  def reverse[A](l: List[A]): List[A] = {
    def go(l: List[A], acc: List[A]): List[A] = l match {
      case Nil        => acc
      case Cons(h, t) => go(t, Cons(h, acc))
    }
    go(l, List())
  }

  def reverse2[A](l: List[A]): List[A] =
    foldRight(l, List[A]())((x, xs) => append(xs, List(x)))

  def reverse3[A](l: List[A]): List[A] =
    foldLeft(l, List[A]())((xs, x) => Cons(x, xs))

  def foldLeftWithFoldRight[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(reverse(l), z)((x, y) => f(y, x))

  def foldRightWithFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((x, y) => f(y, x))

  def append2[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)((x, xs) => Cons(x, xs))

  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r)(Cons(_, _))

  def append3[A](a1: List[A], a2: List[A]): List[A] =
    foldLeft(a2, a1)((xs, x) => reverse(Cons(x, reverse(xs))))

  def flatten[A](l: List[List[A]]): List[A] =
    foldLeft(l, List[A]())(append2)

  def concat[A](l: List[List[A]]): List[A] = // Official answer
    foldRight(l, Nil: List[A])(append)

  def add1(l: List[Int]): List[Int] =
    foldRight(l, Nil: List[Int])((h, t) => Cons(h + 1, t))

  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, Nil: List[String])((h, t) => Cons(h.toString, t))

  def map[A, B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil: List[B])((x, xs) => Cons(f(x), xs))

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] =
    flatten(map(l)(f))

  def filter2[A](l: List[A], f: A => Boolean): List[A] =
    flatMap(l)((x) => if (f(x)) Nil else List(x))

  def zipWithInt(lx: List[Int], ly: List[Int]): List[Int] = lx match {
    case Nil => lx
    case Cons(hx, tx) =>
      ly match {
        case Nil          => lx
        case Cons(hy, ty) => Cons(hx + hy, zipWithInt(tx, ty))
      }
  }

  def zipWith[A](lx: List[A], ly: List[A])(f: (A, A) => A): List[A] =
    lx match {
      case Nil => lx
      case Cons(hx, tx) =>
        ly match {
          case Nil          => lx
          case Cons(hy, ty) => Cons(f(hx, hy), zipWith(tx, ty)(f))
        }
    }
}
