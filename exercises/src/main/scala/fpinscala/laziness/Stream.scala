package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) =>
        f(
          h(),
          t().foldRight(z)(f)
        ) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty      => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  // The natural recursive solution
  def toListRecursive: List[A] = this match {
    case Cons(h, t) => h() :: t().toListRecursive
    case _          => List()
  }

  /*
  The above solution will stack overflow for large streams, since it's
  not tail-recursive. Here is a tail-recursive implementation. At each
  step we cons onto the front of the `acc` list, which will result in the
  reverse of the stream. Then at the end we reverse the result to get the
  correct order again.
   */
  def toListTailRecursive: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h, t) => go(t(), h() :: acc)
      case _          => acc
    }
    go(this, List()).reverse
  }

  def toList: List[A] = this match {
    case Empty      => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def take(n: Int): Stream[A] = this match {
    case _ if n <= 0 => Empty
    case Cons(h, t)  => cons(h(), t().take(n - 1))
  }

  def takeF(n: Int): Stream[A] = unfold(this) {
    case Cons(h, t) if n > 0 => Some((h(), t().takeF(n - 1)))
    case _                   => None
  }

  def takeViaUnfold(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(h, t), 1)          => Some((h(), (empty, 0)))
      case (Cons(h, t), n) if n > 1 => Some((h(), (t(), n - 1)))
      case _                        => None
    }

  // def mapF[B](f: A => B): Stream[B] =
  // unfold(this) {
  //   case Cons(h, t) => Some((f(h()), t()))
  //   case _          => None
  // }

  /*
    Create a new Stream[A] from taking the n first elements from this. We can achieve that by recursively
    calling take on the invoked tail of a cons cell. We make sure that the tail is not invoked unless
    we need to, by handling the special case where n == 1 separately. If n == 0, we can avoid looking
    at the stream at all.
   */
  def takeAnswer(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1  => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _                    => empty
  }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if (n > 0) => t() drop (n - 1)
    case _                     => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t() takeWhile p)
    case _                    => Empty
  }

  def takeWhileF(p: A => Boolean): Stream[A] = unfold(this) {
    case Cons(h, t) if p(h()) => Some((h(), t()))
    case _                    => None
  }

  def takeWhileViaUnfold(f: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if f(h()) => Some((h(), t()))
      case _                    => None
    }

  def takeWhileFold(p: A => Boolean): Stream[A] =
    (this foldRight (empty[A]))((x, xs) => if (p(x)) cons(x, xs) else xs)

  // def forAll(f: A => Boolean): Boolean =
  //   foldRight(true)((a, b) => f(a) && b)

  def forAll(p: A => Boolean): Boolean =
    !(this foldRight (false))((a, b) => !p(a) || b)

  def headOption: Option[A] =
    (this foldRight (None: Option[A]))((h, t) => Some(h))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] =
    (this foldRight (empty[B]))((x, xs) => cons(f(x), xs))

  def mapF[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case _          => None
    }

  def mapAnswer[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h, t) => cons(f(h), t))

  def filter(f: A => Boolean): Stream[A] =
    (this foldRight (empty[A])) { (x, xs) => if (f(x)) cons(x, xs) else xs }

  def filterAnswer(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) =>
      if (f(h)) cons(h, t)
      else t
    )

  def append[B >: A](s: => Stream[B]): Stream[B] =
    (this foldRight (s))((h, t) => cons(h, t))

  def appendAnswer[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h, t) => cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    (this foldRight (empty[B]))((x, xs) => xs append f(x))

  def flatMapAnswer[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h, t) => f(h) append t)

  def zipWith[B, C](s: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, s)) {
      case ((Cons(ha, ta), Cons(hb, tb))) =>
        Some((f(ha(), hb()), (ta(), tb())))
      case _ => None
    }

  def zipWithAnser[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) =>
        Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }

  @annotation.tailrec
  final def startsWith[B](s: Stream[B]): Boolean = s match {
    case Cons(h1, t1) => {
      this match {
        case Cons(h2, t2) if (h1() == h2()) =>
          t2().drop(1) startsWith (t1().drop(1))
        case _ => false
      }
    }
    case Empty => true
  }

  def tails: Stream[Stream[A]] =
    unfold(this) {
      case Empty => None
      case s     => Some((s, s drop 1))
    } append Stream(empty)
  /*
  `s startsWith s2` when corresponding elements of `s` and `s2` are all equal, until the point that `s2` is exhausted. If `s` is exhausted first, or we find an element that doesn't match, we terminate early. Using non-strictness, we can compose these three separate logical steps--the zipping, the termination when the second stream is exhausted, and the termination if a nonmatching element is found or the first stream is exhausted.
   */
  // def startsWithAnswer[A](s: Stream[A]): Boolean =
  //   zipAll(s).takeWhile(!_._2.isEmpty) forAll {
  //     case (h, h2) => h == h2
  //   }

}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def constant[A](a: A): Stream[A] = {
    lazy val tail = cons(a, constant(a))
    cons(a, tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def from(n: Int): Stream[Int] = {
    lazy val tail = cons(n, from(n + 1))
    tail
  }

  val fibs: Stream[Int] = {
    def calc(prev: Int, curr: Int): Stream[Int] = {
      lazy val tail = cons(prev + curr, calc(curr, prev + curr))
      tail
    }
    cons(0, cons(1, calc(0, 1)))
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((h, s)) => cons(h, unfold(s)(f))
      case None         => empty
    }

  def constantFold[A](a: A): Stream[A] = unfold(a)(a => Some((a, a)))

  def fromFold(n: Int): Stream[Int] = unfold(n)(a => Some((a, a + 1)))

  /*
  Scala provides shorter syntax when the first action of a function literal is to match on an expression.  The function passed to `unfold` in `fibsViaUnfold` is equivalent to `p => p match { case (f0,f1) => ... }`, but we avoid having to choose a name for `p`, only to pattern match on it.
   */
  val fibsViaUnfold =
    unfold((0, 1)) { case (f0, f1) => Some((f0, (f1, f0 + f1))) }

  val fibsFold: Stream[Int] =
    unfold((0, 1))(a => a match { case (x, y) => Some((x, (y, x + y))) })

}
