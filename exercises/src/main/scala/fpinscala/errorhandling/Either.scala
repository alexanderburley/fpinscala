package fpinscala.errorhandling

import scala.{
  Option => _,
  Either => _,
  Left => _,
  Right => _,
  _
} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(x) => Right(f(x))
    case Left(y)  => Left(y)
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(x) => f(x)
    case Left(y)  => Left(y)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Right(x) => Right(x)
    case Left(y)  => b
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    this flatMap (a => b map (bb => f(a, bb)))
  //   for { a <- this; b1 <- b } yield f(a, b1)
}
case class Left[+E](get: E) extends Either[E, Nothing]
case class Right[+A](get: A) extends Either[Nothing, A]

object Either {
  // def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
  // a.foldRight[Option[List[B]]](Some(Nil))((h, t) => map2(f(h), t)(_ :: _))
  def traverse[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    es.foldRight[Either[E, List[B]]](Right(Nil))((h, t) => f(h).map2(t)(_ :: _))

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverse(es)(a => a map (a => a))

  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

}
