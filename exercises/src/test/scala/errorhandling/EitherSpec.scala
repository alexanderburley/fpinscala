import org.scalatest.FlatSpec
import scala.{
  Option => _,
  Either => _,
  Left => _,
  Right => _,
  _
} // hide std library `Option` and `Either`, since we are writing our own in this chapter
import fpinscala.errorhandling._
import fpinscala.errorhandling.Either._

class EitherSpec extends FlatSpec {

  "map" should "map the right" in {
    assert((Right(5) map { x: Int =>
      x + 1
    }) == Right(6))
  }

  it should "map the left" in {
    assert((Left("0") map { x: Int =>
      x + 1
    }) == Left("0"))
  }

  "flatMap" should "map the right" in {
    assert((Right(5) flatMap { x: Int =>
      Right(x + 1)
    }) == Right(6))
  }

  it should "map the left" in {
    assert((Left("0") flatMap { x: Int =>
      Right(x + 1)
    }) == Left("0"))
  }

  it should "take functions that can fail" in {
    assert((Right(1) flatMap { x: Int =>
      if (x > 1) Right(x) else Left("Bad")
    }) == Left("Bad"))
  }

  it should "take functions that will suceed" in {
    assert((Right(2) flatMap { x: Int =>
      if (x > 1) Right(x) else Left("Bad")
    }) == Right(2))
  }

  "orElse" should "(return value)" in {
    assert((Right(2) orElse Right(1)) == Right(2))
  }

  it should "return fallback" in {
    val oe = Left("123") orElse Right(1)
    assert(oe == Right(1))
  }

  "map2" should "map 2 values" in {
    assert((Right(1).map2(Right(2))((_ + _))) == Right(3))
  }
}
