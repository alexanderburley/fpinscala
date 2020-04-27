package laziness
import org.scalatest.FlatSpec
import fpinscala.laziness.Stream
import fpinscala.laziness.Stream._
import fpinscala.laziness._

class StreamSpec extends FlatSpec {

  val s = cons(1, cons(2, cons(3, Empty)))

  "toList" should "convert a stream into a list" in {
    assert(s.toList == List(1, 2, 3))
  }

  it should "convert empty list" in {
    assert(Empty.toList == List())
  }

  "take" should "take the first x elements of a list" in {
    assert(s.take(2).toList == List(1, 2))
  }

  it should "take 0 elements " in {
    assert(s.take(0).toList == List())
  }

  "drop" should "drop the first x elements of a list" in {
    assert(s.drop(2).toList == List(3))
  }

  it should "drop empty list" in {
    assert(Empty.drop(1).toList == List())
  }

  it should "drop all elements" in {
    assert(s.drop(3).toList == List())
  }

  "takeWhile" should "returns all starting elements matching the predicate" in {
    assert(s.takeWhile(x => x < 3).toList == List(1, 2))
  }

  it should "takeFromEmpty" in {
    assert(Empty.takeWhile { x: Int => x < 3 }.toList == List())
  }

  "forAll" should "check that for all items, they match a predicate" in {
    assert(s forAll { x => x > 0 })
  }

  it should "check be true for empty" in {
    assert(Empty forAll { x: Int => x > 0 })
  }

  it should "be false for some items in list" in {
    val l = cons(1, cons(2, cons(0, Empty)))
    assert((!(l forAll { x: Int => x > 0 })))
  }

  "takeWhileFold" should "returns all starting elements matching the predicate" in {
    assert(s.takeWhileFold(x => x < 3).toList == List(1, 2))
  }

  it should "take from empty" in {
    assert(Empty.takeWhile { x: Int => x < 3 }.toList == List())
  }
}
