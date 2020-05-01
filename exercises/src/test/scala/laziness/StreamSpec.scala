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

  "takeF" should "take the first x elements of a list" in {
    assert(s.takeF(2).toList == List(1, 2))
  }

  it should "take 0 elements " in {
    assert(s.takeF(0).toList == List())
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

  "takeWhileF" should "returns all starting elements matching the predicate" in {
    assert(s.takeWhileF(x => x < 3).toList == List(1, 2))
  }

  it should "takeFromEmpty" in {
    assert(Empty.takeWhileF { x: Int => x < 3 }.toList == List())
  }

  "takeWhileViaUnfold" should "returns all starting elements matching the predicate" in {
    assert(s.takeWhileViaUnfold(x => x < 3).toList == List(1, 2))
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

  "headOption1" should "Optionally take the head" in {
    assert(s.headOption == Some(1))
  }

  it should "take empty" in {
    assert(Empty.headOption == None)
  }

  "map" should "map the sequence" in {
    assert((s map { x: Int => x + 1 }).toList === List(2, 3, 4))
  }

  it should "map empty list" in {
    assert((Empty.map(e => e)).toList == List())
  }

  "mapF" should "map the sequence" in {
    assert((s mapF { x: Int => x + 1 }).toList === List(2, 3, 4))
  }

  it should "map empty list" in {
    assert((Empty.mapF(e => e)).toList == List())
  }

  "filter" should "filter second element" in {
    assert((s filter { x => x != 2 }).toList == List(1, 3))
  }

  "append" should "append element" in {
    assert(s.append(cons(4, Empty)).toList == List(1, 2, 3, 4))
  }

  "flatMap" should "flatmap the element" in {
    assert((s.flatMap { x => s }).toList == s.toList ::: s.toList ::: s.toList)
  }

  "constant" should "return an infinite stream" in {
    assert(constant(1).take(5).toList == List(1, 1, 1, 1, 1))
  }

  "constantFold" should "return an infinite stream" in {
    assert(constantFold(1).take(5).toList == List(1, 1, 1, 1, 1))
  }

  "from" should "return an infinite stream of integers from n" in {
    assert(from(1).take(5).toList == List(1, 2, 3, 4, 5))
  }

  "fromFold" should "return an infinite stream of integers from n" in {
    assert(fromFold(1).take(5).toList == List(1, 2, 3, 4, 5))
  }

  "fibs" should "generate the fibonacci numbers" in {
    assert(fibs.take(6).toList == List(0, 1, 1, 2, 3, 5))
  }

  "fibsFold" should "generate the fibonacci numbers" in {
    assert(fibsFold.take(6).toList == List(0, 1, 1, 2, 3, 5))
  }

  "unfold" should "build a stream from a function producting the next state" in {
    assert(
      unfold(1)(_ => Some((1, 1))).take(5).toList == List(1, 1, 1, 1, 1)
    )
  }

  "zipWith" should "add values of one list onto the other" in {
    assert((s zipWith s)(_ + _).toList == List(2, 4, 6))
  }

  "startsWith" should "check s starts with 1.2.3" in {
    assert(s startsWith s)
  }

  "startsWith" should "check s starts with empty" in {
    assert(s startsWith Empty)
  }

}
