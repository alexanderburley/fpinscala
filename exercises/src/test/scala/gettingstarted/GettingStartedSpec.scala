import fpinscala.gettingstarted.PolymorphicFunctions._
import org.scalatest.FlatSpec

class GettingStartedSpec extends FlatSpec {

  def gtBool(x: Boolean, y: Boolean) = if (x || (!x && !y)) true else false
  def gtInt(x: Int, y: Int) = x > y

  "isSorted" should "return true for a sorted array of integers" in {
    assert(isSorted(Array(1, 2, 3), gtInt))
  }

  it should "return false for an unsorted array of integers" in {
    assert(!isSorted(Array(3, 2, 1), gtInt))
  }

  it should "return true for an empty array" in {
    assert(isSorted(Array(), gtInt))
  }

  it should "return true for a sorted boolean array" in {
    assert(isSorted(Array(false, false, true, true), gtBool))
  }

  it should "return false for an unsorted boolean array" in {
    assert(!isSorted(Array(true, true, false, false), gtBool))
  }

  val add = (a: Int, b: Int) => a + b;

  "curry" should "partially apply a value to a function" in {
    assert(curry(add)(5)(2) === 7)
  }

  "uncurry" should "reverse partial application" in {
    assert(uncurry(curry(add))(5, 2) === 7)
  }

  "compose" should "compose two functions" in {
    val add2 = curry(add)(2)
    val add10 = curry(add)(10)
    assert(compose(add2, add10)(2) === 14)
  }
}
