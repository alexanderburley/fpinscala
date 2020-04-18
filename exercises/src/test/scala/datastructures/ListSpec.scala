import org.scalatest.FlatSpec
import fpinscala.datastructures.List._
import fpinscala.datastructures._

class ListSpec extends FlatSpec {

  "Exercise 3.1" should "return 3 for given expression" in {
    assert(x === 3)
  }

  "tail" should "remove the first element of a given list" in {
    assert(
      tail(List(1, 2, 3)).toString === Cons(2, Cons(3, Nil)).toString
    )
  }

  it should "return Nil for tail of empty list" in {
    assert(tail(List()) == Nil)
  }

}
