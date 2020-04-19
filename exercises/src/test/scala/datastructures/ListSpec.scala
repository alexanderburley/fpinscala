import org.scalatest.FlatSpec
import fpinscala.datastructures.List._
import fpinscala.datastructures._

class ListSpec extends FlatSpec {

  def eval[A](x: List[A], y: List[A]) = x.toString == y.toString
  val gt = { i: Int => i > 1 }
  val * = (x: Int, y: Int) => x * y

  val l = List(1, 2, 3)
  val e = List()

  "Exercise 3.1" should "return 3 for given expression" in {
    assert(x === 3)
  }

  "tail" should "remove the first element of a given list" in {
    assert(eval(tail(l), List(2, 3)))
  }

  it should "return Nil for tail of empty list" in {
    assert(eval(tail(e), Nil))
  }

  "setHead" should "replace the first element of the list with a new element 4" in {
    assert(eval(setHead(l, 4), List(4, 2, 3)))
  }

  it should "replace the first element of the list with a new element Nil" in {
    // assert(setHead(List(1, 2, 3), Nil).toString === List(Nil, 2, 3).toString)
    assert(eval(setHead(l, Nil), List(Nil, 2, 3)))
  }

  it should "replace the first element of an empty with a new element 4" in {
    assert(eval(setHead(e, 4), List(4)))
  }

  "drop" should "drop 2 elements from a list given 2" in {
    assert(eval(drop(l, 2), List(3)))
  }

  it should "drop 0 elements given 0" in {
    assert(eval(drop(l, 0), l))
  }

  it should "drop 0 elements of empty list" in {
    assert(eval(drop(e, 0), e))
  }

  it should "drop 0 elements of empty list given 3" in {
    assert(eval(drop(e, 3), e))
  }

  "dropWhile" should "drop 0 elements l" in {
    assert(eval(dropWhile(l, gt), l))
  }

  it should "drop 0 elements for none gt" in {
    assert(eval(dropWhile(List(1, 1, 1), gt), List(1, 1, 1)))
  }

  it should "drop all elements for all gt" in {
    assert(eval(dropWhile(List(2, 3, 4), gt), e))
  }

  it should "drop 0 elements for emptylist gt" in {
    assert(eval(dropWhile(e, gt), e))
  }

  "filter" should "drop 2 elements l" in {
    assert(eval(filter(l, gt), List(1)))
  }
  it should "drop 0 elements for none gt" in {
    assert(eval(filter(List(1, 1, 1), gt), List(1, 1, 1)))
  }

  it should "drop all elements for all gt" in {
    assert(eval(filter(List(2, 3, 4), gt), e))
  }

  it should "drop 0 elements for emptylist gt" in {
    assert(eval(filter(e, gt), e))
  }

  "init" should "return all but the last element of a list l" in {
    assert(eval(init(l), List(1, 2)))
  }

  it should "return Nil for empty list" in {
    assert(eval(init(e), e))
  }

  it should "return return empty for single element" in {
    assert(eval(init(List(1)), e))
  }

  "Exercise 3.7" should "We cannot halt the recursions as we always have to traverse to the end of the list" in {}

  // "Exercise 3.8" should "" in {
  //   println("foldRight:", foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _)))
  // }

  "length" should "Return 3 for length of l" in {
    assert(length(l) == 3)
  }

  it should "Return 1 for length of list of 1" in {
    assert(length(List(1)) == 1)
  }

  it should "Return 0 for length of e" in {
    assert(length(e) == 0)
  }

  "length3" should "Return 3 for length of l" in {
    assert(length3(l) == 3)
  }

  it should "Return 1 for length of list of 1" in {
    assert(length3(List(1)) == 1)
  }

  it should "Return 0 for length of e" in {
    assert(length3(e) == 0)
  }

  "foldLeft" should "reduce l into 6" in {
    assert(foldLeft(l, 1)(*) == 6)
  }
  it should "reduce empty list to initial" in {
    assert(foldLeft(e, 1)(*) == 1)
  }

  "sum3" should "sum list of (2,3,4) into 9" in {
    assert(sum3(List(2, 3, 4)) == 9)
  }

  "product3" should "product list of (2,3,4) into 24" in {
    assert(product3(List(2, 3, 4)) == 24)
  }

  "reverse" should "reverse l" in {
    assert(eval(reverse(l), List(3, 2, 1)))
  }

  it should "reverse empty list" in {
    assert(eval(reverse(e), e))
  }

  it should "reverse single" in {
    assert(eval(reverse(List(1)), List(1)))
  }

  "reverse3" should "reverse l" in {
    assert(eval(reverse3(l), List(3, 2, 1)))
  }

  it should "reverse empty list" in {
    assert(eval(reverse3(e), e))
  }

  it should "reverse single" in {
    assert(eval(reverse3(List(1)), List(1)))
  }

}
