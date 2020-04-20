import org.scalatest.FlatSpec
import fpinscala.datastructures.Tree._
import fpinscala.datastructures._

class TreeSpec extends FlatSpec {
  val t = Branch(Leaf(1), Leaf(2))
  val tLarge = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))

  "size" should "count the number of nodes in a small tree" in {
    assert(size(t) == 3)
  }

  it should "count the number of nodes in a large tree" in {
    assert(size(tLarge) == 7)
  }

  it should "count a singe leaf" in {
    assert(size(Leaf(1)) == 1)
  }

  "maximum" should "return 2 for t" in {
    assert(maximum(t) == 2)
  }

  it should "return 6 for t7" in {
    assert(maximum(tLarge) == 4)
  }

  it should "return 1 for 1 leaf" in {
    assert(maximum(Leaf(1)) == 1)
  }
}
