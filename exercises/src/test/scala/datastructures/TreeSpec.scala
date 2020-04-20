import org.scalatest.FlatSpec
import fpinscala.datastructures.Tree._
import fpinscala.datastructures._

class TreeSpec extends FlatSpec {
  val l = Leaf(1)
  val t = Branch(l, Leaf(2))
  val tLarge = Branch(Branch(l, Leaf(2)), Branch(Leaf(3), Leaf(4)))

  "size" should "count the number of nodes in a small tree" in {
    assert(size(t) == 3)
  }

  it should "count the number of nodes in a large tree" in {
    assert(size(tLarge) == 7)
  }

  it should "count a singe leaf" in {
    assert(size(l) == 1)
  }

  "maximum" should "return 2 for t" in {
    assert(maximum(t) == 2)
  }

  it should "return 6 for t7" in {
    assert(maximum(tLarge) == 4)
  }

  it should "return 1 for 1 leaf" in {
    assert(maximum(l) == 1)
  }

  "depth" should "return 1 for depth 1" in {
    assert(depth(l) == 1)
  }

  it should "return 2 for t" in {
    assert(depth(t) == 2)
  }

  it should "return 3 for t" in {
    assert(depth(tLarge) == 3)
  }

  "map" should "modifies a leaf" in {
    assert(map(l){n: Int => n + 1}.toString == Leaf(2).toString)
  }

  it should "modify a tree" in {
    assert(map(t){n: Int => n + 1}.toString == Branch(Leaf(2), Leaf(3)).toString)
  }

    it should "modify a tree of ints to string" in {
    assert(map(t){n: Int => n.toString}.toString == Branch(Leaf("1"), Leaf("2")).toString)
  }
}
