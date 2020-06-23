package fpinscala.testing

class PropFalseStub extends Prop {
  override def check: Boolean = false
}
class PropTrueStub extends Prop {
  override def check: Boolean = true
}
class PropTest extends org.scalatest.FlatSpec {
  "&&" should "return true for two true stubs" in {
    val res: Boolean = new PropTrueStub() && new PropTrueStub()
    assert(res)
  }
  "&&" should "return false for two false stubs" in {
    val res: Boolean = new PropFalseStub() && new PropFalseStub()
    assert(res === false)
  }
  "&&" should "return false for 1 true and 1 false stub" in {
    val res: Boolean = new PropTrueStub() && new PropFalseStub()
    assert(res === false)
  }
}

/*

Exercise 8.1

sum: List[Int] => Int - Properties

- Reversing a list and summing it should give the same result as an original list
- If all list elements are same value, sum should equal length element * N
- Sum of an empty list should be 0

Answer

• The sum of the empty list is 0.
• The sum of a list whose elements are all equal to x is just the list’s length multiplied by x. We
might express this as sum(List.fill(n)(x)) == n*x
• For any list, l, sum(l) == sum(l.reverse), since addition is commutative.
• Given a list, List(x,y,z,p,q), sum(List(x,y,z,p,q)) == sum(List(x,y)) + sum(List(z,p,q)),
since addition is associative. More generally, we can partition a list into two subsequences
whose sum is equal to the sum of the overall list.
• The sum of 1,2,3…n is n*(n+1)/2.

Exercise 8.2

max: List[Int] => Int - Properties

- Reversing a list should have no consequence
- If all list elements are the same, sum should be value N
- Max of an empty list should throw an error

Answer

• The max of a single element list is equal to that element.
• The max of a list is greater than or equal to all elements of the list.
• The max of a list is an element of that list.
• The max of the empty list is unspecified and should throw an error or return None.

 */
