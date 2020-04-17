import fpinscala.gettingstarted.PolymorphicFunctions._
import org.scalatest.FlatSpec

class GettingStartedSpec extends FlatSpec {

  "isSorted" should "return true for a sorted array of integers" in {
    val arr = Array(1, 2, 3)
    def gtInt(x: Int, y: Int) = x < y
    assert(isSorted(arr, gtInt), true)
  }

  "isSorted" should "return true for a sorted array of integers" in {
    val arr = Array(1, 2, 3)
    def gtInt(x: Int, y: Int) = x < y
    assert(isSorted(arr, gtInt), true)
  }

}
