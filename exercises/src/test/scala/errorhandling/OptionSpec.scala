import org.scalatest.FlatSpec
import scala.{None => _, Option => _, Some => _, Either => _, _}
import fpinscala.errorhandling._
import fpinscala.errorhandling.Option._

class OptionSpec extends FlatSpec {

  def m = { x: Int => x + 1 }

  "option.map" should "Apply f if the option is not None" in {
    assert(Some(1).map(m) == Some(2))
  }

  it should "Apply f to none and get back none" in {
    assert((None map m) == None)
  }

  def fm(x: Int): Option[Int] = if (x > 1) Some(x + 1) else None

  "option.flatMap" should "Apply f, which may fail, to the option if not None" in {
    assert(Some(1).flatMap(fm) == None)
  }

  it should "Apply f which does nto fail to the option" in {
    assert(Some(2).flatMap(fm) == Some(3))
  }

  "option.getOrElse" should "Retrive the value if not None" in {
    assert(Some(1).getOrElse(2) == 1)
  }

  it should "Provide a value of subtype B if value is None" in {
    assert(None.getOrElse(2) == 2)
  }

  "option.orElse" should "not evaluate ob unless needed" in {
    assert(Some(1).orElse(Some(2)) == Some(1))
  }

  it should "evaluate ob when needed" in {
    assert(None.orElse(Some(1)) == Some(1))
  }

  "option.filter" should "Convert filter None to None" in {
    assert((None filter { x: Int =>
      x > 1
    }) == None)
  }

  it should "filter out no values" in {
    assert((Some(1) filter { x: Int =>
      x == 1
    }) == Some(1))
  }

  "variance" should "Caculate variance for a sequence" in {
    assert(variance(Seq(1, 2, 3, 4)) == Some(1.25))
  }

  val sum = (x: Int, y: Int) => x + y

  "map2" should "combine two Option values using a binary function" in {
    assert(map2(Some(1), Some(2))(sum) == Some(3))
  }

  it should "combine first empty value" in {
    assert(map2(None, Some(1))(sum) == None)
  }

  it should "combine second empty value" in {
    assert(map2(Some(1), None)(sum) == None)
  }

  it should "combine two empty values" in {
    assert(map2(None, None)(sum) == None)
  }

  "sequence" should "Combine list of options into one option" in {
    assert(sequence(List(Some(1), Some(2), Some(3))) == Some(List(1, 2, 3)))
  }

  "traverse" should "map it into an option of a list" in {
    assert(
      traverse(List(0, 1, 2))(a => if (a > 0) Some("1") else None) == None
    )
  }

  "sequenceViaTraverse" should " map it into an option of a list" in {
    assert(
      seqTrav(List(Some(0), None, Some(2))) == None
    )
  }

  // it should "return the same list" in {
  //   seqTrav(List(Some(0), Some(1), Some(2))) == List(Some(0), List(1), Some(2))
  // }
}
