package parallelism

import fpinscala.parallelism.Par._
import java.util.concurrent.{Executors, ExecutorService}
import org.scalatest.FlatSpec

class ParSpec extends FlatSpec {
  val pool: ExecutorService = Executors.newFixedThreadPool(3)

  "asyncF" should "Convert any function a => b to an asynchronous function" in {
    val asyncFunction = asyncF((a: Int) => List(a))
    assert(asyncFunction(1)(pool).get === List(1))
  }

  "sequence" should "Turn a list of parallel units into a parallel unit containing a list of values" in {
    val values = List(unit(1), unit(2), unit(3))
    assert(sequence(values)(pool).get === List(1, 2, 3))
  }

  "parFilter" should "Filter elements of a list in parallel" in {
    val filtered = parFilter(List(1, 2, 3)) { a => a > 1 }
    assert(filtered(pool).get === List(2, 3))
  }

  "parSum" should "Sum the items" in {
    val summed = parSum(IndexedSeq(1, 2, 3))(0, _ + _)
    assert(summed(pool).get === 6)
  }

  it should "Get the largest element in the list" in {
    val max = parSum(IndexedSeq(1, 10, 3))(0, (a, b) => if (a > b) a else b)
    assert(max(pool).get === 10)
  }

  "choiceN" should "Should run n and use it to determine what to run" in {
    val cond = unit(1)
    val res = choiceN(cond)(List(cond, unit(2)))
    assert(res(pool).get === 2)
  }

  "choiceWithChoiceN" should "Should run for true using choiceN" in {
    val cond = unit(true)
    val res = choiceWithChoiceN(cond)(unit(2), unit(3))
    assert(res(pool).get === 2)
  }

  "choiceWithChoiceN" should "Should run for false using choiceN" in {
    val cond = unit(false)
    val res = choiceWithChoiceN(cond)(unit(2), unit(3))
    assert(res(pool).get === 3)
  }

}
