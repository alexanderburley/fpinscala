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
}
