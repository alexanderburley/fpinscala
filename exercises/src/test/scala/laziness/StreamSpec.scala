package laziness
import org.scalatest.FlatSpec
import fpinscala.laziness.Stream
import fpinscala.laziness.Stream._
import fpinscala.laziness._

class StreamSpec extends FlatSpec {
  "toList" should "convert a stream into a list" in {
    val s = cons(1, cons(2, Empty))
    assert(s.toList.toString == List(1, 2).toString)
  }
}
