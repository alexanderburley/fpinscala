package state
import org.scalatest.FlatSpec
import fpinscala.state._
import fpinscala.state.RNG._

class StateSpec extends FlatSpec {
  "nonNegativeInt" should "generate a number between 0 and Int.MaxValue" in {
    val (n, rng) = nonNegativeInt(RNG.Simple(16159453)) // Returns -128147967 usually
    assert(n == 1780876629)
  }

  it should "generate number fine" in {
    val (n, rng) = nonNegativeInt(RNG.Simple(1))
    assert(n == 384748)
  }

  "double" should "generate a random double between 0 and 1" in {
    val (n, rng) = double(RNG.Simple(16159453))
    assert(n >= 0 && n < 1)
  }
}
