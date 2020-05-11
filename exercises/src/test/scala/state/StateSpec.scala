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

  "intDouble" should "generate a random int and double tuple" in {
    val ((i, d), rng) = intDouble(RNG.Simple(1))
    assert(i == 1151252339)
    val doubleRange = (384748.toDouble / Int.MaxValue.toDouble)

    assert((d > (doubleRange - 0.0001)) && (d < (doubleRange + 0.0001)))
  }
  "doubleInt" should "generate a random int and double tuple" in {
    val ((d, i), rng) = doubleInt(RNG.Simple(1))
    assert(i == 1151252339)
    val doubleRange = (384748.toDouble / Int.MaxValue.toDouble)

    assert((d > (doubleRange - 0.0001)) && (d < (doubleRange + 0.0001)))
  }
  "double3" should "generate a random double triple" in {
    val ((d1, d2, d3), rng) = double3(RNG.Simple(1))
    val double1Range = (384748.toDouble / Int.MaxValue.toDouble)
    val double2Range = (1151252339.toDouble / Int.MaxValue.toDouble)
    assert((d1 > (double1Range - 0.0001)) && (d1 < (double1Range + 0.0001)))
    assert((d2 > (double2Range - 0.0001)) && (d1 < (double2Range + 0.0001)))
  }

  "ints" should "generate a list of random numbers" in {
    val (l, r) = ints(3)(RNG.Simple(1))
    assert(l == List(549383847, 1151252339, 384748))
  }

  "map2" should "take two actions and a function to combine results" in {
    val x = unit(1)
    val y = unit(2)

    val (v, r2) =
      map2(x, y)((a, b) => a + b)(
        RNG.Simple(1)
      )

    assert(v == 3)
    assert(r2 == RNG.Simple(1))
  }

  "sequence" should "combine a list of transition into a single transition" in {
    val x = unit(1)
    val y = unit(2)
    val z = unit(3)

    val (v, r) = sequence(List(x, y, z))(RNG.Simple(1))
    assert(v == List(1, 2, 3))
    assert(r === RNG.Simple(1))
  }

}
