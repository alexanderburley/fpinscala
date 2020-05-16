package state
import org.scalatest.FlatSpec
import fpinscala.statemachine._

class StateMachineSpec extends FlatSpec {
  "simulateMachine" should "take a coin and unlock with candy left" in {
    val mach = Machine(true, 2, 2)
    val ((x, y), m) = Candy.simulateMachine(List(Coin)).run(mach)
    assert(m == Machine(false, 2, 3))
  }

  it should "take a coin and not unlock with no candy left" in {
    val mach = Machine(true, 0, 2)
    val ((x, y), m) = Candy.simulateMachine(List(Coin)).run(mach)
    assert(m == Machine(true, 0, 2))
  }

  it should "turn when unlocked should dispense a candy and lock" in {
    val mach = Machine(false, 2, 2)
    val ((x, y), m) = Candy.simulateMachine(List(Turn)).run(mach)
    assert(m == Machine(true, 1, 2))
  }

  it should "turn when locked should do nothing" in {
    val mach = Machine(true, 2, 2)
    val ((x, y), m) = Candy.simulateMachine(List(Turn)).run(mach)
    assert(m == Machine(true, 2, 2))
  }
  it should "coin when unlocked should do nothing" in {
    val mach = Machine(false, 2, 2)
    val ((x, y), m) = Candy.simulateMachine(List(Coin)).run(mach)
    assert(m == Machine(false, 2, 2))
  }
}
