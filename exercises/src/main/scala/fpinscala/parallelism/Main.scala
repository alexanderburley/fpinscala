package fpinscala.parallelism

import java.util.concurrent.Executors
import fpinscala.parallelism.Par

class Main {}
object Main extends App {
  val a = Par.lazyUnit(42 + 1)
  val S = Executors.newFixedThreadPool(1)
  println(Par.equal(S)(a, Par.fork(a)))
}
