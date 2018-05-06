package introFP.state

import State._

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy {
  def update(i: Input)(s: Machine): Machine = ???

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???

}

object Test {

  import Candy._

  def main(args: Array[String]): Unit = {
    val inputCoin = List(Coin)
    val inputTurn = List(Turn)

    // Inserting a coin into a locked machine will cause it to unlock if there’s any candy left.
    val machine1 = Machine(true, 1, 0)
    assert(!simulateMachine(inputCoin).run(machine1)._2.locked)

    // Turning the knob on an unlocked machine will cause it to dispense candy and become locked.
    val machine2 = Machine(false, 1, 1)
    val m2Result = simulateMachine(inputTurn).run(machine2)
    assert(m2Result._2.locked)
    assert(m2Result._2.candies == 0)

    // Turning the knob on a locked machine or inserting a coin into an unlocked machine does nothing.
    assert(simulateMachine(inputTurn).run(machine1)._2.locked == machine1.locked)
    assert(simulateMachine(inputCoin).run(machine2)._2.locked == machine2.locked)

    // A machine that’s out of candy ignores all inputs.
    val machine3 = Machine(true, 0, 1)
    assert(simulateMachine(inputTurn).run(machine3)._2.locked == machine3.locked)
    assert(simulateMachine(inputCoin).run(machine3)._2.locked == machine3.locked)
  }
}