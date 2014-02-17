package ScalaHDLExample.FSM.Mealy

import ScalaHDL.Core.ScalaHDL
import ScalaHDL.Core.HDLType
import ScalaHDL.Core.DataType._
import ScalaHDL.Core.DataType.Signals._
import ScalaHDL.Simulation.Simulator

object StatesTrafficLights extends Enumeration {
  type StatesTrafficLights = Value
  val cars_go, people_go = Value
}

trait Mealy extends ScalaHDL {

  val timerStep = 2
  val timerStep1 = timerStep + 1

  import StatesTrafficLights._

  defMod.mealy('clk, 'rst,
    'car_waiting, 'ppl_waiting,
    'cars_green, 'cars_yellow, 'cars_red,
    'ppl_green, 'ppl_yellow, 'ppl_red) {

    val state_traffic = toHDLType(unsigned(cars_go.id))

    sync(1).mealy {
      when ('rst is 1) {
        state_traffic := cars_go.id
      } .otherwise {
        when (state_traffic is cars_go.id) {
          when ('ppl_waiting is 1) {
            state_traffic := people_go.id
          }
        } .elsewhen (state_traffic is people_go.id) {
          when ('car_waiting is 1) {
            state_traffic := cars_go.id
          }
        } .otherwise {
          state_traffic := cars_go.id
        }
      }
    }

    async {
      when (state_traffic is cars_go.id) {
        'cars_green := ! 'ppl_waiting
        'cars_yellow := 'ppl_waiting
        'cars_red := 0
        'ppl_green := 0
        'ppl_yellow := 0
        'ppl_red := 1
      } .elsewhen (state_traffic is people_go.id) {
        'cars_green := 0
        'cars_yellow := 0
        'cars_red := 1
        'ppl_green := ! 'car_waiting
        'ppl_yellow := 'car_waiting
        'ppl_red := 0
      } .otherwise {
        'cars_green := 0
        'cars_yellow := 0
        'cars_red := 1
        'ppl_green := 0
        'ppl_yellow := 0
        'ppl_red := 1
      }
    }
  }
}

object Main extends Mealy {
  def main(args: Array[String]) {
    val clk = bool(0)
    val rst = bool(0)
    val car_waiting = bool(0)
    val ppl_waiting = bool(0)
    val cars_green = bool(0)
    val cars_yellow = bool(0)
    val cars_red = bool(0)
    val ppl_green = bool(0)
    val ppl_yellow = bool(0)
    val ppl_red = bool(0)

    println(convert('mealy, clk, rst,
      car_waiting, ppl_waiting,
      cars_green, cars_yellow, cars_red,
      ppl_green, ppl_yellow, ppl_red))
  }
}
