package ScalaHDLExample.FSM.Moore

import ScalaHDL.Core.ScalaHDL
import ScalaHDL.Core.HDLType
import ScalaHDL.Core.DataType._
import ScalaHDL.Core.DataType.Signals._
import ScalaHDL.Simulation.Simulator

object StatesTrafficLights extends Enumeration {
  type StatesTrafficLights = Value
  val cars_go, cars_stop, people_go, people_stop = Value
}

trait Moore extends ScalaHDL {

  val timerStep = 2
  val timerStep1 = timerStep + 1

  import StatesTrafficLights._

  defMod.moore('clk, 'rst,
    'cars_green, 'cars_yellow, 'cars_red,
    'ppl_green, 'ppl_yellow, 'ppl_red) {


    val state_traffic = toHDLType(unsigned(people_stop.id))
    val timer = toHDLType(unsigned(1, timerStep1 + 1))

    sync(1).moore {
      when ('rst is 1) {
        timer := 1
        state_traffic := cars_stop.id
      } .otherwise {
        when (state_traffic is cars_stop.id) {
          timer := 1
          state_traffic := people_go.id
        } .elsewhen (state_traffic is people_go.id) {
          timer := (timer << 1) % math.pow(2, timerStep1).toInt
          when (timer(timerStep) is 1) {
            state_traffic := people_stop.id
          }
        } .elsewhen (state_traffic is people_stop.id) {
          timer := 1
          state_traffic := cars_go.id
        } .elsewhen (state_traffic is cars_go.id) {
          timer := (timer << 1) % math.pow(2, timerStep1).toInt
          when (timer(timerStep) is 1) {
            state_traffic := cars_stop.id
          }
        } .otherwise {
          state_traffic := people_stop.id
        }
      }
    }

    async {
      when (state_traffic is cars_go.id) {
        'cars_green := 1
        'cars_yellow := 0
        'cars_red := 0
        'ppl_green := 0
        'ppl_yellow := 0
        'ppl_red := 1
      } .elsewhen (state_traffic is cars_stop.id) {
        'cars_green := 0
        'cars_yellow := 1
        'cars_red := 0
        'ppl_green := 0
        'ppl_yellow := 0
        'ppl_red := 1
      } .elsewhen (state_traffic is people_go.id) {
        'cars_green := 0
        'cars_yellow := 0
        'cars_red := 1
        'ppl_green := 1
        'ppl_yellow := 0
        'ppl_red := 0
      } .elsewhen (state_traffic is people_stop.id) {
        'cars_green := 0
        'cars_yellow := 0
        'cars_red := 1
        'ppl_green := 0
        'ppl_yellow := 1
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

object Main extends Moore {
  def main(args: Array[String]) {
    val clk = bool(0)
    val rst = bool(0)
    val cars_green = bool(0)
    val cars_yellow = bool(0)
    val cars_red = bool(0)
    val ppl_green = bool(0)
    val ppl_yellow = bool(0)
    val ppl_red = bool(0)

    println(convert('moore, clk, rst,
      cars_green, cars_yellow, cars_red,
      ppl_green, ppl_yellow, ppl_red))
  }
}
