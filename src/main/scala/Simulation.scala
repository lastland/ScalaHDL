package ScalaHDL.Simulation

import ScalaHDL.ScalaHDL
import ScalaHDL.Helpers.CoroutineHelper._

class Waitor {
}

class Simulator(hdl: ScalaHDL){
  private val signals = List()
  private val waiters = List()

  import ScalaHDL.Module._

  def simulate(mods: module*) {
    println("simulate!")
  }
}
