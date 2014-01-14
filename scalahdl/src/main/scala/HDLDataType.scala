package ScalaHDL.Core.DataType {
  object SignalType extends Enumeration {
    type SignalType = Value
    val reg, wire = Value
  }
  import SignalType._

  object SignalDirection extends Enumeration {
    type SignalDirection = Value
    val input, output, middle = Value
  }
  import SignalDirection._

  case class ArgInfo(name: String, tpe: SignalType, dir: SignalDirection, size: Int) {
    def declaration: String = {
      val bits = if (size > 1) "[%d:%d] ".format(size - 1, 0) else ""
      dir match {
        case `input` => "input " + bits + name + ";\n"
        case `output` => "output " + bits + name + ";\n" +
          (tpe match {
            case `reg` => "reg " + bits + name + ";\n"
            case `wire` => "wire " + bits + name + ";\n"
          })
        case `middle` =>
          (tpe match {
            case `reg` => "reg " + bits + name + ";\n"
            case `wire` => "wire " + bits + name + ";\n"
          })
      }
    }
  }
}
