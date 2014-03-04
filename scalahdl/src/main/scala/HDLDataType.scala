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

  case class ArgInfo(name: String, tpe: SignalType, dir: SignalDirection,
    signed: Boolean, size: Int, lstSize: Int = 1) {
    def declaration: String = {
      val sign = if (signed) "signed " else ""
      val bits = if (size > 1) "[%d:0] ".format(size - 1) else ""
      val arrLst = if (lstSize > 1) " [0:%d]".format(lstSize - 1) else ""
      dir match {
        case `input` => "input " + sign + bits + name + arrLst + ";\n"
        case `output` => "output " + sign + bits + name + arrLst + ";\n" +
          (tpe match {
            case `reg` => "reg " + sign + bits + name + arrLst + ";\n"
            case `wire` => "wire " + sign + bits + name + arrLst + ";\n"
          })
        case `middle` =>
          (tpe match {
            case `reg` => "reg " + sign + bits + name + arrLst + ";\n"
            case `wire` => "wire " + sign + bits + name + arrLst + ";\n"
          })
      }
    }
  }
}
