package ScalaHDL.DataType

abstract sealed class HDLDataType

case class Signal(value: Int, bit: Int) extends HDLDataType
