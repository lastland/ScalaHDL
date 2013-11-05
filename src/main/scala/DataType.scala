package ScalaHDL.DataType {

  abstract sealed class HDLDataType

  class Signal(val value: Int, val bits: Int) extends HDLDataType
}
