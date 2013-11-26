package ScalaHDL.Core

class HDLException(stmt: String) extends Exception(stmt)

class NoSuchModuleException(modName: Symbol)
    extends HDLException(List("module", modName.name, "not found!").mkString(" "))

class WrongNumberOfArgumentsException(
  modName: Symbol, expected: Int, defacto: Int)
    extends HDLException(List(
      "wrong number of arguments for module ", modName.name,
      ", expecting ", expected.toString,
      ", get ", defacto.toString, ".").mkString(""))

class NotEnoughBitsException(
  sigName: String, value: Int, expected: Int, defacto: Int)
    extends HDLException(
  "not enough bits to hold value %d in %s, require %d, get %d".format(
    value, sigName, expected, defactor))
