package ScalaHDL

//import scala.reflect.runtime.universe._
import scala.collection.mutable.HashMap

object HDLType_ extends Enumeration { 
  type HDLType_ = Value
  val int = Value
}
import HDLType_._

abstract class HDLPart { 
  def convert(): String
}

case class HDLStatement(stmt: String) extends HDLPart { 
  override def convert = 
    "res <= a + b;\n"
}

case class HDLBlock(stmts: List[HDLStatement]) extends HDLPart { 
  override def convert = 
    "begin\n" + stmts.map(_.convert).mkString("") + "end\n"
}

case class HDLModule(ids: List[String], block: HDLBlock) extends HDLPart { 
  override def convert = "(" + ids.mkString(", ") + ");\n" + block.convert()
}

case class HDLAssignment(id: String, tpe: HDLType)

case class HDLType(tpe: HDLType_, size: Int)

case class HDLConvert(mod: String, params: List[String])

class HDLMainModule(as: List[HDLAssignment], c: HDLConvert) { 
  val types = new HashMap[String, HDLType]
  for (a <- as) types += Tuple2(a.id,  a.tpe)
}

class HDLProgram(main: HDLMainModule) extends HDLPart { 
  val modules = new HashMap[String, HDLModule]()
  override def convert =
    (for ((n, m) <- modules) yield ("module " + n + m.convert())).mkString("\n")
}
