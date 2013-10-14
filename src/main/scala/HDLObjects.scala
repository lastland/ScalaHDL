package ScalaHDL

//import scala.reflect.runtime.universe._
import scala.collection.mutable.HashMap

object HDLType_ extends Enumeration { 
  type HDLType_ = Value
  val int = Value
}
import HDLType_._

case class HDLStatement(stmt: String)

case class HDLBlock(stmts: List[HDLStatement])

case class HDLModule(ids: List[String], block: HDLBlock)

case class HDLAssignment(id: String, tpe: HDLType)

case class HDLType(tpe: HDLType_, size: Int)

case class HDLConvert(mod: String, params: List[String])

class HDLMainModule(as: List[HDLAssignment], c: HDLConvert) { 
  val types = new HashMap[String, HDLType]
  for (a <- as) types += Tuple2(a.id,  a.tpe)
}

class HDLProgram(main: HDLMainModule) { 
  val modules = new HashMap[String, HDLModule]
}
