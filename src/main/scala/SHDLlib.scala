package ScalaHDL

import scala.language.dynamics

class DSL { 

  object HDLOperation extends Enumeration { 
    type HDLOperation = Value
    val add, sub, mul, div = Value
  }
  import HDLOperation._

  abstract sealed class HDLObject
  abstract sealed class HDLFunc extends HDLObject
  case class HDLFunc2 (op: HDLOperation, a: HDLObject, b: HDLObject) extends HDLFunc { 
  }

  case class HDLAssignment(left: HDLIdent, right: HDLObject) extends HDLObject { 
  }

  case class HDLIdent(name: Symbol) extends HDLObject { 
    def +(other: HDLObject) = HDLFunc2(add, this, other)
    def :=(other: HDLObject) = HDLAssignment(this, other)
  }

  case class HDLModule(name: Symbol) { 
    def apply(f: => HDLObject) = f
  }

  object module extends Dynamic { 
    def applyDynamic(name: String)(args: Any*): HDLModule = { 
      HDLModule(Symbol(name))
    }
  }

  def convert(name: Symbol) { 
    println("convert " + name)
  }

  implicit def symbol2Ident(s: Symbol) = HDLIdent(s)
}
