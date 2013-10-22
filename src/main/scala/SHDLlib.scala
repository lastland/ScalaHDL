package ScalaHDL

import scala.language.dynamics

import scala.collection.mutable.HashMap

import ScalaHDL.DataType._

class ScalaHDL { 

  implicit def string2Symbol(s: String) = Symbol(s)

  private val modules = new HashMap[Symbol, HDLModule]
  private val moduleStmts = new HashMap[Symbol, List[HDLObject]] { 
    override def default(key: Symbol) = List[HDLObject]()
  }
  private var currentMod = new HDLModule('notused, List())

  object HDLOperation extends Enumeration { 
    type HDLOperation = Value
    val add, sub, mul, div = Value
  }
  import HDLOperation._

  abstract sealed class HDLObject { 
    def convert(): String
  }

  abstract sealed class HDLFunc extends HDLObject

  case class HDLFunc2 (op: HDLOperation, a: HDLObject, b: HDLObject) extends HDLFunc { 
    def convert(): String = op match { 
      case `add` => a.convert() + " + " + b.convert()
      case `sub` => a.convert() + " - " + b.convert()
      case `mul` => a.convert() + " * " + b.convert()
      case `div` => a.convert() + " / " + b.convert()
    }
  }

  case class HDLAssignment(left: HDLIdent, right: HDLObject) extends HDLObject { 
    def convert(): String = 
      left.convert() + "<=" + right.convert()
  }

  case class HDLIdent(name: Symbol) extends HDLObject { 
    def +(other: HDLObject) = HDLFunc2(add, this, other)
    def -(other: HDLObject) = HDLFunc2(sub, this, other)
    def *(other: HDLObject) = HDLFunc2(mul, this, other)
    def /(other: HDLObject) = HDLFunc2(div, this, other)
    def :=(other: HDLObject) = HDLAssignment.createAssignment(currentMod, this, other)
    def convert(): String = name.name
  }

  case class HDLModule(name: Symbol, params: Seq[Symbol]) { 
    def apply(f: => HDLObject) = f

    def convert(args: Seq[HDLDataType]): String = {
      val lst = moduleStmts(name)
      val modHeader = "module " + name.name + "(\n" +
        params.map(_.name).mkString(",\n") + "\n);\n"
      val stmts = for (stmt <- moduleStmts(name).reverse) yield stmt.convert()
      modHeader + "\nbegin\n" + stmts.mkString(";\n") + "\nend\n\nendmodule\n"
    }
  }

  object HDLModule { 
    def createModule(name: Symbol, params: Seq[Symbol]) = { 
      val m = HDLModule(name, params)
      modules += (name -> m)
      currentMod = m
      m
    }
  }

  object HDLAssignment { 
    def createAssignment(m: HDLModule, id: HDLIdent, ob: HDLObject) = { 
      val a = HDLAssignment(id, ob)
      moduleStmts.update(m.name, a :: moduleStmts(m.name))
      a
    }
  }

  object module extends Dynamic { 
    def applyDynamic(name: String)(params: Symbol*): HDLModule = { 
      HDLModule.createModule(name, params)
    }
  }

  def convert(name: Symbol, args: HDLDataType*) = { 
    val m = modules(name)
    m.convert(args)
  }

  implicit def symbol2Ident(s: Symbol) = HDLIdent(s)
}
