package ScalaHDL

import scala.language.dynamics

import scala.collection.mutable.Stack
import scala.collection.mutable.HashMap
import scala.collection.immutable.Range.Inclusive

import scala.reflect.runtime.{universe => ru}

import ScalaHDL.Core.DataType._
import ScalaHDL.Core.DataType.SignalType._
import ScalaHDL.Core.DataType.SignalDirection._

package Core {

  /*
   * conditions.
   */

  import ScalaHDL.Core.DataType.Edge._
  case class condition(ident: HDLIdent, edge: Edge) {
    def convert(): String = edge match {
      case `posedge` => "posedge " + ident.name.name
      case `negedge` => "negedge " + ident.name.name
    }
  }

  /*
   * module class.
   */

  class module(val name: Symbol, val sigs: Signal*)

  /*
   * HDL Operations.
   */

  object HDLOperation extends Enumeration {
    type HDLOperation = Value
    val add, sub, mul, div = Value
  }
  import HDLOperation._

  object HDLLogicOperator extends Enumeration {
    type HDLLogicOperator = Value
    val lt, let, eqt, get, gt = Value
  }
  import HDLLogicOperator._

  /*
   * HDL Objects.
   */

  object HDLObject {
    def isHDLObject(x: Any): Boolean = x match {
      case x: HDLObject => true
      case _ => false
    }
  }

  abstract sealed class HDLObject(hdl: ScalaHDL) {
    def convert(symTab: Map[Symbol, Any]): String
    def exec(sigMap: Map[Symbol, Signal]): Signal = {
      new Bool("", 0)
    }
  }

  abstract sealed class HDLFunc(hdl: ScalaHDL) extends HDLObject(hdl)

  case class HDLFunc1 (hdl: ScalaHDL,
    op: HDLOperation, a: HDLObject) extends HDLFunc(hdl) {
    override def convert(symTab: Map[Symbol, Any]): String = op match {
      case `add` => " + " + a.convert(symTab)
      case `sub` => " - " + a.convert(symTab)
      case `mul` => " * " + a.convert(symTab)
      case `div` => " / " + a.convert(symTab)
    }
    override def exec(sigMap: Map[Symbol, Signal]): Signal = op match {
      case `sub` => val b = a.exec(sigMap)
        b.opposite
      case _ => a.exec(sigMap)
    }
  }

  case class HDLFunc2 (hdl: ScalaHDL,
    op: HDLOperation, a: HDLObject, b: HDLObject) extends HDLFunc(hdl) {
    override def convert(symTab: Map[Symbol, Any]): String = op match {
      case `add` => a.convert(symTab) + " + " + b.convert(symTab)
      case `sub` => a.convert(symTab) + " - " + b.convert(symTab)
      case `mul` => a.convert(symTab) + " * " + b.convert(symTab)
      case `div` => a.convert(symTab) + " / " + b.convert(symTab)
    }
    override def exec(sigMap: Map[Symbol, Signal]): Signal = {
      val sa = a.exec(sigMap)
      val sb = b.exec(sigMap)
      val va = sa.value
      val vb = sb.value
      op match {
        case `add` => sa + sb
        case `sub` => sa - sb
        case `mul` => sa * sb
        case `div` => sa / sb
      }
    }
  }

  case class HDLAssignment(hdl: ScalaHDL,
    left: HDLIdent, right: HDLObject) extends HDLObject(hdl) {
    override def convert(symTab: Map[Symbol, Any]): String =
      left.convert(symTab) + " <= " + right.convert(symTab) + ";\n"
    override def exec(sigMap: Map[Symbol, Signal]): Signal = {
      val sig = left.exec(sigMap)
      sig.next = right.exec(sigMap)
      hdl.siglist = sig :: hdl.siglist
      sig.next
    }
  }

  object HDLAssignment {
    def createAssignment(hdl: ScalaHDL, id: HDLIdent, ob: HDLObject) = {
      val a = HDLAssignment(hdl, id, ob)
      hdl.currentBlock.top.addStmt(a)
      if (hdl.currentBlock.top.argsMap.contains(id.name)) {
        val v = hdl.currentBlock.top.argsMap(id.name)
        hdl.currentBlock.top.argsMap(id.name) =
          ArgInfo(v.name, v.tpe, output, v.size)
      }
      a
    }

    def addSymbolBinding(hdl: ScalaHDL, id: HDLIdent, ob: Any) {
      hdl.currentBlock.top.addSymbolBinding(id.name, ob)
    }
  }

  case class HDLIdent(hdl: ScalaHDL, name: Symbol)
      extends HDLObject(hdl) {
    def +(other: HDLObject) = HDLFunc2(hdl, add, this, other)
    def -(other: HDLObject) = HDLFunc2(hdl, sub, this, other)
    def *(other: HDLObject) = HDLFunc2(hdl, mul, this, other)
    def /(other: HDLObject) = HDLFunc2(hdl, div, this, other)

    def +(other: Int) = {
      getValue match {
        case x: Int => x + other
        case x: Long => x + other
        case x: Short => x + other
        case x: Float => x + other
        case x: Double => x + other
        case x: Char => x + other
        case x: Byte => x + other
        case _ =>
          val res = invokeMethod("+", getTable, other)
          res match {
            case r: Boolean => r
            case _ => throw new RuntimeException(
              "the + method of %s should result in a Boolean!")
          }
      }
    }

    def <(other: HDLObject) = HDLJudgement(hdl, lt, this, other)
    def <=(other: HDLObject) = HDLJudgement(hdl, let, this, other)
    def ==(other: HDLObject) = HDLJudgement(hdl, eqt, this, other)
    def >=(other: HDLObject) = HDLJudgement(hdl, get, this, other)
    def >(other: HDLObject) = HDLJudgement(hdl, gt, this, other)

    // The following code is awful here,
    // how to reduce LOC in this part?
    def <(other: Int): Boolean = {
      getValue match {
        // because value class are not real class in Scala,
        // so you can't get real type using runtime reflection.
        case x: Int => x < other
        case x: Long => x < other
        case x: Short => x < other
        case x: Float => x < other
        case x: Double => x < other
        case x: Char => x < other
        case x: Byte => x < other
        case _ =>
          val res = invokeMethod("<", getTable, other)
          res match {
            case r: Boolean => r
            case _ => throw new RuntimeException(
              "the < method of %s should result in a Boolean!")
          }
      }
    }

    def <(other: Long): Boolean = {
      getValue match {
        case x: Int => x < other
        case x: Long => x < other
        case x: Short => x < other
        case x: Float => x < other
        case x: Double => x < other
        case x: Char => x < other
        case x: Byte => x < other
        case _ =>
          val res = invokeMethod("<", getTable, other)
          res match {
            case r: Boolean => r
            case _ => throw new RuntimeException(
              "the < method of %s should result in a Boolean!")
          }
      }
    }

    def <(other: Short): Boolean = {
      getValue match {
        case x: Int => x < other
        case x: Long => x < other
        case x: Short => x < other
        case x: Float => x < other
        case x: Double => x < other
        case x: Char => x < other
        case x: Byte => x < other
        case _ =>
          val res = invokeMethod("<", getTable, other)
          res match {
            case r: Boolean => r
            case _ => throw new RuntimeException(
              "the < method of %s should result in a Boolean!")
          }
      }
    }

    def <(other: Float): Boolean = {
      getValue match {
        case x: Int => x < other
        case x: Long => x < other
        case x: Short => x < other
        case x: Float => x < other
        case x: Double => x < other
        case x: Char => x < other
        case x: Byte => x < other
        case _ =>
          val res = invokeMethod("<", getTable, other)
          res match {
            case r: Boolean => r
            case _ => throw new RuntimeException(
              "the < method of %s should result in a Boolean!")
          }
      }
    }

    def <(other: Double): Boolean = {
      getValue match {
        case x: Int => x < other
        case x: Long => x < other
        case x: Short => x < other
        case x: Float => x < other
        case x: Double => x < other
        case x: Char => x < other
        case x: Byte => x < other
        case _ =>
          val res = invokeMethod("<", getTable, other)
          res match {
            case r: Boolean => r
            case _ => throw new RuntimeException(
              "the < method of %s should result in a Boolean!")
          }
      }
    }

    def <(other: Char): Boolean = {
      getValue match {
        case x: Int => x < other
        case x: Long => x < other
        case x: Short => x < other
        case x: Float => x < other
        case x: Double => x < other
        case x: Char => x < other
        case x: Byte => x < other
        case _ =>
          val res = invokeMethod("<", getTable, other)
          res match {
            case r: Boolean => r
            case _ => throw new RuntimeException(
              "the < method of %s should result in a Boolean!")
          }
      }
    }

    def <(other: Byte): Boolean = {
      getValue match {
        case x: Int => x < other
        case x: Long => x < other
        case x: Short => x < other
        case x: Float => x < other
        case x: Double => x < other
        case x: Char => x < other
        case x: Byte => x < other
        case _ =>
          val res = invokeMethod("<", getTable, other)
          res match {
            case r: Boolean => r
            case _ => throw new RuntimeException(
              "the < method of %s should result in a Boolean!")
          }
      }
    }

    def <(other: Any): Boolean = {
      val res = invokeMethod("<", getTable, other)
      res match {
        case r: Boolean => r
        case _ => throw new RuntimeException(
          "the < method of %s should result in a Boolean!")
      }
    }

    def <=(other: Int): Boolean = {
      getValue match {
        case x: Int => x <= other
        case x: Long => x <= other
        case x: Short => x <= other
        case x: Float => x <= other
        case x: Double => x <= other
        case x: Char => x <= other
        case x: Byte => x <= other
        case _ =>
          val res = invokeMethod("<=", getTable, other)
          res match {
            case r: Boolean => r
            case _ => throw new RuntimeException(
              "the <= method of %s should result in a Boolean!")
          }
      }
    }

    def <=(other: Long): Boolean = {
      getValue match {
        case x: Int => x <= other
        case x: Long => x <= other
        case x: Short => x <= other
        case x: Float => x <= other
        case x: Double => x <= other
        case x: Char => x <= other
        case x: Byte => x <= other
        case _ =>
          val res = invokeMethod("<=", getTable, other)
          res match {
            case r: Boolean => r
            case _ => throw new RuntimeException(
              "the <= method of %s should result in a Boolean!")
          }
      }
    }

    def <=(other: Short): Boolean = {
      getValue match {
        case x: Int => x <= other
        case x: Long => x <= other
        case x: Short => x <= other
        case x: Float => x <= other
        case x: Double => x <= other
        case x: Char => x <= other
        case x: Byte => x <= other
        case _ =>
          val res = invokeMethod("<=", getTable, other)
          res match {
            case r: Boolean => r
            case _ => throw new RuntimeException(
              "the <= method of %s should result in a Boolean!")
          }
      }
    }

    def <=(other: Float): Boolean = {
      getValue match {
        case x: Int => x <= other
        case x: Long => x <= other
        case x: Short => x <= other
        case x: Float => x <= other
        case x: Double => x <= other
        case x: Char => x <= other
        case x: Byte => x <= other
        case _ =>
          val res = invokeMethod("<=", getTable, other)
          res match {
            case r: Boolean => r
            case _ => throw new RuntimeException(
              "the <= method of %s should result in a Boolean!")
          }
      }
    }

    def <=(other: Double): Boolean = {
      getValue match {
        case x: Int => x <= other
        case x: Long => x <= other
        case x: Short => x <= other
        case x: Float => x <= other
        case x: Double => x <= other
        case x: Char => x <= other
        case x: Byte => x <= other
        case _ =>
          val res = invokeMethod("<=", getTable, other)
          res match {
            case r: Boolean => r
            case _ => throw new RuntimeException(
              "the <= method of %s should result in a Boolean!")
          }
      }
    }

    def <=(other: Char): Boolean = {
      getValue match {
        case x: Int => x <= other
        case x: Long => x <= other
        case x: Short => x <= other
        case x: Float => x <= other
        case x: Double => x <= other
        case x: Char => x <= other
        case x: Byte => x <= other
        case _ =>
          val res = invokeMethod("<=", getTable, other)
          res match {
            case r: Boolean => r
            case _ => throw new RuntimeException(
              "the <= method of %s should result in a Boolean!")
          }
      }
    }

    def <=(other: Byte): Boolean = {
      getValue match {
        case x: Int => x <= other
        case x: Long => x <= other
        case x: Short => x <= other
        case x: Float => x <= other
        case x: Double => x <= other
        case x: Char => x <= other
        case x: Byte => x <= other
        case _ =>
          val res = invokeMethod("<=", getTable, other)
          res match {
            case r: Boolean => r
            case _ => throw new RuntimeException(
              "the <= method of %s should result in a Boolean!")
          }
      }
    }

    def <=(other: Any): Boolean = {
      val res = invokeMethod("<=", getTable, other)
      res match {
        case r: Boolean => r
        case _ => throw new RuntimeException(
          "the <= method of %s should result in a Boolean!")
      }
    }

    override def equals(other: Any): Boolean = {
      val res = invokeMethod("==", getTable, other)
      res match {
        case r: Boolean => r
        case _ => throw new RuntimeException(
          "the == method of %s should result in a Boolean!")
      }
    }

    def >(other: Int): Boolean = {
      getValue match {
        case x: Int => x > other
        case x: Long => x > other
        case x: Short => x > other
        case x: Float => x > other
        case x: Double => x > other
        case x: Char => x > other
        case x: Byte => x > other
        case _ =>
          val res = invokeMethod(">", getTable, other)
          res match {
            case r: Boolean => r
            case _ => throw new RuntimeException(
              "the > method of %s should result in a Boolean!")
          }
      }
    }

    def >(other: Long): Boolean = {
      getValue match {
        case x: Int => x > other
        case x: Long => x > other
        case x: Short => x > other
        case x: Float => x > other
        case x: Double => x > other
        case x: Char => x > other
        case x: Byte => x > other
        case _ =>
          val res = invokeMethod(">", getTable, other)
          res match {
            case r: Boolean => r
            case _ => throw new RuntimeException(
              "the > method of %s should result in a Boolean!")
          }
      }
    }

    def >(other: Short): Boolean = {
      getValue match {
        case x: Int => x > other
        case x: Long => x > other
        case x: Short => x > other
        case x: Float => x > other
        case x: Double => x > other
        case x: Char => x > other
        case x: Byte => x > other
        case _ =>
          val res = invokeMethod(">", getTable, other)
          res match {
            case r: Boolean => r
            case _ => throw new RuntimeException(
              "the > method of %s should result in a Boolean!")
          }
      }
    }

    def >(other: Float): Boolean = {
      getValue match {
        case x: Int => x > other
        case x: Long => x > other
        case x: Short => x > other
        case x: Float => x > other
        case x: Double => x > other
        case x: Char => x > other
        case x: Byte => x > other
        case _ =>
          val res = invokeMethod(">", getTable, other)
          res match {
            case r: Boolean => r
            case _ => throw new RuntimeException(
              "the > method of %s should result in a Boolean!")
          }
      }
    }

    def >(other: Double): Boolean = {
      getValue match {
        case x: Int => x > other
        case x: Long => x > other
        case x: Short => x > other
        case x: Float => x > other
        case x: Double => x > other
        case x: Char => x > other
        case x: Byte => x > other
        case _ =>
          val res = invokeMethod(">", getTable, other)
          res match {
            case r: Boolean => r
            case _ => throw new RuntimeException(
              "the > method of %s should result in a Boolean!")
          }
      }
    }

    def >(other: Char): Boolean = {
      getValue match {
        case x: Int => x > other
        case x: Long => x > other
        case x: Short => x > other
        case x: Float => x > other
        case x: Double => x > other
        case x: Char => x > other
        case x: Byte => x > other
        case _ =>
          val res = invokeMethod(">", getTable, other)
          res match {
            case r: Boolean => r
            case _ => throw new RuntimeException(
              "the > method of %s should result in a Boolean!")
          }
      }
    }

    def >(other: Byte): Boolean = {
      getValue match {
        case x: Int => x > other
        case x: Long => x > other
        case x: Short => x > other
        case x: Float => x > other
        case x: Double => x > other
        case x: Char => x > other
        case x: Byte => x > other
        case _ =>
          val res = invokeMethod(">", getTable, other)
          res match {
            case r: Boolean => r
            case _ => throw new RuntimeException(
              "the > method of %s should result in a Boolean!")
          }
      }
    }

    def >(other: Any): Boolean = {
      val res = invokeMethod(">", getTable, other)
      res match {
        case r: Boolean => r
        case _ => throw new RuntimeException(
          "the > method of %s should result in a Boolean!")
      }
    }

    def >=(other: Int): Boolean = {
      getValue match {
        case x: Int => x >= other
        case x: Long => x >= other
        case x: Short => x >= other
        case x: Float => x >= other
        case x: Double => x >= other
        case x: Char => x >= other
        case x: Byte => x >= other
        case _ =>
          val res = invokeMethod(">=", getTable, other)
          res match {
            case r: Boolean => r
            case _ => throw new RuntimeException(
              "the >= method of %s should result in a Boolean!")
          }
      }
    }

    def >=(other: Long): Boolean = {
      getValue match {
        case x: Int => x >= other
        case x: Long => x >= other
        case x: Short => x >= other
        case x: Float => x >= other
        case x: Double => x >= other
        case x: Char => x >= other
        case x: Byte => x >= other
        case _ =>
          val res = invokeMethod(">=", getTable, other)
          res match {
            case r: Boolean => r
            case _ => throw new RuntimeException(
              "the >= method of %s should result in a Boolean!")
          }
      }
    }

    def >=(other: Short): Boolean = {
      getValue match {
        case x: Int => x >= other
        case x: Long => x >= other
        case x: Short => x >= other
        case x: Float => x >= other
        case x: Double => x >= other
        case x: Char => x >= other
        case x: Byte => x >= other
        case _ =>
          val res = invokeMethod(">=", getTable, other)
          res match {
            case r: Boolean => r
            case _ => throw new RuntimeException(
              "the >= method of %s should result in a Boolean!")
          }
      }
    }

    def >=(other: Float): Boolean = {
      getValue match {
        case x: Int => x >= other
        case x: Long => x >= other
        case x: Short => x >= other
        case x: Float => x >= other
        case x: Double => x >= other
        case x: Char => x >= other
        case x: Byte => x >= other
        case _ =>
          val res = invokeMethod(">=", getTable, other)
          res match {
            case r: Boolean => r
            case _ => throw new RuntimeException(
              "the >= method of %s should result in a Boolean!")
          }
      }
    }

    def >=(other: Double): Boolean = {
      getValue match {
        case x: Int => x >= other
        case x: Long => x >= other
        case x: Short => x >= other
        case x: Float => x >= other
        case x: Double => x >= other
        case x: Char => x >= other
        case x: Byte => x >= other
        case _ =>
          val res = invokeMethod(">=", getTable, other)
          res match {
            case r: Boolean => r
            case _ => throw new RuntimeException(
              "the >= method of %s should result in a Boolean!")
          }
      }
    }

    def >=(other: Char): Boolean = {
      getValue match {
        case x: Int => x >= other
        case x: Long => x >= other
        case x: Short => x >= other
        case x: Float => x >= other
        case x: Double => x >= other
        case x: Char => x >= other
        case x: Byte => x >= other
        case _ =>
          val res = invokeMethod(">=", getTable, other)
          res match {
            case r: Boolean => r
            case _ => throw new RuntimeException(
              "the >= method of %s should result in a Boolean!")
          }
      }
    }

    def >=(other: Byte): Boolean = {
      getValue match {
        case x: Int => x >= other
        case x: Long => x >= other
        case x: Short => x >= other
        case x: Float => x >= other
        case x: Double => x >= other
        case x: Char => x >= other
        case x: Byte => x >= other
        case _ =>
          val res = invokeMethod(">=", getTable, other)
          res match {
            case r: Boolean => r
            case _ => throw new RuntimeException(
              "the >= method of %s should result in a Boolean!")
          }
      }
    }

    def >=(other: Any): Boolean = {
      val res = invokeMethod(">=", getTable, other)
      res match {
        case r: Boolean => r
        case _ => throw new RuntimeException(
          "the >= method of %s should result in a Boolean!")
      }
    }

    def :=(other: Any) {
      other match {
        case x: HDLObject => HDLAssignment.createAssignment(
          hdl, this, x)
        case _ => HDLAssignment.addSymbolBinding(
          hdl, this, other)
      }
    }

    def is(value: Int): condition =
      if (value == 0) condition(this, negedge)
      else condition(this, posedge)
    override def convert(symTab: Map[Symbol, Any]): String = name.name
    override def exec(sigMap: Map[Symbol, Signal]) = sigMap(name)

    private def argTranslate(arg: Any): Any = arg match {
      case x: HDLIdent => x.getValue
      case _ => arg
    }

    // Unfortunately, Dynamic class doesn't work well with implicit conversion,
    // We have to take this awful approach instead for now.
    def apply(funcName: String)(args: Any*): Any = {
      val m = getTable
      val a: Seq[Any] = for (arg <- args) yield argTranslate(arg)
      invokeMethod(funcName, m, a:_*)
    }

    def apply(args: Any*) = {
      //HDLApply.createApply(hdl, this, args)
      if (hdl.blocks.contains(name)) {
        val blk = hdl.blocks(name)
        blk match {
          case m: HDLModule =>
            hdl.currentBlock.top.addStmt(
              HDLApply(hdl, this, args.asInstanceOf[Seq[HDLObject]]))
          case f: HDLFuncBlock =>
            f.applyFunc(args)
        }
      } else if (getTable.contains(name)) {
      }
    }

    private def getTable: HashMap[Symbol, Any] =
      hdl.currentFrame.top

    def getValue: Any =
      getTable(name)

    private def getInstanceAndMethod(funcName: String, symTab: HashMap[Symbol, Any]):
        (ru.InstanceMirror, ru.Symbol) = {
      val inst = symTab(name)
      val im = ru.runtimeMirror(getClass.getClassLoader).reflect(inst)
      val m = im.symbol.typeSignature.member(ru.newTermName(funcName))
      (im, m)
    }

    private def testMethodValid(funcName: String, symTab: HashMap[Symbol, Any]):
        Boolean = {
      val (im, mm) = getInstanceAndMethod(funcName, symTab)
      testMethodValid(im, mm)
    }

    private def testMethodValid(im: ru.InstanceMirror, mm: ru.Symbol) :
        Boolean = {
      mm != null
    }

    private def invokeMethod(funcName: String, symTab: HashMap[Symbol, Any],
      args: Any*):
        Any = {
      val (im, mm) = getInstanceAndMethod(funcName, symTab)
      println(im)
      println(mm)
      if (!testMethodValid(im, mm)) throw new NoSuchMethodException(
        "no such method called %s of %s".format(funcName, name))
      im.reflectMethod(mm.asMethod)(args:_*)
    }
  }

  case class HDLApply(hdl: ScalaHDL, modIdent: HDLIdent, args: Seq[HDLObject])
      extends HDLObject(hdl) {
    override def convert(symTab: Map[Symbol, Any]): String = "" // Not supported yet!
    override def exec(sigMap: Map[Symbol, Signal]) = null // TODO: write!
  }

  object HDLApply {
    def createApply(hdl: ScalaHDL, modId: HDLIdent, args: Seq[HDLObject]) = {
      val a = HDLApply(hdl, modId, args)
      a
    }
  }

  case class HDLSignal(hdl: ScalaHDL, sig: () => Signal) extends HDLObject(hdl) {
    override def convert(symTab: Map[Symbol, Any]): String = sig().value.toString
    override def exec(sigMap: Map[Symbol, Signal]) = sig()
  }

  abstract class HDLBlock(hdl: ScalaHDL, func: () => Any)
      extends HDLObject(hdl) {
    protected var _content: List[HDLObject] = List()

    val argsMap = new HashMap[Symbol, ArgInfo]

    def content = _content.reverse

    def addStmt(stmt: HDLObject) {
      _content = stmt :: _content
    }

    def addSymbolBinding(k: Symbol, v: Any) {
      val m = hdl.currentFrame.top
      if (m.contains(k))
        m(k) = v
      else
        m += (k -> v)
    }

    protected def preAction() { }

    protected def postAction() { }

    def extract() {
      if (_content.isEmpty) {
        preAction()
        hdl.currentBlock.push(this)
        func()
        hdl.currentBlock.pop()
        postAction()
      }
    }
  }

  class HDLCondBlock(hdl: ScalaHDL, cond: _cond, name: String, func: () => Unit)
      extends HDLBlock(hdl, func) {
    override protected def preAction() {
      if (!hdl.currentBlock.isEmpty) {
        hdl.currentBlock.top.addStmt(this)
        for (pair <- hdl.currentBlock.top.argsMap) {
          val info = pair._2
          argsMap += (pair._1 -> ArgInfo(info.name, info.tpe, input, info.size))
        }
      }
    }

    override protected def postAction() {
      if (!hdl.currentBlock.isEmpty) {
        for (pair <- argsMap) {
          if (pair._2.dir == output)
            hdl.currentBlock.top.argsMap(pair._1) = pair._2
        }
      }
    }

    override def convert(symTab: Map[Symbol, Any]): String =
      (cond match {
        case _sync(hdl, cond) =>
          "always @(" + cond.convert() + ") begin: _" + name + "\n"
      }) + (for (stmt <- content) yield stmt.convert(symTab)).mkString("") + "end\n"
    override def exec(sigMap: Map[Symbol, Signal]) = null
  }

  object HDLFuncBlock {
    def createFuncBlock(hdl: ScalaHDL, name: Symbol, args: Seq[Symbol],
      func: () => Unit): HDLFuncBlock = {
      val b = new HDLFuncBlock(hdl, name, args, func)
      hdl.blocks += (name -> b)
      b
    }
  }

  class HDLFuncBlock(hdl: ScalaHDL, name: Symbol, params: Seq[Symbol],
    func: () => Unit)
      extends HDLBlock(hdl, func) {
    private def symTab = new HashMap[Symbol, Any]

    // TODO: implement!
    override def convert(symTab: Map[Symbol, Any]): String = ""
    override def exec(sigMap: Map[Symbol, Signal]) = null

    override def preAction() {
      val m = new HashMap[Symbol, Any]()
      if (!hdl.currentFrame.isEmpty) {
        for (pair <- hdl.currentFrame.top) {
          m += pair
        }
      }
      for (pair <- symTab) {
        if (m.contains(pair._1)) {
          m(pair._1) = pair._2
        } else {
          m += pair
        }
      }
      hdl.currentFrame.push(m)
    }

    override def postAction() {
      hdl.currentFrame.pop()
    }

    def applyFunc(args: Seq[Any]) {
      if (params.size != args.size)
        throw new WrongNumberOfArgumentsException(name, params.size, args.size)
      val m = params.zip(args).toMap
      symTab.clear()
      for (pair <- m) symTab += pair
      extract()
    }
  }

  class _cond(hdl: ScalaHDL) extends Dynamic {
    def applyDynamic(name: String)(f: => HDLObject): HDLCondBlock = {
      val b = new HDLCondBlock(hdl, this, name, () => f)
      b.extract()
      b
    }
  }

  case class _sync(hdl: ScalaHDL, val cond: condition) extends _cond(hdl)

  case class _delay(hdl: ScalaHDL, val time: Int) extends _cond(hdl)

  object HDLModule {
    def createModule(hdl: ScalaHDL, name: Symbol, params: Seq[Symbol],
      f: () => Unit) = {
      val m = new HDLModule(hdl, name, params, f)
      hdl.blocks += (name -> m)
      m
    }
  }

  class HDLModule(hdl: ScalaHDL,
    _name: Symbol, val params: Seq[Symbol], func: () => Unit)
      extends HDLBlock(hdl, func) {
    def name = _name.name

    private val symTab = new HashMap[Symbol, Any]

    override protected def preAction() {
      if (!hdl.currentBlock.isEmpty) {
        hdl.currentBlock.top.addStmt(this)
        for (pair <- hdl.currentBlock.top.argsMap) {
          val info = pair._2
          argsMap += (pair._1 -> ArgInfo(info.name, info.tpe, input, info.size))
        }
      }
      val m = new HashMap[Symbol, Any]()
      if (!hdl.currentFrame.isEmpty) {
        for (pair <- hdl.currentFrame.top) {
          m += pair
        }
      }
      for (pair <- symTab) {
        if (m.contains(pair._1)) {
          m(pair._1) = pair._2
        } else {
          m += pair
        }
      }
      hdl.currentFrame.push(m)
    }

    override protected def postAction() {
      if (!hdl.currentBlock.isEmpty) {
        for (pair <- argsMap) {
          if (pair._2.dir == output)
            hdl.currentBlock.top.argsMap(pair._1) = pair._2
        }
      }
      hdl.currentFrame.pop()
    }

    private def mapArgs(args: Seq[Any]): Map[Symbol, Any] = {
      if (params.size != args.size)
        throw new WrongNumberOfArgumentsException(_name, params.size, args.size)
      val m = params.zip(args).toMap
      for (param <- params) {
        m(param) match {
          case s: Signal =>
            argsMap += (param -> ArgInfo(param.name, wire, input, s.size))
          case _ => throw new IllegalArgumentException(
            "argument must be subclass of Signal")
        }
      }
      for (kv <- m) symTab += kv
      m
    }

    override def convert(symTab: Map[Symbol, Any]): String = {
      if (_content.isEmpty) extract()
      val s = "module %s (\n".format(name) + params.map(_.name).mkString("\n") +
      "\n);\n\n" +
        (for (param <- params) yield argsMap(param).toHDL).mkString("") + "\n" +
        (for (stmt <- content) yield stmt.convert(symTab)).mkString("") +
      "\nendmodule\n"
      s
    }

    def convert(args: Seq[Any]): String = {
      convert(mapArgs(args))
    }

    override def exec(sigMap: Map[Symbol, Signal]): Signal = {
      null // TODO: write it!
    }
  }


  case class HDLJudgement(hdl: ScalaHDL, op: HDLLogicOperator,
    a: HDLObject, b: HDLObject) extends HDLObject(hdl) {
    // TODO: implement!
    override def convert(symTab: Map[Symbol, Any]): String = ""
    override def exec(sigMap: Map[Symbol, Signal]): Signal = null
  }

  class HDLIf(hdl: ScalaHDL, parent: HDLIf, judge: HDLJudgement, func: () => Unit)
      extends HDLObject(hdl) {
    def this(hdl: ScalaHDL, judge: HDLJudgement, func: () => Unit) {
      this(hdl, null, judge, func)
    }
    // TODO: implement!
    override def convert(symTab: Map[Symbol, Any]): String = ""
    override def exec(sigMap: Map[Symbol, Signal]): Signal = null
    def otherwise(f: => Unit): HDLIf =
      new HDLIf(hdl, this, null, func)
    def elsewhen(judge: HDLJudgement)(f: => Unit): HDLIf =
      new HDLIf(hdl, this, judge, func)
  }

  case class HDLInt(hdl: ScalaHDL, value: Int) {
    def to(end: HDLInt): Inclusive = new Inclusive(value, end.value, 1)
    def to(end: HDLIdent): Inclusive = {
      val obj = end.getValue
      obj match {
        case r: Int => new Inclusive(value, r, 1)
        case _ => throw new IllegalArgumentException(
          "%s should be a Int!".format(end.name))
      }
    }
    def to(end: Symbol): Inclusive = to(HDLIdent(hdl, end))

    def +(other: HDLInt): Int = value + other.value
    def +(other: HDLIdent): Int = {
      val obj = other.getValue
      obj match {
        case r: Int => value + r
        case _ => throw new IllegalArgumentException(
          "%s should be a Int!".format(other.name))
      }
    }
    def +(other: Symbol): Int = this.+(HDLIdent(hdl, other))
  }

  class ScalaHDL {
    import ScalaHDL.Core.DataType.Signals._

    implicit def string2Symbol(s: String) = Symbol(s)
    implicit def int2Signal(value: => Int) = new Signed("", value)
    implicit def int2HDLSignal(value: => Int) = HDLSignal(this, () => int2Signal(value))
    implicit def symbol2Ident(s: Symbol) = HDLIdent(this, s)
    implicit def signal2HDLSignal(s: Signal) = HDLSignal(this, () => s)
    implicit def int2HDLInt(value: Int) = HDLInt(this, value)

    private val hdl: ScalaHDL = this

    val blocks = new HashMap[Symbol, HDLBlock]
    val currentBlock: Stack[HDLBlock] = new Stack()
    val currentFrame: Stack[HashMap[Symbol, Any]] = new Stack()

    var sigs: Set[Signal] = Set()
    var siglist: List[Signal] = List()

    def sync(c: condition): _sync = new _sync(this, c)

    def delay(time: Int): _delay = new _delay(this, time)

    def cycle(a: HDLIdent): HDLAssignment =
      HDLAssignment.createAssignment(hdl, a, HDLFunc1(hdl, sub, a))

    def not(a: HDLObject): HDLFunc1 = HDLFunc1(hdl, sub, a)

    def module(name: Symbol, sigs: Signal*): module = new module(name, sigs: _*)

    object defMod extends Dynamic {
      def applyDynamic(name: String)(params: Symbol*)(f: => Unit): HDLModule = {
        HDLModule.createModule(hdl, name, params, () => f)
      }
    }

    object defFunc extends Dynamic {
      def applyDynamic(name: String)(params: Symbol*)(f: => Unit): HDLFuncBlock = {
        HDLFuncBlock.createFuncBlock(hdl, name, params, () => f)
      }
    }

    def when(stmt: HDLJudgement)(f: => Unit): HDLIf =
      new HDLIf(this, stmt, () => f)

    def convert(name: Symbol, args: Any*) = {
      val m = blocks(name)
      m match {
        case m: HDLModule => m.convert(args)
        case _ => throw new IllegalArgumentException(
          "%s must be name of a module!".format(name))
      }
    }

    /*
     * Simulator class sugar.
     */
    def Simulator(hdl: ScalaHDL, mods: module*): ScalaHDL.Simulation.Simulator =
      new ScalaHDL.Simulation.Simulator(hdl, mods)
  }
}
