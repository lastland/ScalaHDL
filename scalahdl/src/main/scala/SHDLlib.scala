package ScalaHDL

import scala.language.dynamics

import scala.collection.mutable.Stack
import scala.collection.mutable.HashMap

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
        // TODO: more bits
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
  }

  case class HDLIdent(hdl: ScalaHDL, name: Symbol)
      extends HDLObject(hdl) with Dynamic {
    def +(other: HDLObject) = HDLFunc2(hdl, add, this, other)
    def -(other: HDLObject) = HDLFunc2(hdl, sub, this, other)
    def *(other: HDLObject) = HDLFunc2(hdl, mul, this, other)
    def /(other: HDLObject) = HDLFunc2(hdl, div, this, other)
    def :=(other: HDLObject) = HDLAssignment.createAssignment(
      hdl, this, other)
    def is(value: Int): condition =
      if (value == 0) condition(this, negedge)
      else condition(this, posedge)
    override def convert(symTab: Map[Symbol, Any]): String = name.name
    override def exec(sigMap: Map[Symbol, Signal]) = sigMap(name)

    def apply(args: HDLObject*): HDLApply = {
      HDLApply.createApply(hdl, this, args)
    }

    def selectDynamic(name: String): HDLMethodCall = {
      new HDLMethodCall(hdl, this, name)
    }

    def applyDynamic(name: String)(args: Any*): HDLMethodCallWithArgs = {
      new HDLMethodCallWithArgs(hdl, this, name, args)
    }
  }

  class HDLMethodCall(hdl: ScalaHDL, ident: HDLIdent, methodName: String) {
    def getInstanceAndMethod(symTab: Map[Symbol, Any]):
        (ru.InstanceMirror, ru.Symbol) = {
      val inst = symTab(ident.name)
      val im = ru.runtimeMirror(getClass.getClassLoader).reflect(inst)
      val m = im.symbol.typeSignature.member(ru.newTermName(methodName))
      (im, m)
    }

    def convert(symTab: Map[Symbol, Any]) = {
      val im = getInstanceAndMethod(symTab)
      im._1.reflectMethod(im._2.asMethod)()
    }
  }

  class HDLMethodCallWithArgs(hdl: ScalaHDL,
    ident: HDLIdent, methodName: String, args: Seq[Any])
      extends HDLMethodCall(hdl, ident, methodName) {
    override def convert(symTab: Map[Symbol, Any]) = {
      val im = getInstanceAndMethod(symTab)
      im._1.reflectMethod(im._2.asMethod)(args)
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

  abstract class HDLBlock(hdl: ScalaHDL, func: () => HDLObject)
      extends HDLObject(hdl) {
    protected var _content: List[HDLObject] = List()

    val argsMap = new HashMap[Symbol, ArgInfo]

    def content = _content.reverse

    def addStmt(stmt: HDLObject) {
      _content = stmt :: _content
    }

    def extract() {
      if (_content.isEmpty) {
        if (!hdl.currentBlock.isEmpty) {
          hdl.currentBlock.top.addStmt(this)
          for (pair <- hdl.currentBlock.top.argsMap) {
            val info = pair._2
            argsMap += (pair._1 -> ArgInfo(info.name, info.tpe, input, info.size))
          }
        }
        hdl.currentBlock.push(this)
        func()
        hdl.currentBlock.pop()
        if (!hdl.currentBlock.isEmpty) {
          for (pair <- argsMap) {
            if (pair._2.dir == output)
              hdl.currentBlock.top.argsMap(pair._1) = pair._2
          }
        }
      }
    }
  }

  class HDLCondBlock(hdl: ScalaHDL, cond: _cond, name: String, func: () => HDLObject)
      extends HDLBlock(hdl, func) {
    override def convert(symTab: Map[Symbol, Any]): String =
      (cond match {
        case _sync(hdl, cond) =>
          "always @(" + cond.convert() + ") begin: _" + name + "\n"
      }) + (for (stmt <- content) yield stmt.convert(symTab)).mkString("") + "end\n"
    override def exec(sigMap: Map[Symbol, Signal]) = null
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
      f: () => HDLObject) = {
      val m = new HDLModule(hdl, name, params, f)
      hdl.modules += (name -> m)
      m
    }
  }

  class HDLModule(hdl: ScalaHDL,
    _name: Symbol, val params: Seq[Symbol], func: () => HDLObject)
      extends HDLBlock(hdl, func) {
    def name = _name.name

    def mapArgs(args: Seq[Any]): Map[Symbol, Any] = {
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

    override def exec(sigMap: Map[Symbol, Signal]): Signal = {
      null // TODO: write it!
    }
  }

  class ScalaHDL {
    import ScalaHDL.Core.DataType.Signals._

    implicit def string2Symbol(s: String) = Symbol(s)
    implicit def int2Signal(value: => Int) = new Signed("", value)
    implicit def int2HDLSignal(value: => Int) = HDLSignal(this, () => int2Signal(value))
    implicit def symbol2Ident(s: Symbol) = HDLIdent(this, s)
    implicit def signal2HDLSignal(s: Signal) = HDLSignal(this, () => s)

    private val hdl: ScalaHDL = this

    // TODO: Scope?
    val modules = new HashMap[Symbol, HDLModule]
    val currentBlock: Stack[HDLBlock] = new Stack()

    var sigs: Set[Signal] = Set()
    var siglist: List[Signal] = List()

    def sync(c: condition): _sync =
      new _sync(this, c)

    def delay(time: Int): _delay =
      new _delay(this, time)

    def cycle(a: HDLIdent): HDLAssignment =
      HDLAssignment.createAssignment(hdl, a, HDLFunc1(hdl, sub, a))

    def not(a: HDLObject): HDLFunc1 =
      HDLFunc1(hdl, sub, a)

    def module(name: Symbol, sigs: Signal*): module =
      new module(name, sigs: _*)

    object defMod extends Dynamic {
      def applyDynamic(name: String)(params: Symbol*)(f: => HDLObject): HDLModule = {
        HDLModule.createModule(hdl, name, params, () => f)
      }
    }

    def convert(name: Symbol, args: Any*) = {
      val m = modules(name)
      m.convert(m.mapArgs(args))
    }

    /*
     * Simulator class sugar.
     */
    def Simulator(hdl: ScalaHDL, mods: module*): ScalaHDL.Simulation.Simulator =
      new ScalaHDL.Simulation.Simulator(hdl, mods)
  }
}
