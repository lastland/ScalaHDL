package ScalaHDL

import scala.language.dynamics

import scala.collection.mutable.HashMap

import ScalaHDL.Core.DataType._

package Core {

  /*
   * conditions.
   */

  sealed abstract class condition {
    def copy(): condition
    def header(): String = ""
  }

  final class _nocondition extends condition {
    override def copy(): condition = new _nocondition
  }

  final class _sync(val symbol: Symbol, v: Int) extends condition {

    private def _convert(name: String, value: Int): String = value match {
      case -1 => ""
      case 1 => "posedge " + name
      case 0 => "negedge " + name
      case _ => throw new RuntimeException // Not supposed to happen
    }

    override def header(): String = {
      // TODO: multiple sync
      "always @ (" + _convert(symbol.name, v) + ")"
    }

    override def copy(): condition =
      new  _sync(symbol, v)

    def cond = v
  }

  final class _delay(val time: Int) extends condition {
    override def copy(): condition =
      new _delay(time)
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

  abstract sealed class HDLObject(hdl: ScalaHDL) {
    def convert(): String
    def exec(sigMap: Map[Symbol, Signal]): Signal = {
      new Signal("", 0, 1)
    }
  }

  abstract sealed class HDLFunc(hdl: ScalaHDL) extends HDLObject(hdl)

  case class HDLFunc1 (hdl: ScalaHDL,
    op: HDLOperation, a: HDLObject) extends HDLFunc(hdl) {
    def convert(): String = op match {
      case `add` => " + " + a.convert()
      case `sub` => " - " + a.convert()
      case `mul` => " * " + a.convert()
      case `div` => " / " + a.convert()
    }
    override def exec(sigMap: Map[Symbol, Signal]): Signal = op match {
      case `sub` => val b = a.exec(sigMap)
        // TODO: more bits
        new Signal("", 1 - b.value, b.bits)
      case _ => a.exec(sigMap)
    }
  }

  case class HDLFunc2 (hdl: ScalaHDL,
    op: HDLOperation, a: HDLObject, b: HDLObject) extends HDLFunc(hdl) {
    def convert(): String = op match {
      case `add` => a.convert() + " + " + b.convert()
      case `sub` => a.convert() + " - " + b.convert()
      case `mul` => a.convert() + " * " + b.convert()
      case `div` => a.convert() + " / " + b.convert()
    }
    override def exec(sigMap: Map[Symbol, Signal]): Signal = {
      val sa = a.exec(sigMap)
      val sb = b.exec(sigMap)
      val va = sa.value
      val vb = sb.value
      op match {
        case `add` => new Signal("", va + vb)
        case `sub` => new Signal("", va - vb)
        case `mul` => new Signal("", va * vb)
        case `div` => new Signal("", va / vb)
      }
    }
  }

  case class HDLAssignment(hdl: ScalaHDL,
    left: HDLIdent, right: HDLObject) extends HDLObject(hdl) {
    def convert(): String =
      left.convert() + "<=" + right.convert()
    override def exec(sigMap: Map[Symbol, Signal]) = {
      val sig = left.exec(sigMap)
      sig.next = right.exec(sigMap)
      hdl.siglist = sig :: hdl.siglist
      sig.next
    }
  }

  object HDLAssignment {
    def createAssignment(hdl: ScalaHDL,
      id: HDLIdent, ob: HDLObject) = {
      val a = HDLAssignment(hdl, id, ob)
      val n = hdl.currentMod.name
      hdl.moduleStmts.update(n, a :: hdl.moduleStmts(n))
      a
    }
  }

  case class HDLIdent(hdl: ScalaHDL, name: Symbol) extends HDLObject(hdl) {
    def +(other: HDLObject) = HDLFunc2(hdl, add, this, other)
    def -(other: HDLObject) = HDLFunc2(hdl, sub, this, other)
    def *(other: HDLObject) = HDLFunc2(hdl, mul, this, other)
    def /(other: HDLObject) = HDLFunc2(hdl, div, this, other)
    def :=(other: HDLObject) = HDLAssignment.createAssignment(
      hdl, this, other)
    def is(value: Int): _sync = new _sync(name, value)
    override def convert(): String = name.name
    override def exec(sigMap: Map[Symbol, Signal]) = sigMap(name)
  }

  case class HDLSignal(hdl: ScalaHDL, sig: () => Signal) extends HDLObject(hdl) {
    def convert(): String = sig().value.toString
    override def exec(sigMap: Map[Symbol, Signal]) = sig()
  }

  case class HDLModule(hdl: ScalaHDL, name: Symbol, params: Seq[Symbol]) {
    def apply(f: => HDLObject) = f

    def convert(args: Seq[HDLDataType]): String = {
      val lst = hdl.moduleStmts(name)
      val modHeader = "module " + name.name + "(\n" +
      params.map(_.name).mkString(",\n") + "\n);\n"
      val beginHeader = hdl.moduleConds(name).header + "begin\n"
      val stmts = for (stmt <- hdl.moduleStmts(name).reverse) yield stmt.convert()
      modHeader + "\n" + beginHeader + stmts.mkString(";\n") + "\nend\n\nendmodule\n"
    }
  }

  object HDLModule {
    def createModule(hdl: ScalaHDL, name: Symbol, params: Seq[Symbol]) = {
      val m = HDLModule(hdl, name, params)
      hdl.modules += (name -> m)
      hdl.moduleConds += (name -> hdl.currentCond.copy)
      hdl.currentMod = m
      hdl.currentCond = new _nocondition
      m
    }
  }

  class ScalaHDL {
    import ScalaHDL.Core.DataType.SignalMaker._

    implicit def string2Symbol(s: String) = Symbol(s)
    implicit def int2Signal(value: => Int) = new Signal("", value)
    implicit def int2HDLSignal(value: => Int) = HDLSignal(this, () => int2Signal(value))
    implicit def symbol2Ident(s: Symbol) = HDLIdent(this, s)
    implicit def signal2HDLSignal(s: Signal) = HDLSignal(this, () => s)

    private val hdl: ScalaHDL = this

    // TODO: Scope?
    val modules = new HashMap[Symbol, HDLModule]
    val moduleStmts = new HashMap[Symbol, List[HDLObject]] {
      override def default(key: Symbol) = List[HDLObject]()
    }
    val moduleConds = new HashMap[Symbol, condition]()
    val moduleSigMap = new HashMap[Symbol, Map[Symbol, Signal]]

    var currentMod = new HDLModule(hdl, 'notused, List())
    var currentCond: condition = new _nocondition

    var sigs: Set[Signal] = Set()
    var siglist: List[Signal] = List()

    def sync(s: => _sync) {
      currentCond = s
    }

    def delay(t: Int) {
      currentCond = new _delay(t)
    }

    def cycle(a: HDLIdent): HDLAssignment =
      HDLAssignment.createAssignment(hdl, a, HDLFunc1(hdl, sub, a))

    def module(name: Symbol, sigs: Signal*): module =
      new module(name, sigs: _*)

    object defMod extends Dynamic {
      def applyDynamic(name: String)(params: Symbol*): HDLModule = {
        HDLModule.createModule(hdl, name, params)
      }
    }

    def convert(name: Symbol, args: HDLDataType*) = {
      val m = modules(name)
      m.convert(args)
    }

    /*
     * Simulator class sugar.
     */
    def Simulator(hdl: ScalaHDL, mods: module*): ScalaHDL.Simulation.Simulator =
      new ScalaHDL.Simulation.Simulator(hdl, mods)
  }
}
