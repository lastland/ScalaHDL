package ScalaHDL

import scala.language.dynamics

import scala.collection.Set
import scala.collection.mutable.Stack
import scala.collection.mutable.HashSet
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

  class module(val name: Symbol, val sigs: Signal*) {
    def extract(hdl: ScalaHDL) {
      hdl.modules(name).mapArgs(sigs)
      hdl.modules(name).extract()
    }
  }

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
    def convert(): String
    def exec(sigMap: HashMap[Symbol, Signal]): Signal = {
      new Bool("", 0)
    }
  }

  abstract sealed class HDLFunc(hdl: ScalaHDL) extends HDLObject(hdl)

  case class HDLFunc1 (hdl: ScalaHDL,
    op: HDLOperation, a: HDLObject) extends HDLFunc(hdl) {
    override def convert(): String = op match {
      case `add` => " + " + a.convert()
      case `sub` => " - " + a.convert()
      case `mul` => " * " + a.convert()
      case `div` => " / " + a.convert()
    }
    override def exec(sigMap: HashMap[Symbol, Signal]): Signal = op match {
      case `sub` => val b = a.exec(sigMap)
        b.opposite
      case _ => a.exec(sigMap)
    }
  }

  case class HDLFunc2 (hdl: ScalaHDL,
    op: HDLOperation, a: HDLObject, b: HDLObject) extends HDLFunc(hdl) {
    override def convert(): String = op match {
      case `add` => a.convert() + " + " + b.convert()
      case `sub` => a.convert() + " - " + b.convert()
      case `mul` => a.convert() + " * " + b.convert()
      case `div` => a.convert() + " / " + b.convert()
    }
    override def exec(sigMap: HashMap[Symbol, Signal]): Signal = {
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
    override def convert(): String =
      if (hdl.currentBlock.top.argsMap(left.name).tpe == wire)
        "assign " + left.convert() + " = " + right.convert() + ";\n"
      else
        left.convert() + " <= " + right.convert() + ";\n"

    override def exec(sigMap: HashMap[Symbol, Signal]): Signal = {
      val sig = left.exec(sigMap)
      sig.next = right.exec(sigMap).value
      hdl.siglist.add(sig)
      sig
    }
  }

  object HDLAssignment {
    def createAssignment(hdl: ScalaHDL, id: HDLIdent, ob: HDLObject) = {
      val a = HDLAssignment(hdl, id, ob)
      hdl.currentBlock.top.addStmt(a)
      if (hdl.currentBlock.top.argsMap.contains(id.name)) {
        val v = hdl.currentBlock.top.argsMap(id.name)
        var dir = v.dir
        var tpe = v.tpe
        if (v.dir != middle) {
          dir = output
        }
        hdl.currentBlock.top match {
          case b: HDLCondBlock =>
            if (!b.simpleComb) {
              tpe = reg
            }
          case b =>
            tpe = reg
        }
        hdl.currentBlock.top.argsMap(id.name) =
          ArgInfo(v.name, tpe, dir, v.size)
      }
      hdl.currentBlock.top.senslist ++= findSenslist(ob)
      a
    }

    def findSenslist(elem: HDLObject): HashSet[Symbol] = {
      val ret = new HashSet[Symbol]
      elem match {
        case HDLIdent(_, name) =>
          ret += name
        case HDLFunc1(_, _, a) =>
          ret ++= findSenslist(a)
        case HDLFunc2(_, _, a, b) =>
          ret ++= findSenslist(a)
          ret ++= findSenslist(b)
        case sig: HDLSignal =>
          null
        case _ =>
          throw new RuntimeException("Not supported!")
      }
      ret
    }
  }

  class HDLType(val idt: HDLIdent, val info: ArgInfo) {
    private val hdl: ScalaHDL = idt.hdl
    def :=(other: HDLObject): HDLAssignment =
      HDLAssignment.createAssignment(hdl, idt, other)

    def <(other: HDLObject): HDLJudgement =
      HDLJudgement(hdl, lt, idt, other)

    def <=(other: HDLObject): HDLJudgement =
      HDLJudgement(hdl, let, idt, other)

    def >(other: HDLObject): HDLJudgement =
      HDLJudgement(hdl, gt, idt, other)

    def >=(other: HDLObject): HDLJudgement =
      HDLJudgement(hdl, get, idt, other)

    def is(other: HDLObject): HDLJudgement =
      HDLJudgement(hdl, eqt, idt, other)

    def +(other: HDLObject): HDLFunc2 =
      new HDLFunc2(hdl, add, idt, other)

    def -(other: HDLObject): HDLFunc2 =
      new HDLFunc2(hdl, sub, idt, other)

    def *(other: HDLObject): HDLFunc2 =
      new HDLFunc2(hdl, mul, idt, other)

    def /(other: HDLObject): HDLFunc2 =
      new HDLFunc2(hdl, div, idt, other)

    override def toString =
      "HDLType(" + idt.toString + "," + info.toString + ")"
  }

  case class HDLIdent(hdl: ScalaHDL, name: Symbol)
      extends HDLObject(hdl) {
    def +(other: HDLObject) = HDLFunc2(hdl, add, this, other)
    def -(other: HDLObject) = HDLFunc2(hdl, sub, this, other)
    def *(other: HDLObject) = HDLFunc2(hdl, mul, this, other)
    def /(other: HDLObject) = HDLFunc2(hdl, div, this, other)
    def :=(other: HDLObject) = HDLAssignment.createAssignment(
      hdl, this, other)
    def is(value: Int): condition =
      if (value == 0) condition(this, negedge)
      else condition(this, posedge)
    override def convert(): String = name.name
    override def exec(sigMap: HashMap[Symbol, Signal]) = sigMap(name)

    def apply(args: HDLObject*): HDLApply = {
      HDLApply.createApply(hdl, this, args)
    }
  }

  case class HDLApply(hdl: ScalaHDL, modIdent: HDLIdent, args: Seq[HDLObject])
      extends HDLObject(hdl) {
    override def convert(): String = "" // Not supported yet!
    override def exec(sigMap: HashMap[Symbol, Signal]) = null // TODO: write!
  }

  object HDLApply {
    def createApply(hdl: ScalaHDL, modId: HDLIdent, args: Seq[HDLObject]) = {
      val a = HDLApply(hdl, modId, args)
      a
    }
  }

  case class HDLSignal(hdl: ScalaHDL, sig: () => Signal) extends HDLObject(hdl) {
    override def convert(): String = sig().value.toString
    override def exec(sigMap: HashMap[Symbol, Signal]) = sig()
  }

  abstract class HDLBlock(hdl: ScalaHDL, func: () => Unit)
      extends HDLObject(hdl) {
    protected var _content: List[HDLObject] = List()

    val sigMap = new HashMap[Symbol, Signal]
    val argsMap = new HashMap[Symbol, ArgInfo]
    val senslist = new HashSet[Symbol]

    def content = {
      if (_content.isEmpty) extract()
      _content.reverse
    }

    def addStmt(stmt: HDLObject) {
      _content = stmt :: _content
    }

    def extract() {
      if (_content.isEmpty) {
        if (!hdl.currentBlock.isEmpty) {
          argsMap ++= hdl.currentBlock.top.argsMap
          sigMap ++= hdl.currentBlock.top.sigMap
        }
        hdl.currentBlock.push(this)
        func()
        hdl.currentBlock.pop()
        if (!hdl.currentBlock.isEmpty) {
          for (pair <- argsMap) {
            hdl.currentBlock.top.argsMap(pair._1) = pair._2
          }
          for (pair <- sigMap) {
            hdl.currentBlock.top.sigMap(pair._1) = pair._2
          }
          hdl.currentBlock.top.senslist ++= senslist
        }
      }
    }
  }

  class HDLCondBlock(hdl: ScalaHDL, val cond: _cond, name: String,
    func: () => Unit)
      extends HDLBlock(hdl, func) {

    private var _simpleComb: Boolean = true
    private var _simpleCombCalc: Boolean = false

    cond match {
      case s: _sync =>
        senslist += 'clk
      case _ => null
    }

    def simpleComb: Boolean = {
      cond match {
        case a: _async =>
          if (_simpleCombCalc) _simpleComb
          else {
            _simpleComb = !content.exists(stmt =>
              stmt match {
                case a: HDLAssignment => false
                case _ => true
              })
            _simpleCombCalc = true
            _simpleComb
          }
        case _ => false
      }
    }

    override def convert(): String =
      (cond match {
        case _sync(hdl, e) =>
          "always @(clk." +
          (if (e == posedge) "posedge"  else "negedge") +
          ") begin: _" +
            name + "\n" +
            (for (stmt <- content) yield stmt.convert()).mkString("") + "end\n"
        case _async(hdl) =>
          if (simpleComb) {
            (for (stmt <- content) yield stmt.convert()).mkString("")
          }
          else {
            "always @(" + senslist.map(_.name).mkString(", ") + ") begin\n" +
              (for (stmt <- content) yield stmt.convert()).mkString("") + "end\n"
          }
      }) + "\n"

    override def exec(sigMap: HashMap[Symbol, Signal]) = null
  }

  class _cond(hdl: ScalaHDL) extends Dynamic {

    def apply(f: => Unit): HDLCondBlock = {
      val b = new HDLCondBlock(hdl, this, "", () => f)
      hdl.currentBlock.top.addStmt(b)
      b.extract()
      b
    }

    def applyDynamic(name: String)(f: => Unit): HDLCondBlock = {
      val b = new HDLCondBlock(hdl, this, name, () => f)
      hdl.currentBlock.top.addStmt(b)
      b.extract()
      b
    }
  }

  case class HDLJudgement(hdl: ScalaHDL, op: HDLLogicOperator,
    a: HDLObject, b: HDLObject) extends HDLObject(hdl) {
    override def convert(): String = op match {
      case `lt` => a.convert() + " < " + b.convert()
      case `let` => a.convert() + " <= " + b.convert()
      case `eqt` => a.convert() + " == " + b.convert()
      case `gt` => a.convert() + " > " + b.convert()
      case `get` => a.convert() + " >= " + b.convert()
    }

    override def exec(sigMap: HashMap[Symbol, Signal]): Signal = {
      val bool = op match {
        case `lt` => a.exec(sigMap) < b.exec(sigMap)
        case `let` => a.exec(sigMap) <= b.exec(sigMap)
        case `eqt` => a.exec(sigMap) == b.exec(sigMap)
        case `gt` => a.exec(sigMap) > b.exec(sigMap)
        case `get` => a.exec(sigMap) >= b.exec(sigMap)
      }
      if (bool) new Bool("", 1)
      else new Bool("", 0)
    }
  }

  class HDLIf(hdl: ScalaHDL, parent: HDLIf, judge: HDLJudgement, func: () => Unit)
      extends HDLBlock(hdl, func) {
    var child: HDLIf = null

    def this(hdl: ScalaHDL, judge: HDLJudgement, func: () => Unit) {
      this(hdl, null, judge, func)
    }

    override def convert(): String =
      (if (parent != null) "else " else "") +
      (if (judge != null) "if (" + judge.convert() + ") " else "") +
      "begin\n" +
      content.map(_.convert()).mkString("") + "end\n" +
      (if (child != null) child.convert() else "")

    override def exec(sigMap: HashMap[Symbol, Signal]): Signal = {
      if (judge == null || judge.exec(sigMap).value > 0) {
        for (stmt <- content) {
          stmt.exec(sigMap)
        }
      }
      else if (child != null) {
        child.exec(sigMap)
      }
      null
    }

    def otherwise(f: => Unit): HDLIf = {
      val b = new HDLIf(hdl, this, null, () => f)
      child = b
      b.extract()
      b
    }

    def elsewhen(judge: HDLJudgement)(f: => Unit): HDLIf = {
      val b = new HDLIf(hdl, this, judge, () => f)
      child = b
      b.extract()
      b
    }
  }

  case class _sync(hdl: ScalaHDL, val e: Edge) extends _cond(hdl)

  case class _async(hdl: ScalaHDL) extends _cond(hdl)

  case class _delay(hdl: ScalaHDL, val time: Int) extends _cond(hdl)

  object HDLModule {
    def createModule(hdl: ScalaHDL, name: Symbol, params: Seq[Symbol],
      f: () => Unit) = {
      val m = new HDLModule(hdl, name, params, f)
      hdl.modules += (name -> m)
      m
    }
  }

  class HDLModule(hdl: ScalaHDL,
    _name: Symbol, val params: Seq[Symbol], func: () => Unit)
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

    override def convert(): String = {
      if (_content.isEmpty) extract()
      hdl.currentBlock.push(this)
      val s = "module %s (\n".format(name) + params.map(_.name).mkString(",\n") +
      "\n);\n\n" +
        (for (arg <- argsMap) yield arg._2.declaration).toList.sorted.mkString("") + "\n" +
        (for (stmt <- content) yield stmt.convert()).mkString("") +
      "\nendmodule\n"
      hdl.currentBlock.pop()
      s
    }

    def convert(args: Seq[Any]): String = {
      mapArgs(args)
      convert()
    }

    // TODO: implement
    override def exec(sigMap: HashMap[Symbol, Signal]): Signal = null
  }

  class ScalaHDL {
    import ScalaHDL.Core.DataType.Signals._

    implicit def string2Symbol(s: String) = Symbol(s)
    implicit def int2Signal(value: => Int) = new Signed("", value)
    implicit def int2HDLSignal(value: => Int) = HDLSignal(this, () => int2Signal(value))
    implicit def signal2HDLSignal(s: Signal) = HDLSignal(this, () => s)
    implicit def sym2HDLObj(s: Symbol): HDLObject = HDLIdent(this, s)
    implicit def tpe2HDLIdt(tpe: HDLType): HDLObject = tpe.idt
    implicit def sig2HDLType(sig: Signal): HDLType = toHDLType(sig)
    implicit def sym2HDLType(s: Symbol): HDLType = toHDLType(s)

    private val hdl: ScalaHDL = this
    private var tmpNum = 0;

    val modules = new HashMap[Symbol, HDLModule]
    val currentBlock: Stack[HDLBlock] = new Stack()

    var sigs: Set[Signal] = Set()
    var siglist: HashSet[Signal] = HashSet()

    def sync(e: Int): _sync =
      if (e == 1)
        new _sync(this, posedge)
      else
        new _sync(this, negedge)

    def sync(e: Edge): _sync =
      new _sync(this, e)

    def async: _async =
      new _async(this)

    def delay(time: Int): _delay =
      new _delay(this, time)

    def cycle(a: HDLObject): HDLAssignment = a match {
      case id: HDLIdent =>
        HDLAssignment.createAssignment(hdl, id, HDLFunc1(hdl, sub, id))
      case _ =>
        throw new RuntimeException("This parameter must be a HDLIdent!")
    }

    def not(a: HDLObject): HDLFunc1 =
      HDLFunc1(hdl, sub, a)

    def module(name: Symbol, sigs: Signal*): module =
      new module(name, sigs: _*)

    def when(stmt: HDLJudgement)(f: => Unit): HDLIf = {
      val b = new HDLIf(this, stmt, () => f)
      currentBlock.top.addStmt(b)
      b.extract()
      b
    }

    private def unusedName(set: Set[Symbol]): Symbol = {
      var res = "tmp_" + tmpNum;
      while (set.contains(res)) {
        tmpNum += 1
        res = "tmp_" + tmpNum
      }
      tmpNum += 1
      res
    }

    import ScalaHDL.Core.DataType.ArgInfo

    def toHDLType(name: Symbol): HDLType = new HDLType(
      HDLIdent(this, name), currentBlock.top.argsMap(name))

    def toHDLType(sig: Signal): HDLType = {
      val name = unusedName(currentBlock.top.argsMap.keySet)
      sig.name = name.name
      val info = ArgInfo(name.name, wire, middle, sig.size)
      hdl.currentBlock.top.argsMap += (name -> info)
      hdl.currentBlock.top.sigMap += (name -> sig)
      new HDLType(HDLIdent(this, name), info)
    }

    object defMod extends Dynamic {
      def applyDynamic(name: String)(params: Symbol*)(f: => Unit): HDLModule = {
        HDLModule.createModule(hdl, name, params, () => f)
      }
    }

    def convert(name: Symbol, args: Any*) = {
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
