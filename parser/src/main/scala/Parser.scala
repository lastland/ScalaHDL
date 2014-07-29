package ScalaHDL.Parser

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.Positional
import scala.util.parsing.input.CharArrayReader

class ScalaHDLParser extends RegexParsers {

  def identifier = """[_\p{L}][_\p{L}\p{Nd}]*""".r

  def hdlidentifier = """[_\p{L}][_\p{L}\p{Nd}]*""".r ~
    ("[" ~> integer <~ "]").? ^^ {
      case symbol ~ num => Identifier(symbol, num)
    }

  def integer = """(0|[1-9]\d*)""".r ^^ (_.toInt)

  def string = """\"[_\p{L}\p{Nd}\.]*\"""".r

  def packageName = """[_\p{L}][_\p{L}\p{Nd}\.]*""".r

  def value = (hdlidentifier) | (integer ^^ { HDLInt(_) })

  def anyBut(c: Elem) = Parser { input =>
    if (input.atEnd || input.first.equals(c)) Failure("", input)
    else Success(input.first, input.rest)
  }

  def packageDec = "@package" ~> "=" ~> packageName

  def programName = "@" ~> identifier

  def program =
    packageDec.? ~ programName ~ module.* ~ mainProgram ^^ {
      case packageDec ~ name ~ modules ~ mainp => Program(name, modules, mainp, packageDec)
    }

  def mainProgram =
    "@main" ~> "{" ~> initDec.* ~ command.* <~ "}" ^^ {
      case decs ~ commands => MainProgram(decs, commands)
    }

  def initDec =
    identifier ~ ("=" ~> (signal | signalList)) ^^ {
      case target ~ (sig: Signal) => InitSingleDeclaration(target, sig)
      case target ~ (sigs: SignalList) => InitMultDeclaration(target, sigs)
    }

    def signalList =
    ("[" ~> repsep(signal, ",") <~ "]") ~ ("*" ~> integer).? ^^ {
      case sigs ~ Some(num) => SignalList(sigs, num)
      case sigs ~ None => SignalList(sigs)
    }

  def signal =
    (("bool" ~ ("(" ~> integer <~ ")")) ^^ {
      case hdltype ~ first => Signal(hdltype, first)
    }) | (("signed"| "unsigned") ~ ("(" ~> integer ~ ("," ~> integer).? <~ ")") ^^ {
      case hdltype ~ (first ~ second) => Signal(hdltype, first, second)
    })

  def command = "compile" ~> "(" ~> moduleCall ~ ("," ~> string <~ ")") ^^ {
    case modCall ~ fileName => CompileCommand(modCall, fileName)
  }

  def moduleCall = hdlidentifier ~ ("(" ~> repsep(identifier, ",") <~ ")") ^^ {
    case idt ~ (params: List[String]) => ModuleCall(idt, params)
  }

  def module =
    positioned("defmod" ~> identifier ~ ("(" ~> repsep(hdlidentifier, ",") <~ ")") ~
      ("{" ~> block.* <~ "}") ^^ {
        case moduleName ~ params ~ blocks => Module(moduleName, params, blocks)
      })

  def condition =
    "@" ~>identifier ~ ("(" ~> integer.* <~ ")") ^^ {
      case condName ~ params => Condition(condName, params)
    }

  def block =
    positioned(("@{" ~> (codeBlock(0).*) <~ "}") ^^ {
      case code => ScalaBlock(code.mkString)
    } |
      condition.? ~ identifier ~ ("{" ~> statement.* <~ "}") ^^ {
      case condition ~ blockName ~ statements => HDLBlock(condition, blockName, statements)
    })

  def codeBlock(i: Int): Parser[String] =
    ("{" ~ codeBlock(i+1).* ~ "}") ^^ {
      case lb ~ s ~ rb => lb + s.mkString + rb
    } |
    (not("{") ~> anyBut('}')+) ~ codeBlock(i+1).* ^^ {
      case lst ~ cb => lst.mkString + cb.mkString
    }

  def statement =
    positioned(("@{" ~> (codeBlock(0).*) <~ "}") ^^ {
      case code => ScalaStatement(code.mkString)
    } | hdlidentifier ~ (":=" ~> expression) ^^ {
      case left ~ right => AssignStatement(left, right)
    })

  def expression =
    ((value ~ ("""[\+\-\*\/\%\&\|\^]""".r) ~ value) ^^ {
      case left ~ op ~ right => BinaryExpression(op, left, right)
    }) | (("""[\~\!]""".r ~ value) ^^ {
      case op ~ target => UnaryExpression(op, target)
    })

  def apply(input: String): Option[Program] = parseAll(
    program, new CharArrayReader(input.toArray)) match {
    case Success(pro, _) => Some(pro)
    case x => {
      println(x)
      None
    }
  }
}

object SymbolType extends Enumeration {
  type SymbolType = Value
  val SignalType, SignalListType = Value
}
import SymbolType._

private object SymbolTable {
  import scala.collection.mutable.HashMap

  val moduleTable: HashMap[String, HashMap[String, SymbolType]] =
    new HashMap[String, HashMap[String, SymbolType]]
  val mainProgramTable: HashMap[String, List[String]] =
    new HashMap[String, List[String]]

  def registerOnModule(modName: String, idt: String, tpe: SymbolType) {
    if (!moduleTable.contains(modName)) {
      moduleTable(modName) = new HashMap[String, SymbolType]
    }
    moduleTable(modName)(idt) = tpe
  }

  def clear {
    moduleTable.clear
    mainProgramTable.clear
  }
}

case class Program(name: String, mods: List[Module],
  main: MainProgram, packageDec: Option[String] = None)
    extends Statement {
  override def generate = {
    SymbolTable.clear
    List((packageDec match {
      case Some(name) => s"package $name"
      case None => ""
    }),"import ScalaHDL.Core.ScalaHDL\nimport ScalaHDL.Core.HDLType\nimport ScalaHDL.Core.DataType.Signals._",
      s"trait $name extends ScalaHDL {",
      mods.map(_.generate).mkString("\n"), "}", s"object Main extends $name {",
      main.generate, "}").mkString("\n")
  }
}

case class MainProgram(decs: List[InitDeclaration], commands: List[Command])
    extends Statement {
  override def generate = "\ndef main(args: Array[String]) {\n" + List.flatten(
    List(decs.map(_.generate), commands.map(_.generate))).mkString("\n") + "\n}"
}

case class Signal(hdltype: String, first: Int, second: Option[Int] = None)
    extends HDLValue {
  override def generate = List(hdltype, "(", first, (second match {
    case Some(s) => ", " + s
    case None => ""
  }), ")").mkString
  override def flatten = List(this)
}

case class SignalList(sigs: List[Signal], rep: Int = 1) extends HDLValue {
  def size = sigs.size * rep
  def allSignals = sigs
  override def generate = "List(" + sigs.map(_.generate).mkString(", ") + ")"
  override def flatten: List[HDLValue] = sigs
}

case class InitSingleDeclaration(idt: String, sig: Signal) extends InitDeclaration {
  override def generate = {
    SymbolTable.mainProgramTable(idt) = List(idt)
    List("val ", idt, " = ", sig.generate).mkString("")
  }

  def identifier = idt
}

case class InitMultDeclaration(idt: String, sigs: SignalList) extends InitDeclaration {
  override def generate = {
    val s = generateMultiple
    s.map((e: InitSingleDeclaration) =>
      if (!SymbolTable.mainProgramTable.contains(idt)) {
        SymbolTable.mainProgramTable(idt) = List(e.identifier)
      } else {
      SymbolTable.mainProgramTable(idt) =
        e.identifier :: SymbolTable.mainProgramTable(idt)
      })
    s.map(_.generate).mkString("\n")
  }
  def generateMultiple =
    for (i <- 0 until sigs.size) yield
      InitSingleDeclaration(idt + i, sigs.allSignals(i % sigs.allSignals.size))
}

abstract trait InitDeclaration extends Statement

case class ModuleCall(moduleName: Identifier, params: List[String])
    extends Statement {
  override def generate = moduleName.toString + ", " + params.flatMap(
    SymbolTable.mainProgramTable(_).reverse).mkString(", ")
}

case class CompileCommand(moduleCall: ModuleCall, fileName: String)
    extends Command {
  override def generate =
    s"import java.io._\nval writer = new PrintWriter(new File($fileName))\n" +
      "writer.write(convert(" + moduleCall.generate + "))\nwriter.close()\n"
}

case class Identifier(symbol: String, size: Option[Int]) extends HDLValue {
  override def generate: String = symbol
  override def toString = "'" + symbol
  override def flatten = List(generateMultiple: _*)


  def registerOnModule(modName: String) {
    size match {
      case Some(num) =>
        SymbolTable.registerOnModule(modName, symbol, SignalListType)
      case None =>
        SymbolTable.registerOnModule(modName, symbol, SignalType)
    }
  }

  def generateMultiple: Seq[Identifier] = size match {
    case Some(num) =>
      for (i <- 0 until num) yield Identifier(symbol + i, None)
    case None => Seq(this)
  }
}

case class HDLInt(value: Int) extends HDLValue {
  override def generate: String = toString
  override def flatten = List(this)
}

abstract trait HDLValue extends Statement {
  def flatten: List[HDLValue]
}

case class Module(name: String, params: List[Identifier], blocks: List[Block])
    extends Statement {
  override def generate: String = {
    params.map(_.registerOnModule(name))
    val ps = params.flatMap(_.generateMultiple)
    List(
      s"defMod.$name(" + ps.mkString(",") + ") {",
      params.map(i => SymbolTable.moduleTable(name)(i.generate) match {
        case SignalType => "val " + i.generate + " = toHDLType(" + i + ")"
        case SignalListType => "val " + i.generate + " = List(" +
          i.generateMultiple.mkString(", ") + ").map(toHDLType)"
      }).mkString("\n"),
      blocks.map(_.generate).mkString("\n"),
      "}").mkString("\n")
  }
}

case class HDLBlock(cond: Option[Condition], name: String, statements: List[Statement])
    extends Block {
  override def generate: String = List((cond match {
      case Some(condition) => condition.generate
      case _ => ""
    }) + s"$name {",
    statements.map(_.generate).mkString("\n"),
    "}").mkString("\n")
}

case class ScalaBlock(code: String) extends Block {
  override def generate: String = code
}

abstract trait Block extends Statement

case class Condition(cond: String, params: List[Int]) extends Statement {
  override def generate: String = cond + "(" + params.mkString(",") + ")."
}

case class AssignStatement(left: Identifier, right: Expression) extends Statement {
  override def generate: String = left.generate + ":=" + right.generate
}

case class BinaryExpression(operator: String, left: HDLValue, right: HDLValue)
    extends Expression {
  override def generate: String = left.generate + operator + right.generate
}

case class UnaryExpression(operator: String, target: HDLValue) extends Expression {
  override def generate: String = operator + target.generate
}

case class ScalaStatement(code: String) extends Statement {
  override def generate: String = code
}

abstract trait Command extends Statement

abstract trait Expression extends Statement

abstract trait Statement extends Positional {
  def generate: String
}
