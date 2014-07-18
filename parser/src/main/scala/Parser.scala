package ScalaHDL.Parser

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.Positional
import scala.util.parsing.input.CharArrayReader

class ScalaHDLParser extends RegexParsers {

  def identifier = """[_\p{L}][_\p{L}\p{Nd}]*""".r

  def hdlidentifier = """[_\p{L}][_\p{L}\p{Nd}]*""".r ^^ {
    case symbol => Identifier(symbol)
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
    identifier ~ ("=" ~> signal) ^^ {
      case target ~ sig => InitDeclaration(target, sig)
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
    case idt ~ params => ModuleCall(idt, params)
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
    positioned(condition.? ~ identifier ~ ("{" ~> statement.* <~ "}") ^^ {
      case condition ~ blockName ~ statements => Block(condition, blockName, statements)
    })

  def codeBlock(i: Int): Parser[String] =
    ("{" ~ codeBlock(i+1) ~ "}") ^^ {
      case lb ~ s ~ rb => lb + s + rb
    } |
    (not("{") ~> anyBut('}')+) ^^ { _.mkString }

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

case class Program(name: String, mods: List[Module], main: MainProgram, packageDec: Option[String] = None)
    extends Statement {
  override def generate = List((packageDec match {
    case Some(name) => s"package $name"
    case None => ""
  }),"import ScalaHDL.Core.ScalaHDL\nimport ScalaHDL.Core.DataType.Signals._",
    s"trait $name extends ScalaHDL {",
    mods.map(_.generate).mkString("\n"), "}", s"object Main extends $name {",
    main.generate, "}").mkString("\n")
}

case class MainProgram(decs: List[InitDeclaration], commands: List[Command])
    extends Statement {
  override def generate = "\ndef main(args: Array[String]) {\n" + List.flatten(
    List(decs.map(_.generate), commands.map(_.generate))).mkString("\n") + "\n}"
}

case class Signal(hdltype: String, first: Int, second: Option[Int] = None)
    extends Statement {
  override def generate = List(hdltype, "(", first, (second match {
    case Some(s) => ", " + s
    case None => ""
  }), ")").mkString
}

case class InitDeclaration(idt: String, sig: Signal) extends Statement {
  override def generate = List("val ", idt, " = ", sig.generate).mkString("")
}

case class ModuleCall(moduleName: Identifier, params: List[String])
    extends Statement {
  override def generate = moduleName.toString + ", " + params.mkString(", ")
}

case class CompileCommand(moduleCall: ModuleCall, fileName: String)
    extends Command {
  override def generate = s"import java.io._\nval writer = new PrintWriter(new File($fileName))\n" +
  "writer.write(convert(" + moduleCall.generate + "))\nwriter.close()\n"
}

case class Identifier(symbol: String) extends Value {
  override def generate: String = symbol
  override def toString = "'" + symbol
}

case class HDLInt(value: Int) extends Value {
  override def generate: String = toString
}

abstract trait Value extends Statement

case class Module(name: String, params: List[Identifier], blocks: List[Block])
    extends Statement {
  override def generate: String = List(s"defMod.$name(" + params.mkString(",") + ") {",
    params.map(i => "val " + i.generate + " = " + i).mkString("\n"),
    blocks.map(_.generate).mkString("\n"),
  "}").mkString("\n")
}

case class Block(cond: Option[Condition], name: String, statements: List[Statement])
    extends Statement {
  override def generate: String = List((cond match {
      case Some(condition) => condition.generate
      case _ => ""
    }) + s"$name {",
    statements.map(_.generate).mkString("\n"),
    "}").mkString("\n")
}

case class Condition(cond: String, params: List[Int]) extends Statement {
  override def generate: String = cond + "(" + params.mkString(",") + ")."
}

case class AssignStatement(left: Identifier, right: Expression) extends Statement {
  override def generate: String = left.generate + ":=" + right.generate
}

case class BinaryExpression(operator: String, left: Value, right: Value)
    extends Expression {
  override def generate: String = left.generate + operator + right.generate
}

case class UnaryExpression(operator: String, target: Value) extends Expression {
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
