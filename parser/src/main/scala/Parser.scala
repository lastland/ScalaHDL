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

  def value = (hdlidentifier) | (integer ^^ { HDLInt(_) })

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

  def statement =
    positioned(hdlidentifier ~ (":=" ~> expression) ^^ {
      case left ~ right => AssignStatement(left, right)
    })

  def expression =
    ((value ~ ("""[\+\-\*\/\%\&\|\^]""".r) ~ value) ^^ {
      case left ~ op ~ right => BinaryExpression(op, left, right)
    }) | (("""[\~\!]""".r ~ value) ^^ {
      case op ~ target => UnaryExpression(op, target)
    })

  def apply(input: String): Option[Module] = parseAll(
    module, new CharArrayReader(input.toArray)) match {
    case Success(mod, _) => Some(mod)
    case x => None
  }
}

case class Identifier(symbol: String) extends Value {
  override def generate: String = "'" + symbol
}

case class HDLInt(value: Int) extends Value {
  override def generate: String = toString
}

abstract trait Value extends Statement

case class Module(name: String, params: List[Identifier], blocks: List[Block])
    extends Statement {
  override def generate: String = List(s"trait $name extends ScalaHDL {",
    s"defMod.$name(" + params.map(_.generate).mkString(",") + ") {",
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

case class BinaryExpression(operator: String, left: Value, right: Value) extends Expression {
  override def generate: String = left.generate + operator + right.generate
}

case class UnaryExpression(operator: String, target: Value) extends Expression {
  override def generate: String = operator + target.generate
}

abstract trait Expression extends Statement

abstract trait Statement extends Positional {
  def generate: String
}
