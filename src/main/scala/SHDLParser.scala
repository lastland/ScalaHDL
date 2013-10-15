package ScalaHDL

import scala.util.parsing.combinator.syntactical._

class SHDLParser extends StandardTokenParsers { 

  lexical.reserved ++= List("def", "main", "Signal", "Int", "convert")
  lexical.delimiters ++= List("(", ")", ",", "{", "}", "=", "+", "-", "*", "/")

  def module: Parser[(String, HDLModule)] = 
    "def" ~> moduleName ~ params ~ block ^^ { 
      case moduleName ~ params ~ block => { 
	val m = HDLModule(params, block)
	(moduleName, m)
      }
    }

  def moduleName: Parser[String] = ident
  
  def params: Parser[List[String]] = 
    "(" ~> rep1sep(ident, ",") <~ ")" 

  def block: Parser[HDLBlock] = 
    "{" ~> rep1(stmt) <~ "}" ^^ { 
      case stmts => HDLBlock(stmts)
    }
  
  def stmt: Parser[HDLStatement] =
    ident ~ "=" ~ ident ~ "+" ~ ident ^^ { 
      case a ~ b ~ c ~ d ~ e => HDLStatement(List(a, b, c, d, e).mkString(""))
    }

  def mainModule: Parser[HDLMainModule] = 
    ("def" ~ "main" ~ "(" ~ ")" ~ "{") ~> rep1(assignment) ~ convert <~"}" ^^ { 
      case assignments ~ convert => {
	new HDLMainModule(assignments, convert)
      }
    }

  def assignment: Parser[HDLAssignment] = 
    ident ~ "=" ~ decl ^^ { 
      case ident ~ "=" ~ decl => HDLAssignment(ident, decl)
    }

  def decl: Parser[HDLType] =
    ("Signal" ~ "(") ~> tpe ~ "," ~ numericLit <~ ")" ^^ { 
      case tpe ~ "," ~ num => HDLType(tpe, num.toInt)
    }

  import HDLType_._
  def tpe: Parser[HDLType_] = "Int" ^^ {
    case "Int" => int
  }

  def convert: Parser[HDLConvert] =
    ("convert" ~ "(") ~> ident ~ "," ~ rep1sep(ident, ",") <~ ")" ^^ { 
      case moduleName ~ "," ~ idents => HDLConvert(moduleName, idents)
    }

  def shdlprogram: Parser[HDLProgram] =
    rep1(module) ~ mainModule ^^ { 
      case ms ~ mm => { 
	val hp = new HDLProgram(mm)
	ms.foreach(mod => hp.modules += mod)
	hp
      }
    }

  def run(text: String) = { 
    var fn = shdlprogram
    fn(new lexical.Scanner(text))
  }
}
