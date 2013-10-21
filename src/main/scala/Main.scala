package ScalaHDL

object Main extends DSL { 
  def main(args: Array[String]) { 
    val t = module.add('a, 'b, 'res) { 
      'res := 'a + 'b
    }
    println(t)
    convert('add)
  }
}
