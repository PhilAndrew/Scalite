package scalite

import com.moandjiezana.toml.Toml

object PreProcessScaliteFile {

  val TOML = "toml"
  val ANSIBLE = "ansible"

  val macros = Seq(TOML, ANSIBLE)

  def findMacroLines(txt: String, lines: List[String]): Map[String,Int] = {
    val m: List[(String,Int)] = macros.map( (m: String) => {

      // @todo can make efficient here
      val withMacro: List[(String,Int)] = lines.zipWithIndex.map( (l: (String, Int)) => {
        val i = l._1.indexOf("= %" + m)
        if (i >= 0)
          Some( m -> l._2 )
        else
          None
      }).filter(_.isDefined).map(_.get)

      withMacro
    }).flatten.toList
    m.toMap
  }

  def indentOfLine(s: String): Int = {
    if (s.trim.size == 0) -1 else
      s.takeWhile(_ == ' ').size
  }

  def linesWhileIndented(lines: Seq[String], startIndex: Int, startIndent: Int): Seq[String] = {
    lines.drop(startIndex+1).takeWhile { (line: String) =>
    {
      val i = indentOfLine(line)
      (i > startIndent) || (i < 0)
    }
    }
  }

  def reduceIndent(lines: Seq[String], startIndent: Int): Seq[String] = {
    // +2 due to the assumption that this code is 2 space indented
    lines.map(_.drop(startIndent+2))
  }

  def process(txt: String): String = {

    // Look for "= %toml" then all indented lines from here onwards are TOML language
    val NL = System.lineSeparator()

    val lines: List[String] = txt.split('\n').toList
    val macroLines = findMacroLines(txt, lines)

    def decUntilZero(i:Int):Int = {
      val result:Int = i - 1
      if (result < 0) 0 else result
    }

    var result = List[String]()
    var ignoreLinesCount = 0
    for ((line:String,index:Int) <- lines.zipWithIndex) {

      ignoreLinesCount = decUntilZero(ignoreLinesCount)
      val notIgnoreLines = ignoreLinesCount == 0

      if (notIgnoreLines) {
        val macroOnThisLine = macroLines.find { case (a: String, b: Int) => {
            b == index
          }
        }
        if (macroOnThisLine.isDefined) {
          val macroType = macroOnThisLine.get._1
          val startIndex = macroOnThisLine.get._2
          val startTxt = lines(startIndex)
          val startIndent = indentOfLine(startTxt)
          val linesIndented = linesWhileIndented(lines.toSeq, startIndex, startIndent)
          val macroLines = reduceIndent(linesIndented, startIndent)

          ignoreLinesCount = linesIndented.size + 1
        } else
          result = line :: result
      } else
        result = "//" + line :: result
    }

    //result.reverse.map( (d:String) => println(":!:" + d))
    result.reverse.mkString(NL)
        /**/


      /*val macroType = line._1
      val startIndex = line._2
      val startTxt = lines(startIndex)
      val startIndent = indentOfLine(startTxt)
      val linesIndented = linesWhileIndented(lines.toSeq, startIndex, startIndent)
      val macroLines = reduceIndent(linesIndented, startIndent)*/



      /*
      val resultLines: List[String] = macroLines.map( (line: (String, Int)) => {
        val macroType = line._1
        val startIndex = line._2
        val startTxt = lines(startIndex)
        val startIndent = indentOfLine(startTxt)
        val linesIndented = linesWhileIndented(lines.toSeq, startIndex, startIndent)
        val macroLines = reduceIndent(linesIndented, startIndent)

        val linesWithIndex = lines.zipWithIndex
        val endIndex = startIndex + linesIndented.size-1

        val indentSpace = (0 until startIndent).map(_ => " ").mkString("")
        val tomlVal = indentSpace + "val tomlString = \"\"\"" + NL +
          macroLines.map(indentSpace + "|" + _).mkString(NL) + "\"\"\".stripMargin" + NL
        // Emit toml
        val tomlString = tomlVal +
          indentSpace + "val toml = new Toml().parse(tomlString)" + NL

        val tomlLines: List[String] = tomlString.split('\n').toList

        println("Total lines " + tomlLines.size)

        val result = linesWithIndex.map {
          (l) => {
            l match {
              case (line: String, i: Int) => {
                // @todo Simplify
                val result: List[String] = if ((i >= startIndex) && (i <= endIndex)) {
                  val commentLine = "//" + line
                  if (i < endIndex)
                    List(commentLine)
                  else if (i == endIndex)
                    List(commentLine) ::: tomlLines
                  else
                    List(line)
                } else List(line)
                result
              }
            }
          }
        }.flatten.toSeq

        result.map((s:String) => println(":" + s))

        result

        //for (l <- macroLines) println("DEBUG:" + l.toString)

        // Now we either
        // 1. Generate valid Scala code which works in normal Scala
        // 2. Generate Scala code which uses an existing library, eg, Toml parser
        // 3. ????
      }).flatten.toList
      */

    /*    val p = new Toml()
        val toml = p.parse(txt)
        val someValue = toml.getString("someKey")
        val someDate = toml.getDate("someTable.someDate")
        //val myClass = toml.to(classOf[MyClass])*/

  }
}
