package scalite

import scala.tools.nsc.plugins.{Plugin, PluginComponent}
import scala.tools.nsc.util.BatchSourceFile
import scala.tools.nsc.{Global, Phase, _}
class ScalitePlugin(val global: Global) extends Plugin {


  override def init(options: List[String],  error: String => Unit): Boolean = true

  val name = "demo"
  val description = "a plugin"
  val components = List[PluginComponent](DemoComponent)
  object sal extends {override val global = ScalitePlugin.this.global} with SyntaxAnalyzerLite {

    override val runsAfter: List[String] = Nil
    override val runsRightAfter: Option[String] = None
  }
  private object DemoComponent extends PluginComponent {

    val global = ScalitePlugin.this.global
    import global._

    override val runsAfter = List("parser")
    override val runsRightAfter = Some("parser")
    override val runsBefore = List("namer")

    val phaseName = "Scalite"

    override def newPhase(prev: Phase) = new StdPhase(prev){

      def apply(unit: global.CompilationUnit): Unit = {
        
        if (unit.source.file.name.endsWith(".scalite")){

          //val a = unit.source.content.mkString
          //val b = unit.source.content.mkString.replace("\n//", "\n").drop(2)
          //import java.nio.file.{Paths, Files}
          //import java.nio.charset.StandardCharsets
          //Files.write(Paths.get("\\home\\a.txt"), a.getBytes(StandardCharsets.UTF_8))
          //Files.write(Paths.get("\\home\\b.txt"), b.getBytes(StandardCharsets.UTF_8))

          val scaliteText = unit.source.content.mkString.replace("\n//", "\n").drop(2)
          val txt = PreProcessScaliteFile.process(scaliteText)
          //.replace("\"ScalaJS:\"", "\"XXX:\"")

          val singleFile = new io.VirtualFile(unit.source.file.name)
          val output = singleFile.output
          output.write(txt.getBytes)
          output.close()

          val sourceFile = new BatchSourceFile(singleFile, txt)

          unit.body = new sal.UnitParserX(
            new CompilationUnit(sourceFile).asInstanceOf[sal.global.CompilationUnit]
          ).compilationUnit()
            .asInstanceOf[DemoComponent.this.global.Tree]
        }
      }
    }
  }
}
