package hocki.klocki.pipeline

import hocki.klocki.analysis.resolveNames
import hocki.klocki.ast.Toplevel
import hocki.klocki.parsing.DflParser
import hocki.klocki.typing.{SchemaTy, inferTypes, instantiateSchemata}
import hocki.klocki.visualize.graph.{buildProgram, programToJson}
import hocki.klocki.visualize.presentTyping

import java.io.{BufferedWriter, FileWriter}
import scala.io.Source.fromFile

def runPipeline
(
  filename: String,
  outputFilename: String,
  expansionDepth: Int,
  typingFilename: Option[String],
): Boolean =
  load(filename) match
    case Some(tree) =>
      val names =
        try resolveNames(tree)
        catch case e: Exception =>
          println(s"Name resolution error: ${e}")
          e.printStackTrace()
          return false

      val program = buildProgram(tree, names)
      writeToFile(programToJson(program), outputFilename)

      typingFilename match
        case Some(filename) =>
          val typing =
            try 
              val schemata = instantiateSchemata(tree, names)
              inferTypes(schemata, names.primitives)
            catch
              case e: StackOverflowError =>
                writeToFile("typing poszedł w buraki", filename)
                return false
              case e: Exception =>
                println(e.getMessage)
                e.printStackTrace()
                writeToFile(s"<typing error> ${e}", filename)
                return false
          val typingPresentation = presentTyping(typing)
          writeToFile(typingPresentation, filename)
        case None => ()
      true
    case None => false

private def load(filename: String): Option[Toplevel] =
  val source = fromFile(filename)
  val code =
    try source.mkString
    finally source.close()
  DflParser.parseAll(DflParser.program, withoutComments(code)) match
    case DflParser.Success(result, _) => Some(result)
    case DflParser.Failure(msg, input) =>
      println(msg)
      println(input.pos.longString)
      None
    case DflParser.Error(_, _) =>
      throw RuntimeException("Something very sad happened")

private def withoutComments(code: String) = code.replaceAll("#[^\n]*", "")

private def writeToFile(content: String, filename: String): Unit =
  val writer = BufferedWriter(new FileWriter(filename))
  writer.write(content)
  writer.close()
