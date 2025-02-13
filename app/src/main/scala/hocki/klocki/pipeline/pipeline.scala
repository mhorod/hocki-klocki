package hocki.klocki.pipeline

import hocki.klocki.analysis.resolveNames
import hocki.klocki.ast.{SchemaBinding, Toplevel}
import hocki.klocki.parsing.DflParser
import hocki.klocki.semantics.graphs.buildGraph
import hocki.klocki.typing.{SchemaTy, inferTypes}
import hocki.klocki.utils.printTree
import hocki.klocki.visualize.{presentTyping, schemataToGraphviz}

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
          println("Name resolution error")
          return false

      val graph = buildGraph(tree, names)
      val graphviz = schemataToGraphviz(graph, expansionDepth)
      writeToFile(graphviz, outputFilename)

      typingFilename match
        case Some(filename) =>
          val typing =
            try inferTypes(tree, names)
            catch
              case e: StackOverflowError =>
                writeToFile("infinite recursion (typing poszedł w buraki)", filename)
                return false
              case e: Exception =>
                println(e)
                writeToFile(e.getMessage, filename)
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
