package hocki.klocki.pipeline

import hocki.klocki.analysis.resolveNames
import hocki.klocki.ast.Toplevel
import hocki.klocki.parsing.DflParser
import hocki.klocki.semantics.graphs.buildGraph
import hocki.klocki.utils.printTree

import scala.io.Source.fromFile

def runPipeline(filename: String): Boolean =
  load(filename) match
    case Some(tree) =>
      printTree(tree)
      val names = resolveNames(tree)
      val graph = buildGraph(tree, names)
      true
    case None => false

private def load(filename: String): Option[Toplevel] =
  val source = fromFile(filename)
  val code =
    try source.mkString
    finally source.close()
  DflParser.parseAll(DflParser.program, code) match
    case DflParser.Success(result, _) => Some(result)
    case DflParser.Failure(msg, input) =>
      println(msg)
      println(input.pos.longString)
      None
    case DflParser.Error(_, _) =>
      throw RuntimeException("Something very sad happened")