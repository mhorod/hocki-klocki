package hocki.klocki.pipeline

import hocki.klocki.ast.Toplevel
import hocki.klocki.parsing.DflParser

import scala.io.Source.fromFile

def runPipeline(filename: String): Boolean =
  ???

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