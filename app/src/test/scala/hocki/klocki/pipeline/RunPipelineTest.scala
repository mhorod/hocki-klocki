package hocki.klocki.pipeline

import hocki.klocki.pipeline.RunPipelineTest.runExamplesForResults
import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.{Files, Path}
import scala.util.Using
import scala.jdk.StreamConverters.*

class RunPipelineTest extends AnyFunSuite:

  test("correct examples can be typed") {
    runExamplesForResults("examples/correct").foreach {
      case (name, isRight) => assert(isRight, name)
    }
  }

  test("incorrect examples cannot be typed") {
    runExamplesForResults("examples/incorrect").foreach {
      case (name, isRight) => assert(!isRight, name)
    }
  }

private object RunPipelineTest:
  private def runExamplesForResults(dirname: String): List[(String, Boolean)] =
    Using(Files.list(Path.of(dirname))) { stream =>
      stream.toScala(List).map { path =>
        val name = path.toString
        val result = runPipeline(name)
        (name, result.isRight)
      }
    }.get
