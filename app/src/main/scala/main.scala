import hocki.klocki.pipeline.runPipeline

@main
def main(filename: String): Unit =
  try
    val msg = if runPipeline(filename) then "OK" else "FAIL"
    println(msg)
  catch
    case e: Exception => println(s"TRAGEDY: $e")


@main
def funnyExample(): Unit = main("examples/nested.dfl")