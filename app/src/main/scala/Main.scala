import hocki.klocki.pipeline.runPipeline

object Main:
  def main(args: Array[String]): Unit =
    if args.length < 2 then
      println("App requires two argument {source filename} {target filename}")
      return

    val filename = args(0)
    val outputFilename = args(1)
    try
      val msg = if runPipeline(filename, outputFilename) then "OK" else "FAIL"
      println(msg)
    catch
      case e: Exception => {
        println(s"TRAGEDY: $e")
        e.printStackTrace()
      }