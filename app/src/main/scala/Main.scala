import hocki.klocki.pipeline.runPipeline

object Main:
  def main(args: Array[String]): Unit =
    if args.length < 2 then
      println("App requires two argument {source filename} {target filename}")
      return

    println(args.mkString("Array(", ", ", ")"))

    val filename = args(0)
    val outputFilename = args(1)
    val expansionDepth = if args.length > 2 then args(2).toInt else 0
    val showTyping = if args.length > 3 then args(3).toBoolean else false

    try
      val msg = if runPipeline(filename, outputFilename, expansionDepth, showTyping) then "OK" else "FAIL"
      println(msg)
    catch
      case e: Exception => {
        println(s"TRAGEDY: $e")
        e.printStackTrace()
      }