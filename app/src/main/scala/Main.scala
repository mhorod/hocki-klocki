import hocki.klocki.pipeline.execPipeline

object Main:
  def main(args: Array[String]): Unit =
    if args.length < 2 then
      println("App requires two argument {source filename} {target filename}")
      return

    println(args.mkString("Array(", ", ", ")"))

    val filename = args(0)
    val outputFilename = args(1)
    val _expansionDepth = if args.length > 2 then args(2).toInt else 0
    val typingFilename = if args.length > 3 then Some(args(3)) else None

    try
      val msg = if execPipeline(filename, outputFilename, typingFilename) then "OK" else "FAIL"
      println(msg)
    catch
      case e: Exception => {
        println(s"TRAGEDY: $e")
        e.printStackTrace()
      }

