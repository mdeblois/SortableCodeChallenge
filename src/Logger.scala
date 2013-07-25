import java.io._
// singleton object to write logs to file
object Logger {
  val logWriter = new PrintWriter(new File("log.txt"))
  def log(log: String): Unit = {
    logWriter.write(log + "\n")
    logWriter.flush()
  }
}