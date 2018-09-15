package loxc

object Error {
  // Simply log the error with line num, where and message into stdout.
  def reportConsole(numOfLine: Int, where: String = "", message: String): Unit =
    println(s"[line $numOfLine] Error $where: $message")
}

final case class ScannerException(private val message: String, private val cause: Throwable = None.orNull)
    extends Exception(message, cause)
