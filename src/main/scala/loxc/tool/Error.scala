package loxc.tool

object Error {

  // Simply log the error with line num, where and message into stdout.
  def reportConsole(numOfLine: Int, where: AnyRef, message: String): Unit = println(format(numOfLine, where, message))

  def format(numOfLine: Int, where: AnyRef, message: String): String =
    s"[Line $numOfLine] [Error] ${where.getClass.getCanonicalName}: $message"
}

final case class ScannerException(private val message: String, private val cause: Throwable = None.orNull)
    extends Exception(message, cause)
