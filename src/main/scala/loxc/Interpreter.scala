package loxc

import java.io.{BufferedReader, InputStreamReader, OutputStreamWriter}
import java.nio.charset.Charset
import java.nio.file.{Files, Paths}

object Interpreter {

  def main(args: Array[String]): Unit = {
    // Specify file path or using a REPL in single line.
    if (args.length > 1) {
      Console.err.println("Usage: loxc [script]")
      sys.exit(1)
    } else if (args.length == 1) {
      evalFile(args(0))
    } else {
      evalRepl()
    }

  }

  private def evalFile(path: String): Unit = {
    val bytes = Files.readAllBytes(Paths.get(path))
    eval(new String(bytes, Charset.defaultCharset()))
  }


  private def evalRepl() = {
    val input = new InputStreamReader(System.in)
    val reader = new BufferedReader(input)
    while (true) {
      print(">")
      eval(reader.readLine())
    }
  }

  private def eval(source: String) = {
    val scanner = Scanner(source)
  }

}
