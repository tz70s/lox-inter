package loxc.ast

import loxc.Scanner
import org.scalatest.{FlatSpec, Matchers}

class ParserSpec extends FlatSpec with Matchers {

  behavior of "Parser"

  it should "parse simple arithmetic expression into ast" in {
    val text = "1 + 2 * 3"
    val expr = Parser.parse(Scanner.tokenize(text))
    AstConsole.debug(expr)
  }

  it should "parse simple grouping arithmetic expression into ast" in {
    val text = "1 + (2 * 3)"
    val expr = Parser.parse(Scanner.tokenize(text))
    AstConsole.debug(expr)
  }

  it should "parse arithmetic expression into left-associative ast" in {
    val text = "7 - 3 - 2"
    val expr = Parser.parse(Scanner.tokenize(text))
    AstConsole.debug(expr)
  }

  it should "parse slightly more complex expression into ast" in {
    val text = "7 * 3 + 2 * 5 - 4 + 1"
    val expr = Parser.parse(Scanner.tokenize(text))
    AstConsole.debug(expr)
  }
}
