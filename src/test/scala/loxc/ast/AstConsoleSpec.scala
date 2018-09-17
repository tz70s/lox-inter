package loxc.ast

import loxc.Tokens
import loxc.Tokens.{CompleteToken, Empty, Minus, Number, NumberLiteral, Star}
import org.scalatest.{FlatSpec, Matchers}

class AstConsoleSpec extends FlatSpec with Matchers {

  behavior of "AstConsole"

  it should "ordered print the expression" in {
    // Expression = -123 * (45.67)

    val expression = Binary(
      Unary(CompleteToken(Minus, 1, Empty), Literal(Tokens.CompleteToken(Number, 1, NumberLiteral(123)))),
      CompleteToken(Star, 1, Empty),
      Grouping(Literal(CompleteToken(Number, 1, NumberLiteral(45.67))))
    )

    // TODO(tz70s): for further stabilization, should add assertion here with AstConsole.format(expression) method.
    AstConsole.debug(expression)
  }

}
