package loxc
import loxc.Tokens._
import org.scalatest.{FlatSpec, Matchers}

class ScannerSpec extends FlatSpec with Matchers {

  // Helper method to match with omitted line.
  implicit class ExpectedTokens(completeTokens: List[CompleteToken]) {
    def expectTokens(target: List[Token]): Unit =
      completeTokens.map(c => c.token).zip(target).foreach {
        case (s: Token, t: Token) => s should be(t)
      }
  }

  behavior of "Scanner"

  it should "lex single tokens correctly" in {
    val braces = "( )"
    val scanner = Scanner(braces)
    scanner.tokens expectTokens List(LeftParen, RightParen, EOF)
  }
}
