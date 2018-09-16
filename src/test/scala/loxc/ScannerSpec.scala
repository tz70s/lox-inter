package loxc

import loxc.Tokens._
import loxc.tool.LazyLog
import org.scalatest.{FlatSpec, Matchers}

class ScannerSpec extends FlatSpec with Matchers with LazyLog {

  // Helper method to match with omitted line.
  implicit class ExpectedTokens(completeTokens: List[CompleteToken]) {
    def expectTokens(target: List[Token]): Unit =
      completeTokens.map(c => c.token).zip(target).foreach {
        case (s: Token, t: Token) => s should be(t)
      }
  }

  behavior of "Scanner"

  it should "lex single character tokens correctly" in {
    val braces = "( )"
    val tokens = Scanner.tokenize(braces)
    tokens expectTokens List(LeftParen, RightParen, EOF)
  }

  it should "lex multi characters tokens correctly" in {
    val expr =
      raw"""
      | class TestClass(name, age) {
      | fun hello() = print "name"
      | }
      """.stripMargin
    val expected = List(Clazz,
                        Ident,
                        LeftParen,
                        Ident,
                        Comma,
                        Ident,
                        RightParen,
                        LeftBrace,
                        Fun,
                        Ident,
                        LeftParen,
                        RightParen,
                        Equal,
                        Print,
                        StringLiter,
                        RightBrace,
                        EOF)

    val tokens = Scanner.tokenize(expr)
    tokens expectTokens expected
  }

  it should "lex string literal correctly" in {
    val expr = "let name = \"Jon Snow\""
    val tokens = Scanner.tokenize(expr)
    tokens expectTokens List(Let, Ident, Equal, StringLiter, EOF)
    tokens.drop(3).head should be(CompleteToken(StringLiter, 1, StringLiteral("Jon Snow")))
  }

  it should "lex number tokens correctly" in {
    val expr = "let number = 2.56"
    val expected = List(Let, Ident, Equal, Number, EOF)

    val tokens = Scanner.tokenize(expr)
    tokens expectTokens expected
    tokens.drop(3).head should be(CompleteToken(Number, 1, NumberLiteral(2.56)))
  }
}
