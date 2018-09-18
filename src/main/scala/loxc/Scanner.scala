package loxc

import loxc.Tokens.CompleteToken

import scala.collection.mutable.ArrayBuffer
import tool.Error

object Scanner {

  def tokenize(source: String): Array[CompleteToken] = Scanner(source).tokens

  private def apply(source: String): Scanner = new Scanner(source)
}

/** For performance reason, use mutable-based implementation here. */
class Scanner(private val source: String) {
  import Tokens._

  // Since we're using single-threaded impl, all safe around; as ugly as Scala compiler.
  private var tokenStream = new ArrayBuffer[CompleteToken](50)
  private var pos = 0
  private var head = 0
  private var numOfLine = 1

  def tokens: Array[CompleteToken] = {
    while (!endOfSource) {
      pos = head
      nextToken()
    }
    // Not necessary, but for easier parsing.
    tokenStream += EOF(numOfLine)
    tokenStream.toArray
  }

  private def nextToken(): Unit =
    ahead() match {
      case s @ ('(' | ')' | '{' | '}' | ',' | '.' | '-' | '+' | '*') => addToken(singleCodec(s))
      case ' ' | '\r' | '\t' => // Do nothing.
      case '\n' => numOfLine += 1
      // For below level, use look ahead matching
      case _ => lookAheadToken()
    }

  /** Handle tokens that we should look ahead to resolve. */
  private def lookAheadToken(): Unit = {

    def lexEqualExtension(ops: Char): Unit = {
      def addOneOfVals(token: Token, tokenWithEqual: Token): Unit =
        if (peekOne('=')) { ahead(); addToken(tokenWithEqual) } else addToken(token)

      ops match {
        case '!' => addOneOfVals(Bang, BangEqual)
        case '=' => addOneOfVals(Equal, EqualEqual)
        case '<' => addOneOfVals(Less, LessEqual)
        case '>' => addOneOfVals(Greater, GreaterEqual)
      }
    }

    def lexComment(): Unit =
      if (peekOne('/')) { while (!peekOne('\n')) { ahead() } } else addToken(Slash)

    def lexString(): Unit = {
      while (!peekOne('"') && !endOfSource) {
        if (peekOne('\n')) numOfLine += 1
        ahead()
      }

      if (endOfSource) {
        Error.reportConsole(numOfLine, this, "String literal drained out the source.")
      } else {
        ahead()
        val value = source.substring(pos + 1, head - 1)
        addToken(StringLiter, value)
      }
    }

    def lexNumber(): Unit = {
      while (isNum(peekOne())) ahead()

      if (peekOne() == '.' && isNum(peekTwo())) {
        ahead()
        while (isNum(peekOne())) ahead()
      }

      addToken(Number, source.substring(pos, head))
    }

    def lexIdent(): Unit = {
      while (isAlphaNumeric(peekOne())) ahead()
      val value = source.substring(pos, head)
      // match if it's keyword
      keyWordsCodec.get(value) match {
        case Some(k) => addToken(k)
        case None => addToken(Ident, value)
      }
    }

    def isNum(c: Char) = c >= '0' && c <= '9'
    def isAlpha(c: Char) = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c == '_')
    def isAlphaNumeric(c: Char) = isAlpha(c) || isNum(c)

    source.charAt(head - 1) match {
      // Handle Operators which may concatenate with =
      case c @ ('!' | '=' | '<' | '>') => lexEqualExtension(c)
      case '/' => lexComment()
      case '"' => lexString()
      case c if isNum(c) => lexNumber()
      case c if isAlpha(c) => lexIdent()
      case _ => Error.reportConsole(numOfLine, this, "Unexpected character.")
    }
  }

  // ==========================================================================
  // Helper methods.

  private def ahead() = {
    head += 1
    source.charAt(head - 1)
  }

  private def addToken(token: Token, literal: String = "", lexeme: String = ""): Unit =
    tokenStream += token(numOfLine, literal)

  private def peekOne(expected: Char) = if (endOfSource) false else if (source.charAt(head) != expected) false else true
  private def peekOne() = if (endOfSource) '\0' else source.charAt(head)
  private def peekTwo() = if (head + 1 > source.length) '\0' else source.charAt(head + 1)

  private def endOfSource: Boolean = head >= source.length
}
