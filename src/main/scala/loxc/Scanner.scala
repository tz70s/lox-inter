package loxc

import scala.collection.mutable.ArrayBuffer

object Scanner {
  def apply(source: String): Scanner = new Scanner(source)
}

/** For performance reason, use mutable-based implementation here. */
class Scanner(private val source: String) {
  import Tokens._

  // Since we're using single-threaded impl, all safe around; as ugly as Scala compiler.
  private var tokenStream = new ArrayBuffer[CompleteToken](50)
  private var pos = 0
  private var head = 0
  private var numOfLine = 1

  def tokens: List[CompleteToken] = {
    while (!endOfSource) {
      pos = head
      nextToken()
    }
    // Not necessary, but for easier parsing.
    tokenStream += EOF(numOfLine)
    tokenStream.toList
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

    def extendEqual(ops: Char, hasEqual: Boolean) = {
      def returnVal(token: Token, tokenWithEqual: Token) = if (hasEqual) tokenWithEqual else token

      ops match {
        case '!' => returnVal(BangEqual, Bang)
        case '=' => returnVal(EqualEqual, Equal)
        case '<' => returnVal(LessEqual, Less)
        case '>' => returnVal(GreaterEqual, Greater)
      }
    }

    source.charAt(head - 1) match {
      // Handle Operators which may concatenate with =
      case ops @ ('!' | '=' | '<' | '>') =>
        val token = extendEqual(ops, peekOne('='))
        addToken(token)

      // Handle comment.
      // TODO(tz70s): maybe I should make this more readable.
      case '/' => if (peekOne('/')) { while (!peekOne('\n')) { ahead() } } else addToken(Slash)
    }
  }

  // ==========================================================================
  // Helper methods.

  private def ahead() = {
    head += 1
    source.charAt(head - 1)
  }

  private def addToken(token: Token, literal: String = "", lexeme: String = "") =
    tokenStream += token(numOfLine, literal, lexeme)

  private def peekOne(expected: Char) = if (endOfSource) false else if (source.charAt(head) != expected) false else true

  @inline private def endOfSource: Boolean = head >= source.length
}
