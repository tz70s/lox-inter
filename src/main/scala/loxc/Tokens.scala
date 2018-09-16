package loxc

/** Token values. */
// TODO(tz70s): refactor this boilerplate code.
object Tokens {
  case class CompleteToken(token: Token, numOfLine: Int, literal: LiteralValue = Empty)

  sealed trait Token {
    def apply(numOfLine: Int, literal: String = "") = CompleteToken(this, numOfLine)
  }

  sealed trait LiteralValue
  case object Empty extends LiteralValue
  case class IdentLiteral(value: String) extends LiteralValue
  case class StringLiteral(value: String) extends LiteralValue
  case class NumberLiteral(value: Double) extends LiteralValue

  sealed trait SingleCharToken extends Token
  case object LeftParen extends SingleCharToken
  case object RightParen extends SingleCharToken
  case object LeftBrace extends SingleCharToken
  case object RightBrace extends SingleCharToken
  case object Comma extends SingleCharToken
  case object Dot extends SingleCharToken
  case object Minus extends SingleCharToken
  case object Plus extends SingleCharToken
  case object Slash extends SingleCharToken
  case object Star extends SingleCharToken

  val singleCodec =
    Map('(' -> LeftParen,
        ')' -> RightParen,
        '{' -> LeftBrace,
        '}' -> RightBrace,
        ',' -> Comma,
        '.' -> Dot,
        '-' -> Minus,
        '+' -> Plus,
        '*' -> Star)

  sealed trait MultiCharToken extends Token
  case object Bang extends MultiCharToken
  case object BangEqual extends MultiCharToken
  case object Equal extends MultiCharToken
  case object EqualEqual extends MultiCharToken
  case object Greater extends MultiCharToken
  case object GreaterEqual extends MultiCharToken
  case object Less extends MultiCharToken
  case object LessEqual extends MultiCharToken

  sealed trait Literal extends Token
  case object Ident extends Literal {
    override def apply(numOfLine: Int, literal: String) =
      CompleteToken(this, numOfLine, IdentLiteral(literal))
  }
  case object StringLiter extends Literal {
    override def apply(numOfLine: Int, literal: String) =
      CompleteToken(this, numOfLine, StringLiteral(literal))
  }
  case object Number extends Literal {
    override def apply(numOfLine: Int, literal: String) =
      CompleteToken(this, numOfLine, NumberLiteral(literal.toDouble))
  }

  val keyWordsCodec = Map(
    "and" -> And,
    "class" -> Clazz,
    "else" -> Else,
    "fun" -> Fun,
    "for" -> For,
    "if" -> If,
    "null" -> Null,
    "or" -> Or,
    "print" -> Print,
    "return" -> Return,
    "super" -> Super,
    "this" -> This,
    "true" -> True,
    "false" -> False,
    "let" -> Let,
    "while" -> While
  )

  sealed trait KeyWord extends Token
  case object And extends KeyWord
  case object Clazz extends KeyWord
  case object Else extends KeyWord
  case object Fun extends KeyWord
  case object For extends KeyWord
  case object If extends KeyWord
  case object Null extends KeyWord
  case object Or extends KeyWord
  case object Print extends KeyWord
  case object Return extends KeyWord
  case object Super extends KeyWord
  case object This extends KeyWord
  case object True extends KeyWord
  case object False extends KeyWord
  case object Let extends KeyWord
  case object While extends KeyWord

  case object EOF extends Token
}
