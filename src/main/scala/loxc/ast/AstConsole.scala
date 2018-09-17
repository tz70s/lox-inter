package loxc.ast
import loxc.Tokens._

object AstConsole {

  def format(expr: Expr): String =
    expr match {
      case Literal(literal) => s"${formatToken(literal)}"
      case Grouping(exp) => s"(group ${format(exp)})"
      case Unary(operator, right) => s"(${formatToken(operator)} ${format(right)})"
      case Binary(left, operator, right) => s"(${formatToken(operator)} ${format(left)} ${format(right)})"
    }

  def formatToken(token: CompleteToken): String =
    token.literal match {
      case Empty => s"${token.token}"
      case NumberLiteral(value) => s"$value"
      case StringLiteral(value) => s"$value"
      case IdentLiteral(value) => s"$value"
    }

  def debug(expr: Expr): Unit =
    println(format(expr))
}
