package loxc.ast

import loxc.Tokens.CompleteToken

/**
 * In the evaluation side:
 *
 * The expression will be generally formed into:
 *
 * expression -> literal
 *            |  grouping
 *            |  unary
 *            |  binary ;
 *
 * There's not necessary to bring precedence rules into AST representation.
 */
sealed trait Expr

case class Literal(literal: CompleteToken) extends Expr
case class Grouping(expr: Expr) extends Expr
case class Unary(operator: CompleteToken, right: Expr) extends Expr
case class Binary(left: Expr, operator: CompleteToken, right: Expr) extends Expr
