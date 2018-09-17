package loxc.ast

import loxc.Tokens.CompleteToken

/**
 * The following bnf notation is the spec of lox grammar, with slightly modified from my own taste.
 *
 * expression -> literal
 *             | unary
 *             | binary
 *             | grouping ;
 *
 * literal    -> NUMBER | STRING | "true" | "false" | "null" ;
 * grouping   -> "(" expression ")" ;
 * unary      -> ( "-" | "!" ) expression ;
 * binary     -> expression operator expression ;
 * operator   -> "==" | "!=" | "<" | "<=" | ">=" | ">=" | "+" | "-"| "*" | "/" ;
 *
 */
sealed trait Expr

case class Literal(literal: CompleteToken) extends Expr
case class Grouping(expr: Expr) extends Expr
case class Unary(operator: CompleteToken, right: Expr) extends Expr
case class Binary(left: Expr, operator: CompleteToken, right: Expr) extends Expr
