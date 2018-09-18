package loxc.ast

import loxc.Tokens._
import loxc.tool.ParseError

object Parser {
  def parse(tokens: Array[CompleteToken]): Expr = new Parser(tokens).expression()
}

/**
 * The following bnf notation is the spec of lox grammar, with slightly modified from my own taste.
 *
 * NOTE: this will be further changed.
 *
 * The principle of generating an un-ambiguous context-free grammar is considering Precedence and Associative.
 * From this perspective:
 *
 * Subtypes of Expression  Operators  Associative
 * Unary                   ! -        Right
 * Multiplication          / *        Left
 * Addition                - +        Left
 * Comparison              > >= < <=  Left
 * Equality                == !=      Left
 *
 * We'll build subexpressions from top to bottom with lower precedence to higher precedence.
 * In additional, in recursive descent parsing, we should make the grammar avoid left recursion.
 *
 * expression -> equality ;
 * equality -> comparison ( ( "!=" | "==" ) comparison )* ;
 * comparison -> addition ( ( ">" | ">=" | "<=" | "<" ) addition )* ;
 * addition -> multiplication ( ( "-" | "+" ) multiplication )* ;
 * multiplication -> unary ( ( "/" | "*" ) unary )* ;
 * unary -> ( "!" | "-" ) unary
 *       |  primary ;
 * primary -> literal | grouping ;
 * literal    -> NUMBER | STRING | "true" | "false" | "null" ;
 * grouping   -> "(" expression ")" ;
 *
 * This may be weird at a first glance, i.e. can a equality replace with comparison??
 * But not being fooled around with the naming:
 * They are all subexpressions expressions, and are forced to follow the precedence during substitution.
 */
class Parser(tokens: Array[CompleteToken]) {
  var pos = 0

  /**
   * Using DSL for recursive descent.
   * The each recCall accept a sequence of expected operators, and recursively call (substitute) with subexpression.
   */
  private def expression(): Expr =
    recCall(BangEqual, EqualEqual)(
      recCall(Greater, GreaterEqual, Less, LessEqual)(
        recCall(Minus, Plus)(
          recCall(Slash, Star)(unary())
        )
      )
    )

  /**
   * DSL construction for recursive descent in Binary expression construction.
   * Useful to reduce code repetition in binary expression parsing.
   *
   * Note that the next call is lazy evaluated. This is necessary, or it'll be evaluated when construction.
   * Caused breaking out the ordering of evaluation.
   *
   * @param operators denotes the possible matched tokens.
   * @param next the next, to-be substitute subexpression, which is a lazy evaluated value.
   * @return expression.
   */
  // TODO(tz70s): I'm not sure this abstraction is scalable.
  private def recCall(operators: Token*)(next: => Expr): Expr = {
    var leftExpr = next
    var operator = peekOne

    // This is the most importance thing in the recursive descent parsing.
    // How can we achieve associative?
    // Consider an expression: 7 - 3 + 2
    // We should evaluate it as (7 - 3) + 2 = 6 instead of 7 - (3 + 2) = 2
    // That means: in a left associate rule, we need a left associative parse tree.
    //
    //     +                 +
    //   -   2       v.s.  7   -
    //  7 3                   3 2
    //
    // From intuition, a construction via recursive call, i.e.
    // {{
    //   if (operators.contains(operator.token)) advance Binary(leftExpr, operator, recCall(operators)(next)) else leftExpr
    // }}
    // This will result a right associative tree, it's not a correct result.
    // Therefore, the modification grammar is:
    //
    // addition -> multiplication ( ( "+" | "-" ) multiplication )* ;
    //
    // Means that we need a flattened sequence that combine left associatively, here.
    //
    // In this operation, we flattened out into a sequence, and keep binary combines left associatively.
    // Carefully reading the parsing chapter in Crafting Interpreter book and Engineering a Compiler book.
    //
    // TODO(tz70s): refactor this after later practicing.
    while (operators.contains(operator.token)) {
      advance()
      leftExpr = Binary(leftExpr, operator, next)
      operator = peekOne
    }
    leftExpr
  }

  private def unary(): Expr = {
    val operator = peekOne
    operator.token match {
      case Bang | Minus =>
        advance()
        Unary(operator, unary())
      case _ => primary()
    }
  }

  private def primary(): Expr = {
    val token = peekOne
    token.token match {
      case False | True | Null | Number | StringLiter =>
        advance()
        Literal(token)
      case LeftParen =>
        advance() // Jump off left paren
        val expr = expression()
        // Required a RightParen and drop it.
        if (peekOne.token == RightParen) {
          advance()
          Grouping(expr)
        } else {
          throw ParseError("Expect ')' after group expression")
        }
      case _ => throw ParseError("Parse exhausted, unsupported semantic.")
    }
  }

  private def advance(): Unit =
    if (!endOfSource) pos += 1 else throw ParseError("System error: drained out parsing tokens.")
  private def peekOne = tokens(pos)
  private def endOfSource = peekOne.token == EOF
}
