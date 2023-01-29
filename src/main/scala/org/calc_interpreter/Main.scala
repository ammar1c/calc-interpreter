package org.calc_interpreter

enum TokenType:
  case Number
  case Plus
  case Minus
  case Mul
  case Div
  case LParen
  case RParen
  case EOF

final case class Token(tokenType: TokenType, value: String, pos: Int)

sealed trait Expr

final case class NumberExpr(value: Double) extends Expr:
  override def toString: String = value.toString

final case class AddExpr(left: Expr, right: Expr) extends Expr:
  override def toString: String = s"($left + $right)"

final case class SubExpr(left: Expr, right: Expr) extends Expr:
  override def toString: String = s"($left - $right)"

final case class MulExpr(left: Expr, right: Expr) extends Expr:
  override def toString: String = s"($left * $right)"

final case class DivExpr(left: Expr, right: Expr) extends Expr:
  override def toString: String = s"($left / $right)"

class Parser(tokens: Seq[Token]):
  var pos: Int = 0
  var nextToken: Token = _
  advance()

  private def advance(): Unit = {
    if (pos == tokens.length - 1) {
      nextToken = Token(TokenType.EOF, "", -1)
    } else {
      nextToken = tokens(pos)
      pos += 1
    }
  }

  def term(): Expr =
    var result = factor()
    while (nextToken.tokenType != TokenType.EOF && (nextToken.tokenType == TokenType.Mul || nextToken.tokenType == TokenType.Div)) do
      val token = nextToken
      if (token.tokenType == TokenType.Mul) then
        advance()
        result = MulExpr(result, factor())
      else if (token.tokenType == TokenType.Div) then
        advance()
        result = DivExpr(result, factor())

    result

  def factor(): Expr =
    if nextToken.tokenType == TokenType.EOF then
      return null

    if nextToken.tokenType == TokenType.LParen then
      advance()
      val result = parse()
      if nextToken.tokenType != TokenType.RParen then
        throw new RuntimeException("Expected ')'")
      advance()
      return result
    if nextToken.tokenType == TokenType.Number then
      val token = nextToken
      advance()
      return NumberExpr(token.value.toInt)

    throw new RuntimeException("Invalid syntax")


  def parse(): Expr =
    var result = term()
    while (nextToken.tokenType != TokenType.EOF && (nextToken.tokenType == TokenType.Plus || nextToken.tokenType == TokenType.Minus)) do
      nextToken.tokenType match {
        case TokenType.Plus =>
          advance()
          result = AddExpr(result, term())
        case TokenType.Minus =>
          advance()
          result = SubExpr(result, term())
      }
    result


class Lexer(text: String):
  private var currentChar: Char = 0;
  private var pos = 0
  advance()

  private def advance(): Unit =
    if (pos == text.length - 1) {
      currentChar = 0
    } else {
      currentChar = text(pos)
      pos += 1
    }

  private def number(): Token =
    var result = ""
    var decimals = 0
    while currentChar != 0 && (currentChar.isDigit || currentChar == '.') do
      if currentChar == '.' then decimals += 1
      if decimals > 1 then throw new Exception(s"Invalid number at pos {$pos}")
      result += currentChar
      advance()
    //    println(s"Number: {$result}")
    Token(TokenType.Number, result, pos)

  def tokens(): Seq[Token] =
    var result = Seq[Token]()
    while currentChar != 0 do
      if currentChar == '.' || currentChar.isDigit then
      //        println(s"digit: {$currentChar}")
        result = result :+ number()
      else if currentChar == '+' then
        result = result :+ Token(TokenType.Plus, "+", pos)
        advance()
      else if currentChar == '-' then
        result = result :+ Token(TokenType.Minus, "-", pos)
        advance()
      else if currentChar == '*' then
        result = result :+ Token(TokenType.Mul, "*", pos)
        advance()
      else if currentChar == '/' then
        result = result :+ Token(TokenType.Div, "/", pos)
        advance()
      else if currentChar == '(' then
        result = result :+ Token(TokenType.LParen, "(", pos)
        advance()
      else if currentChar == ')' then
        result = result :+ Token(TokenType.RParen, ")", pos)
        advance()
      else if currentChar.isWhitespace then
        advance()
      else
        throw new Exception("Invalid character")

    result = result :+ Token(TokenType.EOF, "", pos)
    result

object Interpreter:
  def interpret(expr: Expr): Double =
    expr match {
      case NumberExpr(value) => value
      case AddExpr(left, right) => interpret(left) + interpret(right)
      case SubExpr(left, right) => interpret(left) - interpret(right)
      case MulExpr(left, right) => interpret(left) * interpret(right)
      case DivExpr(left, right) => interpret(left) / interpret(right)
    }
object Main {

  def main(args: Array[String]): Unit =
    while true do
      print("calc> ")
      val line = scala.io.StdIn.readLine()
      val lexer = new Lexer(line + " ")
      val tokens = lexer.tokens()
      println(tokens)
      var tree = new Parser(tokens).parse()
      println(tree)
      var value = Interpreter.interpret(tree)
      println(value)

}
