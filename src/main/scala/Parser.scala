package irxground

import scala._
import util.parsing.combinator._

case class ClassDec(name: String)

case class FuncDec(name: String, argList: List[ArgDec], returnType: TypeModifier, block: Option[Block])

case class ArgDec(name: String, typeDec: TypeModifier)

case class Block(stmt: List[Expr])

sealed abstract class Expr
case class StringLiteral(value: String) extends Expr
case class Identifier(value: String) extends Expr
case class FuncCall(target: Expr, args: List[Expr]) extends Expr
case class Member(target: Expr, member: String) extends Expr
case class Cast(target: Expr, typeMod: TypeModifier) extends Expr

case class TypeModifier(value: String) // TODO: improve


object MyParser extends RegexParsers with  PackratParsers {

  // ----- ----- ----- ----- Helper ----- ----- ----- -----

  @inline private[this] def TODO = throw new NotImplementedError()

  @inline private[this] def read(str: String): Parser[String] = str

  val ident: Parser[String] = "[A-Za-z_][A-Za-z0-9_]*".r

  @inline private[this] def aroundParen[T](p: Parser[T]) = aroundX("(", ")", p)

  def aroundX[T](open: String, close: String, p: Parser[T]): Parser[List[T]] = {
    def loop: Parser[List[T]] =
      (read(close) map { _ => Nil: List[T] }) |
      (for {
        x  <- p
        xs <- loop
      } yield x::xs)

    open ~> loop
  }

  @inline private[this] def toTail[T, U](p: Parser[T])(f: (Expr, T) => U): Parser[Expr => U] = {
    p.map { t => e: Expr => f(e, t) }
  }

  // ----- ----- ----- ----- Expression ----- ----- ----- -----

  def expr: Parser[Expr] = factor // TODO replace to binary operation


  def tailFuncCall: Parser[Expr => FuncCall] = toTail(aroundParen(expr)) { new FuncCall(_, _) }
  def tailMemerAccess: Parser[Expr => Member] = toTail("." ~> ident) { new Member(_, _) }
  def tailCast: Parser[Expr => Cast] = toTail(typeModifier) { new Cast(_, _) }

  def factor: Parser[Expr] = for {
    target <- singleExpr
    rest <- (tailFuncCall | tailMemerAccess | tailCast)*
  } yield rest.foldLeft(target) { (x, f) => f(x) }


  def singleExpr: Parser[Expr] = strLiteral | identExpr

  def strLiteral: Parser[StringLiteral] =
    ("\"" ~> """([^"\\]|\\[rnt"\\])+""".r <~ "\"") map { x =>
      new StringLiteral(x
        .replaceAll("\\\\r", "\r")
        .replaceAll("\\\\n", "\n")
        .replaceAll("\\\\t", "\t")
      )
    }

  def identExpr: Parser[Identifier] = ident map { new Identifier(_) }

  // ----- ----- ----- ----- Declaration ----- ----- ----- -----

  def funcDec: Parser[FuncDec] = for {
    _       <- read("def")
    name    <- ident
    args    <- aroundParen(argument)
    retType <- opt(typeModifier)
    block   <- opt(block)
  } yield new FuncDec(name, args, retType.getOrElse(new TypeModifier("")), block)

  def argument: Parser[ArgDec] = for {
    name <- ident
    typeDec <- typeModifier
  } yield new ArgDec(name, typeDec);

  def typeModifier: Parser[TypeModifier] = "[" ~> "[^\\]]+".r <~ "]" map { new TypeModifier(_) }

  // ----- ----- ----- ----- Statements ----- ----- ----- -----
  def block: Parser[Block] = aroundX("{", "}", expr) map { new Block(_) }
}

