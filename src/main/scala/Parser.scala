package irxground

import scala._
import util.parsing.combinator._

case class LangDec(name: Option[String], body: List[Declaration])

sealed abstract class Declaration
case class ClassDec(name: String, body: List[Declaration]) extends Declaration
case class ObjDec(name: Option[String], body: List[Declaration]) extends Declaration

case class FuncDec(name: String, argList: List[ArgDec], returnType: TypeModifier, block: Option[Block]) extends Declaration
case class OpDec(operator: String, left: ArgDec, right: ArgDec, returnType: TypeModifier, block: Option[Block]) extends Declaration

case class ArgDec(name: String, typeDec: TypeModifier)

case class Block(stmt: List[Expr])

sealed abstract class Expr
case class StringLiteral(value: String) extends Expr
case class Identifier(value: String) extends Expr
case class FuncCall(target: Expr, args: List[Expr]) extends Expr
case class Member(target: Expr, member: String) extends Expr
case class Cast(target: Expr, typeMod: TypeModifier) extends Expr

case class ExternalOperator(op: String) extends Expr
case class ExternalIdentifier(value: String) extends Expr

case class TypeModifier(value: String) // TODO: improve


object MyParser extends RegexParsers with  PackratParsers {

  // ----- ----- ----- ----- Helper ----- ----- ----- -----

  @inline private[this] def TODO = throw new NotImplementedError()

  @inline private[this] def read(str: String): Parser[String] = str

  val ident: Parser[String] = "[A-Za-z_][A-Za-z0-9_]*".r

  @inline private[this] def aroundParen[T](p: Parser[T]) = aroundX("(", ")", Some(","), p)

  def aroundX[T](open: String, close: String, separator: Option[String], p: Parser[T]): Parser[List[T]] = {
    val endParser: Parser[List[T]] = read(close) map { _ => Nil }
    def withSep(p: Parser[List[T]]): Parser[List[T]] = separator match {
      case Some(s) => for {
        sep <- opt(s)
        rest <- if (sep.nonEmpty) p else endParser
      } yield rest
      case None => p
    }
    def loop: Parser[List[T]] = {
      endParser |
      (for {
        x  <- p
        xs <- withSep(loop)
      } yield x::xs)
    }
    open ~> loop
  }

  @inline private[this] def toTail[T, U](p: Parser[T])(f: (Expr, T) => U): Parser[Expr => U] = {
    p.map { t => e: Expr => f(e, t) }
  }

  // ----- ----- ----- ----- Expression ----- ----- ----- -----

  // ---- operator
  def expr: Parser[Expr] = factor // TODO replace to binary operation


  // ---- chains

  def factor: Parser[Expr] = for {
    target <- singleExpr
    rest <- (tailFuncCall | tailMemerAccess | tailCast)*
  } yield rest.foldLeft(target) { (x, f) => f(x) }

  def tailFuncCall: Parser[Expr => FuncCall] = toTail(aroundParen(expr)) { FuncCall(_, _) }
  def tailMemerAccess: Parser[Expr => Member] = toTail("." ~> ident) { Member(_, _) }
  def tailCast: Parser[Expr => Cast] = toTail(typeModifier) { Cast(_, _) }

  // ---- single Expr

  def singleExpr: Parser[Expr] = external | strLiteral | identExpr

  def external: Parser[Expr] = for {
    _ <- read("external")
    x <- opt("operator")
    v <- quoteStr
  // } yield x.fold(ExternalIdentifier(v): Expr) { _ => ExternalOperator(v): Expr }
  } yield if (x.nonEmpty) ExternalOperator(v) else ExternalIdentifier(v)

  def strLiteral: Parser[StringLiteral] = quoteStr map StringLiteral

  def quoteStr: Parser[String] =
    ("\"" ~> """([^"\\]|\\[rnt"\\])+""".r <~ "\"") map { x =>
      x
        .replaceAll("\\\\r", "\r")
        .replaceAll("\\\\n", "\n")
        .replaceAll("\\\\t", "\t")
    }

  def identExpr: Parser[Identifier] = ident map Identifier

  // ----- ----- ----- ----- Declaration ----- ----- ----- -----

  def decList: Parser[List[Declaration]] = aroundX("{", "}", None, declaration)
  def optDecList: Parser[List[Declaration]] = opt(decList) map { _ getOrElse Nil }

  def langDec: Parser[LangDec] = for {
    _ <- read("language")
    name <- opt(quoteStr | ident)
    body <- decList
  } yield LangDec(name, body)

  def declaration: Parser[Declaration] = classDec | objDec | funcDec

  def classDec: Parser[ClassDec] = for {
    _    <- read("class")
    name <- ident
    body <- optDecList
  } yield ClassDec(name, body)

  def objDec: Parser[ObjDec] = for {
    _    <- read("object")
    name <- opt(ident)
    body <- optDecList
  } yield ObjDec(name, body)

  def funcDec: Parser[Declaration] = "def" ~> (partialOpDec | partialFuncDec)

  def partialOpDec: Parser[OpDec] = for {
    _       <- read("operator")
    name    <- quoteStr
    _       <- read("(")
    left    <- argument
    _       <- read(",")
    right   <- argument
    _       <- opt(",")
    _       <- read(")")
    retType <- opt(typeModifier)
    block   <- opt(block)
  } yield OpDec(name, left, right, retType.getOrElse(TypeModifier("")), block)

  def partialFuncDec: Parser[FuncDec] = for {
    name    <- ident
    args    <- aroundParen(argument)
    retType <- opt(typeModifier)
    block   <- opt(block)
  } yield FuncDec(name, args, retType.getOrElse(TypeModifier("")), block)

  def argument: Parser[ArgDec] = for {
    name <- ident
    typeDec <- typeModifier
  } yield ArgDec(name, typeDec);

  def typeModifier: Parser[TypeModifier] = "[" ~> "[^\\]]+".r <~ "]" map { TypeModifier(_) }

  // ----- ----- ----- ----- Statements ----- ----- ----- -----
  def block: Parser[Block] = aroundX("{", "}", Some(";"), expr) map { Block(_) }
}

