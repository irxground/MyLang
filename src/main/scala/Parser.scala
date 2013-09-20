package irxground

import scala._
import util.parsing.combinator._

trait DeclarationBlock {
  def block: List[Declaration];
}
case class LangDec(name: Option[String], body: List[Declaration]) extends DeclarationBlock { def block = body }
case class Program(content: List[Declaration]) extends DeclarationBlock { def block = content }

sealed abstract class Declaration
case class ClassDec(name: String, body: List[Declaration]) extends Declaration
case class ObjDec(name: Option[String], body: List[Declaration]) extends Declaration

case class FuncDec(name: String, argList: List[ArgDec], returnType: TypeModifier, block: Option[Block]) extends Declaration
case class OpDec(operator: String, left: ArgDec, right: ArgDec, returnType: TypeModifier, block: Option[Block]) extends Declaration

case class ArgDec(name: String, typeDec: TypeModifier)

case class Block(stmt: List[Expr])

sealed abstract class Expr
case class BinExpr(left: Expr, op: String, right: Expr) extends Expr
case class Identifier(value: String) extends Expr
case class FuncCall(target: Expr, args: List[Expr]) extends Expr
case class Member(target: Expr, member: String) extends Expr
case class Cast(target: Expr, typeMod: TypeModifier) extends Expr

case class ExternalOperator(op: String) extends Expr
case class ExternalIdentifier(value: String) extends Expr

case class StringLiteral(value: String) extends Expr
case class NumberLiteral(value: String) extends Expr

case class TypeModifier(value: String) // TODO: improve


object MyParser extends RegexParsers with  PackratParsers {

  // ----- ----- ----- ----- Helper ----- ----- ----- -----

  @inline private[this] def TODO = throw new NotImplementedError()

  @inline private[this] def read(str: String): Parser[String] = str

  val ident: Parser[String] = "[A-Za-z_][A-Za-z0-9_]*".r
  val number: Parser[String] = "[1-9][_0-9]*".r

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

  @inline private[this] def orParser(ps: Seq[String]): Parser[String] = {
    ps.map{ (x: String) => read(x) }
      .reduce{ (x: Parser[String], y: Parser[String]) => (x | y): Parser[String] }
  }
  // ---- operator
  val operators = Array(
    Array("+", "-"),
    Array("*", "/", "%")
  )
  val opTokens: Parser[String] = orParser(operators.flatMap{ x => x })

  def operator(ops: Seq[String]): Parser[String] = for {
    head <- orParser(ops)
    rest <- opTokens*
  } yield (head::rest).mkString

  def expr: Parser[Expr] = binExpr(0) // TODO replace to binary operation

  def binExpr(n: Int): Parser[Expr] = {
    if (n >= operators.length) factor
    else {
      for {
        left <- binExpr(n + 1)
        rest <- (for {
          op <- operator(operators(n))
          right <- binExpr(n + 1)
        } yield (ex: Expr) => BinExpr(ex, op, right))*
      } yield rest.foldLeft(left){ (x, f) => f(x) }
    }
  }

  // ---- chains

  def factor: Parser[Expr] = for {
    target <- singleExpr
    rest <- (tailFuncCall | tailMemerAccess | tailCast)*
  } yield rest.foldLeft(target) { (x, f) => f(x) }

  def tailFuncCall: Parser[Expr => FuncCall] = toTail(aroundParen(expr)) { FuncCall(_, _) }
  def tailMemerAccess: Parser[Expr => Member] = toTail("." ~> ident) { Member(_, _) }
  def tailCast: Parser[Expr => Cast] = toTail(typeModifier) { Cast(_, _) }

  // ---- single Expr

  def singleExpr: Parser[Expr] = paren | external | numLiteral | strLiteral | identExpr
  def paren = "(" ~> expr <~ ")"

  def external: Parser[Expr] = for {
    _ <- read("external")
    x <- opt("operator")
    v <- quoteStr
  // } yield x.fold(ExternalIdentifier(v): Expr) { _ => ExternalOperator(v): Expr }
  } yield if (x.nonEmpty) ExternalOperator(v) else ExternalIdentifier(v)

  def strLiteral: Parser[StringLiteral] = quoteStr map StringLiteral
  def numLiteral: Parser[NumberLiteral] = number map NumberLiteral

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

  def program: Parser[Program] = (declaration*) map Program

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

