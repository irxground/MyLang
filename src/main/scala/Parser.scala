package irxground

import scala._
import util.parsing.combinator._

case class ClassDec(name: String)

case class FuncDec(name: String, argList: List[ArgDec], returnType: TypeModifier, block: Option[Block])

case class ArgDec(name: String, typeDec: TypeModifier)

case class Block(stmt: List[Expr])

sealed abstract class Expr
case class StringLiteral(value: String) extends Expr
case class Identifier(value: String)    extends Expr
case class FuncCall(target: Expr, args: List[Expr]) extends Expr

case class TypeModifier(value: String) // TODO: improve


object MyParser extends RegexParsers with  PackratParsers {

  // ----- ----- ----- ----- Helper ----- ----- ----- -----

  @inline def TODO = throw new NotImplementedError()

  @inline def read(str: String): Parser[String] = str

  val ident: Parser[String] = "[A-Za-z_][A-Za-z0-9_]*".r

  @inline def aroundParen[T](p: Parser[T]) = aroundX("(", ")", p)

  def aroundX[T](open: String, close: String, p: Parser[T]): Parser[List[T]] = {
    def loop: Parser[List[T]] =
      (read(close) map { _ => Nil: List[T] }) |
      (for {
        x  <- p
        xs <- loop
      } yield x::xs)

    open ~> loop
  }

  // ----- ----- ----- ----- Expression ----- ----- ----- -----

  def expr: Parser[Expr] = funcCallExpr

  def funcCallExpr: Parser[Expr] = for {
    target      <- singleExpr
    argListList <- aroundParen(expr)*
  } yield argListList.foldLeft(target) { new FuncCall(_, _) }

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

  /*

  def classDec: Parser[ClassDec] = "class" ~> ident <~ (classDecBody*) map { new ClassDec(_) }

  def classDecBody: Parser[Unit] = "{" ~ "}" map { _ => Unit }


  def argList: Parser[List[ArgDec]] = repsep(argument, ",")

  def argument: Parser[ArgDec] = for {
    name <- ident
    typeDec <- typeUse
  } yield new ArgDec(name, typeDec);

  def typeUse: Parser[String] =
    ident.around("[", "]")

  implicit class ParserEx[T](val parser: Parser[T]) {

    // def <<(str: String) = parse(parser, str).get

    def around(left: String, right: String): Parser[T] =
      left ~> parser <~ right
  }
  */
}

