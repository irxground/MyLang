package irxground


trait Generator {

}

object PHPGenerator extends Generator {

  def apply(p: Program, langs: List[LangDec]): String = {

    var writer = new PHPWriter
    writer.start
    p.content.foreach {
      case FuncDec("main", _, _, Some(block)) =>
        writer.writeBlock(block)

      case _ => ;
    }
    return writer.toString
  }
}

class PHPWriter {
  import java.lang.StringBuilder

  case class Indent(value: Int)

  val buff = new StringBuilder
  val indentToken = "\t"
  val newline = "\r\n"

  override def toString = buff.toString

  def start = {
    buff.append("<?php").append(newline)
  }

  def writeBlock(block: Block)(implicit i: Indent = Indent(0)) {
    for (expr <- block.stmt) {
      writeStmt(expr)
    }
  }

  def writeStmt(expr: Expr)(implicit i: Indent) {
    writeIndent
    writeExpr(expr)
    buff.append(";").append(newline)
  }

  def writeExpr(expr: Expr) {
    expr match {
      case FuncCall(Member(obj, meth), args) =>
        writeExpr(obj)
        buff.append("->").append(meth).append("(")
        writeWithComma(args)
        buff.append(")")

      case Identifier(x) =>
        buff.append(x)

      case StringLiteral(x) =>
        writeStringLiteral(x)

      case BinExpr(left, op, right) =>
        writeExpr(left)
        buff.append(" ")
        buff.append(op)
        buff.append(" ")
        writeExpr(right)

      case _ =>
        buff.append("/* Not supported( " + expr + ") */")
    }
  }

  def writeWithComma(exprs: Seq[Expr]) {
    var splitter = ""
    for (expr <- exprs) {
      buff.append(splitter)
      splitter = ", "
      writeExpr(expr)
    }
  }

  def writeStringLiteral(x: String) {
    buff.append('"')
    buff.append(x)
    buff.append('"')
  }

  def writeIndent(implicit i: Indent) {
    for (_ <- 1 to i.value) {
      buff.append(indentToken)
    }
  }
}
