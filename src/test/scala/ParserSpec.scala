package irxground

import irxground._
import org.specs2.mutable._

class ParserSpec extends Specification {

  implicit class ParserExL[T](val parser: MyParser.Parser[T]) {
    def <<(str: String) = MyParser.parse(parser, str).get
  }

  implicit class ParserExR(val str: String) {
    def >>[T](parser: MyParser.Parser[T]) = MyParser.parse(parser, str).get
  }

  val ps = MyParser

  "Ident" should {
    "parse Alphabet" in {
      "  foo  " >> ps.ident must_== "foo"
    }
  }
  "Paren List" should {
    "be parsed" in {
      "(a b c)" >> ps.aroundParen(ps.ident) must_== List("a", "b", "c")
    }
  }
  "String Literal" should {
    "be parse" in {
      """ "foo\nbar" """ >> ps.strLiteral must_== new StringLiteral("foo\nbar")
    }
  }
  "Identifier" should {
    "be parse" in {
      "foo" >> ps.identExpr must_== new Identifier("foo")
    }
  }
  "FuncCall" should {
    "be parse" in {
      "foo(bar)(hoge piyo)" >> ps.funcCallExpr must_==
        new FuncCall(
          new FuncCall(new Identifier("foo"), List(new Identifier("bar"))),
          List(new Identifier("hoge"), new Identifier("piyo")))
    }
  }
  "TypeModifier" should {
    "be parse" in {
      "[org.java.util]" >> ps.typeModifier must_== new TypeModifier("org.java.util")
    }
  }
  "FuncDec" should {
    "be parse" in {
      "def foo()" >> ps.funcDec must_== new FuncDec("foo", List(), new TypeModifier(""), None)
    }
    "be parse" in {
      """
      def print(format[System.String] object[Object]) [Unit] {
        print("Hello, world!")
        print("Hello, world!")
      }
      """ >> ps.funcDec must_==
        new FuncDec("print",
          List(
            new ArgDec("format", new TypeModifier("System.String")),
            new ArgDec("object", new TypeModifier("Object"))),
          new TypeModifier("Unit"),
          Some(new Block(List(
            new FuncCall(new Identifier("print"), List(new StringLiteral("Hello, world!"))),
            new FuncCall(new Identifier("print"), List(new StringLiteral("Hello, world!")))
          ))))
    }
  }

  // "FunctionDec" should {
  //   "be parse" in {

  //   }
  // }
/*
  "ClassDec" should {
    "be parse" in {
      val dec = ps.classDec << "class Foo"
      dec.name must_== "Foo"
    }
    "be parse" in {
      val dec = ps.classDec << "class Foo {}"
      dec.name must_== "Foo"
    }
  }


*/
}

