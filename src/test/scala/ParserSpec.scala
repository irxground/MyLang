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
  "String Literal" should {
    "be parse" in {
      """ "foo\nbar" """ >> ps.strLiteral must_== StringLiteral("foo\nbar")
    }
  }
  "Identifier" should {
    "be parse" in {
      "foo" >> ps.identExpr must_== Identifier("foo")
    }
  }
  "Func Call" should {
    "be parse" in {
      "foo(bar)(hoge piyo)" >> ps.factor must_==
        FuncCall(
          FuncCall(Identifier("foo"), List(Identifier("bar"))),
          List(Identifier("hoge"), Identifier("piyo")))
    }
  }
  "MemberAccess" should {
    "be parse" in {
      "foo.bar.baz" >> ps.factor must_==
        Member(Member(Identifier("foo"), "bar"), "baz")
    }
  }
  "Cast" should {
    "be parse" in {
      "foo[String]" >> ps.factor must_==
        Cast(Identifier("foo"), TypeModifier("String"))
    }
  }
  "Method Call" should {
    "be parse" in {
      "foo.bar().baz()" >> ps.factor must_==
        FuncCall(
          Member(
            FuncCall(
              Member(
                Identifier("foo"),
                "bar"),
              List()),
            "baz"),
          List())
    }
  }
  "TypeModifier" should {
    "be parse" in {
      "[org.java.util]" >> ps.typeModifier must_== TypeModifier("org.java.util")
    }
  }
  "FuncDec" should {
    "be parse" in {
      "def foo()" >> ps.funcDec must_== FuncDec("foo", List(), TypeModifier(""), None)
    }
    "be parse" in {
      """
      def print(format[System.String] object[Object]) [Unit] {
        print("Hello, world!")
        print("Hello, world!")
      }
      """ >> ps.funcDec must_==
        FuncDec("print",
          List(
            ArgDec("format", TypeModifier("System.String")),
            ArgDec("object", TypeModifier("Object"))),
          TypeModifier("Unit"),
          Some(Block(List(
            FuncCall(Identifier("print"), List(StringLiteral("Hello, world!"))),
            FuncCall(Identifier("print"), List(StringLiteral("Hello, world!")))
          ))))
    }
  }
  "Sample program" should {
    "be parse" in {
      """
      def main() {
        Console.WriteLine("Hello")
      }
      """ >> ps.funcDec must_==
        FuncDec("main",
          List(),
          TypeModifier(""),
          Some(Block(List(
            FuncCall(
              Member(Identifier("Console"), "WriteLine"),
              List(StringLiteral("Hello")))))))
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

