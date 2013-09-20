package irxground

import irxground._
import org.specs2.mutable._

class ParserSpec extends Specification {

  implicit class ParserExR(val str: String) {
    def >>[T](parser: MyParser.Parser[T]) = MyParser.parse(parser, str).get
  }

  implicit class IntExtension(val num: Int) {
    def xLit = NumberLiteral(num.toString)
  }

  implicit class StringExtension(val str: String) {
    def xId = Identifier(str)
    def xLit = StringLiteral(str)
    def xT = TypeModifier(str)

    def xF(args: Expr*) = xId.xF(args: _*)
    def xM(str: String) = xId.xM(str)
    def xC(str: String) = xId.xC(str)
    def xMe(str: String, args: Expr*) = xId.xMe(str, args: _*)
  }

  implicit class ExprExtension(val ex: Expr) {
    def xF(args: Expr*) = FuncCall(ex, List(args: _*))
    def xM(str: String) = Member(ex, str)
    def xC(str: String) = Cast(ex, TypeModifier(str))
    def xMe(str: String, args: Expr*) = xM(str).xF(args: _*)
  }

  val ps = MyParser

  "Ident" should {
    "parse Alphabet" in {
      "  foo  " >> ps.ident must_== "foo"
    }
  }
  "String Literal" should {
    "be parse" in {
      """ "foo\nbar" """ >> ps.strLiteral must_== ("foo\nbar").xLit
    }
  }
  "Number Literal" should {
    "be parse" in {
      "1" >> ps.numLiteral must_== 1.xLit
    }
  }
  "Identifier" should {
    "be parse" in {
      "foo" >> ps.identExpr must_== "foo".xId
    }
  }
  "External X" should {
    "be parse" in {
      """ external "foo" """ >> ps.external must_== ExternalIdentifier("foo")
    }
    "be parse" in {
      """ external "foo"(bar) """ >> ps.factor must_== ExternalIdentifier("foo").xF("bar".xId)
    }
    "be parse" in {
      """ external "echo"(external "PHP_EOL") """ >> ps.factor must_==
        ExternalIdentifier("echo").xF(ExternalIdentifier("PHP_EOL"))
    }
    "be parse" in {
      """ external operator "+"("Hello", external "PHP_EOL") """ >> ps.factor must_==
        ExternalOperator("+").xF("Hello".xLit, ExternalIdentifier("PHP_EOL"))
    }
  }

  "BinaryExpr" should {
    "be parse" in {
      "1 + 2 * 3 + 4" >> ps.expr must_==
        BinExpr(
          BinExpr(1.xLit, "+", BinExpr(2.xLit, "*", 3.xLit)),
          "+",
          4.xLit)
      " (1 + 2) * (3 + 4)" >> ps.expr must_==
        BinExpr(
          BinExpr(1.xLit, "+", 2.xLit),
          "*",
          BinExpr(3.xLit, "+", 4.xLit)
        )
    }
  }
  "Func Call" should {
    "be parse" in {
      "foo()" >> ps.factor must_== "foo".xF()
      "foo(bar)" >> ps.factor must_== "foo".xF("bar".xId)
      "foo(bar,)" >> ps.factor must_== "foo".xF("bar".xId)
      "foo(bar,baz)" >> ps.factor must_== "foo".xF("bar".xId, "baz".xId)
      "foo(bar)(baz)" >> ps.factor must_== "foo".xF("bar".xId).xF("baz".xId)
    }
  }
  "MemberAccess" should {
    "be parse" in {
      "foo.bar.baz" >> ps.factor must_== "foo".xM("bar").xM("baz")
    }
  }
  "Cast" should {
    "be parse" in {
      "foo[String]" >> ps.factor must_== "foo".xC("String")
    }
  }
  "Method Call" should {
    "be parse" in {
      "foo.bar().baz()" >> ps.factor must_== "foo".xMe("bar").xMe("baz")
    }
  }
  "TypeModifier" should {
    "be parse" in {
      "[org.java.util]" >> ps.typeModifier must_== "org.java.util".xT
    }
  }
  "FuncDec" should {
    "be parse" in {
      "def foo()" >> ps.funcDec must_== FuncDec("foo", Nil, "".xT, None)
    }
    "be parse" in {
      """
      def print(format[System.String], object[Object]) [Unit] {
        print("Hello, world!");
        print("Hello, world!");
      }
      """ >> ps.funcDec must_==
        FuncDec("print",
          List(
            ArgDec("format", "System.String".xT),
            ArgDec("object", "Object".xT)),
          "Unit".xT,
          Some(Block(List(
            "print".xF("Hello, world!".xLit),
            "print".xF("Hello, world!".xLit)
          ))))
    }
    "be parse" in {
      """def operator "+"(left[String], right[String]) """ >> ps.funcDec must_==
        OpDec("+",
          ArgDec("left", "String".xT),
          ArgDec("right", "String".xT),
          TypeModifier(""),
          None)
    }
  }
  "Class/Object declaration" should {
    "be parse" in {
      "class People" >> ps.classDec must_== ClassDec("People", Nil)
    }
    "be parse" in {
      "object People" >> ps.objDec must_== ObjDec(Some("People"), Nil)
    }
  }

  "Declaration List" should {
    "be parse" in {
      """
      language {
        class Foo
        class Bar {
          object {
            def Foo()
          }
        }
      }
      """ >> ps.langDec must_==
        LangDec(None, List(
          ClassDec("Foo", Nil),
          ClassDec("Bar", List(
            ObjDec(None, List(
              FuncDec("Foo", Nil, "".xT, None)
            ))
          ))
        ))
    }
  }

  "Sample program" should {
    "be parse" in {
      """
      def main() {
        Console.WriteLine("Hello");
      }
      """ >> ps.program must_==
        Program(List(
        FuncDec("main",
          Nil,
          "".xT,
          Some(Block(List(
            "Console".xMe("WriteLine", "Hello".xLit)
          ))))
        ))
    }
    "be parse" in {
      """
      language {
        class String

        def operator "+" (left[String], right[String]) [String]

        object Console {
          def WriteLine(str[String])
        }
      }
      """ >> ps.langDec must_!= Nil
    }
    "be parse" in {
      """
      language PHP {
        def operator "+" (left[String], right[String]) [String] {
          external operator "." (left, right)
        }

        object Console {
          def WriteLine(str[String]) {
            external "echo" (str, external "PHP_EOL")
          }
        }
      }
      """ >> ps.langDec must_!= Nil
    }
    "be parse" in {
      """
      language Ruby {
        def operator "+" (left[String], right[String]) [String] {
          external operator "+" (left, right)
        }

        object Console {
          def WriteLine(str[String]) {
            external "puts" (str)
          }
        }
      }
      """ >> ps.langDec must_!= Nil
    }
  }
}
