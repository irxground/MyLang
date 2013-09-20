package irxground

import irxground._
import org.specs2.mutable._

class PHPParserSpec extends Specification {
  val langA = """
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
  """

  val sourceA = """
    def main() {
      Console.WriteLine("Hello" + "World");
    }
  """

  val outputA = """<?php
echo("Hello" . "World", PHP_EOL);
"""

  def parseLang(code: String): LangDec = {
    val p = MyParser
    return p.parse(p.langDec, code).get
  }

  def parseProg(code: String): Program = {
    val p = MyParser
    return p.parse(p.program, code).get
  }

  "PHP Program" should {
    "be generated" in {
      val lang = parseLang(langA)
      val code = parseProg(sourceA)
      val out = PHPGenerator(code, List(lang))
      out must_== ""
    }
  }
}

