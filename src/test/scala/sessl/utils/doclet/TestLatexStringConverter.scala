package sessl.utils.doclet

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSpec

/**
 * Test for [[LatexStringConverter]].
 *
 * @author Roland Ewald
 */
@RunWith(classOf[JUnitRunner])
class TestLatexStringConverter extends FunSpec {

  import LatexStringConverter._

  describe("Latex String Converter") {

    describe("special character conversion") {

      it("deals with empty input") {
        assert(convertSpecialChars("") === "")
      }

      it("deals with special characters like '" + escapeSymbols.mkString(",") + "'") {
        assert(convertSpecialChars("_test%__$ string") === """\_test\%\_\_\$ string""")
      }

      it("leaves other input alone") {
        assert(convertSpecialChars("test string") === "test string")
      }

      it("deals correctly with cases like '<T>'") {
        assert(convertSpecialChars("<T") === """\textless T""")
      }
    }

    describe("hyphenation") {

      it("works with empty / normal input") {
        assert(camelCaseHyphenation("") === "")
        assert(camelCaseHyphenation("test_string") === "test_string")
      }

      it("works for camel case") {
        assert(camelCaseHyphenation("myMethodName") === """my\-Method\-Name""")
      }

      it("leaves abbreviations alone") {
        assert(camelCaseHyphenation("ABCConjectureSolution") === """ABCConjecture\-Solution""")
      }
    }

    describe("code inliner") {

      it("works with empty/normal input") {
        val simpleDef = "def f(x: Int): Int = x"
        assert(codeToInline(simpleDef) === newlineCharCodeInline + simpleDef)
        assert(codeToInline("") === newlineCharCodeInline)
      }

      it("it prefixes special chars with a '\\'") {
        assert(codeToInline("""{}\%""") === newlineCharCodeInline + """\{\}\\\%""")
      }

      it("converts double spaces to '\\ \\ '") {
        assert(codeToInline("here are  spaces") === newlineCharCodeInline + """here are\ \ spaces""")
      }

      it("it converts leading spaces to '\\ '") {
        assert(codeToInline(" so") === newlineCharCodeInline + """\ so""")
      }

      it("it appends '^^J' to lines (and at the beginning)") {
        assert(codeToInline("a\nb") === newlineCharCodeInline + "a^^J\nb")
      }
    }

    it("produces valid labels") {
      assert(produceValidLabel("a%#f") === "a::f")
    }

  }
}