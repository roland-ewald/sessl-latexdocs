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
    }

    describe("hyphenation") {

      it("works with empty / normal input") {
        assert(camelCaseHyphenation("") === "")
        assert(camelCaseHyphenation("test_string") === "test_string")
      }

      it("works for camel case") {
        assert(camelCaseHyphenation("myMethodName") === """my\-Method\-Name""")
        assert(camelCaseHyphenation("ABCConjectureSolution") === """ABCConjecture\-Solution""")
      }
    }

  }
}