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

    it("deals with empty input") {
      assert(convert("") === "")
    }

    it("deals with special characters like '" + escapeSymbols.mkString(",") + "'") {
      assert(convert("_test%__$ string") === """\_test\%\_\_\$ string""")
    }

    it("leaves other input alone") {
      assert(convert("test string") === "test string")
    }

  }
}