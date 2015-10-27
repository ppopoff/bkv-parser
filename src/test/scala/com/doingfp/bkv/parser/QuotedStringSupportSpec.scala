package com.doingfp.bkv.parser

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSpec, Matchers}

/**
 * To test a trait we need to create a mock class
 */
import org.parboiled2._

/**
 * Parser wrapper that mixes the the testable trait, and enhances it
 * by adding EOI at the end of the main rule
 */
class TestableDoubleQuotedStringRecognizer(val input: ParserInput)
extends Parser with QuotedStringSupport {

  def TestableDoubleQuotedString = rule {
    QuotedStringRecognizer ~ EOI
  }
}

case object Test {
  /**
   * Small helper method that creates Testable recognizer for
   */
  def apply(s: String) =
    new TestableDoubleQuotedStringRecognizer(s).TestableDoubleQuotedString.run()
}


@RunWith(classOf[JUnitRunner])
class DoubleQuotedStringSupportSpec extends FunSpec with Matchers {
  describe("Double Quotation support") {
    it ("should not match unquoted string") {
      val unquotedString = "unquoted string"
      Test(unquotedString).isFailure shouldBe true
    }

    it ("should not match string which is quoted only from one side") {
      val leftSideQuotedString = "\"left side quoted string"
      val rightSideQuotedString = "right side quoted string\""

      Test(leftSideQuotedString).isFailure shouldBe true
      Test(rightSideQuotedString).isFailure shouldBe true
    }

    it ("can contain spaces") {
      val text = "\"my double quoted string\""
      Test(text).isSuccess shouldBe true
    }

    it ("can not contain unescaped slashes") {
      val textWithUnescapedSlash = "\"slash is unescaped \\ \""
      Test(textWithUnescapedSlash).isFailure shouldBe true
    }

    describe ("escaped sequences") {
      it ("should accept escaped quotation symbol") {
        val textWithQuotation = "\" \\\" \""
        Test(textWithQuotation).isSuccess shouldBe true
      }

      it ("should accept escaped backslash") {
        val textWithBackslashEscaped = "\" \\\\ \""
        Test(textWithBackslashEscaped).isSuccess shouldBe true
      }

      it ("should accept escaped sequences") {
        val escapedSequences = "\" \\a \\b \\f \\n \\r \\t \\b \""
        Test(escapedSequences).isSuccess shouldBe true
      }

      it ("should not accept other unescaped sequences") {
        val nonExistingEscapedSequences = "\" \\k \\l \\m \""
        Test(nonExistingEscapedSequences).isFailure shouldBe true
      }
    }
  }
}
