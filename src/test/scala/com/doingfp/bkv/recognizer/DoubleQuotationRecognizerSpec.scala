package com.doingfp.bkv.recognizer

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{Matchers, FunSpec}

/**
 * To test a trait we need to create a mock class
 */
import org.parboiled2._
class TestableDoubleQuotedStringRecognizer(val input: ParserInput) extends Parser with DoubleQuotationRecognizer {
  def TestableDoubleQuotedString = rule {
    DoubleQuotedString ~ EOI
  }
}

@RunWith(classOf[JUnitRunner])
class DoubleQuotationRecognizerSpec extends FunSpec with Matchers {

  def test(s: String) =
    new TestableDoubleQuotedStringRecognizer(s).TestableDoubleQuotedString.run()

  describe ("Double quoted string") {
    it ("should not match unquoted string") {
      val unquotedString = "unquoted string"
      test(unquotedString).isFailure shouldBe true
    }

    it ("should not match string which is quoted only from one side") {
      val leftSideQuotedString = "\"left side quoted string"
      val rightSideQuotedString = "right side quoted string\""

      test(leftSideQuotedString).isFailure shouldBe true
      test(rightSideQuotedString).isFailure shouldBe true
    }

    it ("can contain spaces") {
      val text = "\"my double quoted string\""
      test(text).isSuccess shouldBe true
    }

    it ("can not contain unescaped slashes") {
      val textWithUnescapedSlash = "\"slash is unescaped \\ \""
      test(textWithUnescapedSlash).isFailure shouldBe true
    }

    describe ("escaped sequences") {
      it ("should accept escaped quotation symbol") {
        val textWithQuotation = "\" \\\" \""
        test(textWithQuotation).isSuccess shouldBe true
      }

      it ("should accept escaped backslash") {
        val textWithBackslashEscaped = "\" \\\\ \""
        test(textWithBackslashEscaped).isSuccess shouldBe true
      }

      it ("should accept escaped sequences") {
        val escapedSequences = "\" \\a \\b \\f \\n \\r \\t \\b \""
        test(escapedSequences).isSuccess shouldBe true
      }

      it ("should not accept other unescaped sequences") {
        val nonExistingEscapedSequences = "\" \\k \\l \\m \""
        test(nonExistingEscapedSequences).isFailure shouldBe true
      }
    }
  }
}
