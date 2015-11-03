package com.doingfp.bkv.parser

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSpec, Matchers}


/**
 * Tests parser internals
 *  - whitespace recognition
 *  - optional whitespace's behaviour
 *  - newline recognition
 *  - key
 *  - value
 */
@RunWith(classOf[JUnitRunner])
class BkvParserComponentsSpec extends FunSpec with Matchers {
  describe ("Companion object") {
    describe ("Key symbol") {
      it ("should not contain whitespace characters") {
        BkvParser.KeyChar.matchesAny(" \t\n") shouldBe false
      }

      it ("may contain any alphanumeric symbol, or underscore") {
        BkvParser.KeyChar.matchesAll("AaZz0123456789_") shouldBe true
      }

      it ("should fail for other printable symbols") {
        BkvParser.BlockNameChar.matchesAny(";/#-=(){}[]`!") shouldBe false
      }
    }

    describe("Block name symbol") {
      it ("should not contain whitespace characters") {
        BkvParser.BlockNameChar.matchesAny(" \t\n") shouldBe false
      }

      it ("may contain any alphanumeric symbol, or underscore, or dot") {
        BkvParser.BlockNameChar.matchesAll("AaZz0123456789_.") shouldBe true
      }

      it ("should fail for other printable symbols") {
        BkvParser.BlockNameChar.matchesAny(";/#-=(){}[]`!") shouldBe false
      }
    }
  }

  describe ("Bkv Recognizer rules") {
    describe("WhiteSpace characters") {
      def testWhitespace(input: String) =
        new TestableBkvParser(input).TestableWhitespace.run()

      it ("should fail on empty input") {
        testWhitespace("").isFailure shouldBe true
      }

      it ("should recognize whitespace char") {
        testWhitespace(" ").isSuccess shouldBe true
      }

      it ("should recognize \\n as whitespace char") {
        testWhitespace("\n").isSuccess shouldBe true
      }

      it ("should recognize \\t as whitespace char") {
        testWhitespace("\t").isSuccess shouldBe true
      }
    }

    describe("Optional whitespaces") {
      describe ("matches zero or more whitespace chars") {
        def testOptionalWhitespaces(input: String) =
          new TestableBkvParser(input).TestableOptionalWhitespaces.run()

        it ("should match no whitespace chars at all") {
          testOptionalWhitespaces("").isSuccess shouldBe true
        }

        it ("should match a single whitespace char") {
          testOptionalWhitespaces(" ").isSuccess shouldBe true
        }

        it ("should match a multiple whitespace chars") {
          testOptionalWhitespaces("   ").isSuccess shouldBe true
        }

        it ("should match different combinations of whitespace characters") {
          testOptionalWhitespaces("\t\n  ").isSuccess shouldBe true
        }
      }
    }

    describe("Newline") {
      describe("Unix/Linux newline") {
        def testNewLine(input: String) =
          new TestableBkvParser(input).TestableNewLine.run()

        it ("should match unix/linux newline") {
          testNewLine("\n").isSuccess shouldBe true
        }

        it ("should match Windows type of newline") {
          testNewLine("\r\n").isSuccess shouldBe true
        }
      }
    }

    describe("Key") {
      def testKey(input: String) =
        new TestableBkvParser(input).TestableKey.run()

      it ("should contain only valid symbols") {
        testKey("valid_Key0123").isSuccess shouldBe true
      }

      it ("should not contain whitespaces") {
        testKey("invalid Key0123").isFailure shouldBe true
      }

      it ("should not contain unallowed symbols") {
        testKey(";").isFailure shouldBe true
        testKey("()").isFailure shouldBe true
        testKey("{}").isFailure shouldBe true
        testKey("/").isFailure shouldBe true
        testKey("!@#$%^&*").isFailure shouldBe true
      }
    }

    describe("Value") {
      def testValue(input: String) =
        new TestableBkvParser(input).TestableValue.run()

      it ("must be a quoted string") {
        val invalidPair = "value"
        testValue(invalidPair).isFailure shouldBe true

        val validPair = "\"value\""
        testValue(validPair).isSuccess shouldBe true
      }
    }
  }
}
