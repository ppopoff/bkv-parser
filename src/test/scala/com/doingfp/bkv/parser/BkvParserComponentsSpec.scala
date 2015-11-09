package com.doingfp.bkv.parser

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSpec, Matchers}


/**
 * Tests parser internals
 *  - whitespace recognition
 *  - optional whitespace's behaviour
 *  - newline recognition
 *  - key and block name
 */
@RunWith(classOf[JUnitRunner])
class BkvParserComponentsSpec extends FunSpec with Matchers {
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

    describe("Identifier the same rules for both Block name and Key") {
      def testIdentifier(input: String) =
        new TestableBkvParser(input).TestableIdentifier.run()

      describe("the first character of the identifier") {
        describe("Allowed chars") {
          it ("can start with underscore char") {
            testIdentifier("_validId").isSuccess shouldBe true
          }

          it ("can start with any alphabetic character") {
            testIdentifier("isValidIdentifier").isSuccess shouldBe true
            testIdentifier("ValidIdentifier").isSuccess shouldBe true
          }
        }

        describe("Forbidden chars") {
          it ("should not start with number") {
            testIdentifier("12wrongId").isFailure shouldBe true
          }

          it ("should not start with dot character") {
            testIdentifier(".wrong_id").isFailure shouldBe true
          }

          it ("should start only with letters") {
            testIdentifier("$wrong_id").isFailure shouldBe true
            testIdentifier("#wrong_id").isFailure shouldBe true
            testIdentifier("%wrong_id").isFailure shouldBe true
            testIdentifier("!wrong_id").isFailure shouldBe true
          }
        }
      }

      describe("Identifier allowed characters") {
        it ("may contain dot symbol is allowed from the 2nd character") {
          testIdentifier("valid.id").isSuccess shouldBe true
          testIdentifier("v.alid_id").isSuccess shouldBe true
        }

        it ("may contain digits from the second character") {
          testIdentifier("valid1_d3nt1fier").isSuccess shouldBe true
        }
      }

      describe("Identifier forbidden characters") {
        it ("should not contain whitespace chars") {
          testIdentifier("has spaces").isFailure shouldBe true
          testIdentifier("has\nnewline").isFailure shouldBe true
          testIdentifier("has\ttabulation").isFailure shouldBe true
        }
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
