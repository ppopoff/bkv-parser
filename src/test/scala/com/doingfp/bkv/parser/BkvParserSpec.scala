package com.doingfp.bkv.parser

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSpec, Matchers}


@RunWith(classOf[JUnitRunner])
class BkvParserSpec extends FunSpec with Matchers {
  describe ("Companion object") {
    describe ("Key symbol") {
      it ("should not contain whitespace characters") {
        BkvParser.KeySymbol.matchesAny(" \t\n") shouldBe false
      }

      it ("may contain any alphanumeric symbol, or underscore") {
        BkvParser.KeySymbol.matchesAll("AaZz0123456789_") shouldBe true
      }

      it ("should fail for other printable symbols") {
        BkvParser.BlockNameSymbol.matchesAny(";/#-=(){}[]`!") shouldBe false
      }
    }

    describe("Block name symbol") {
      it ("should not contain whitespace characters") {
        BkvParser.BlockNameSymbol.matchesAny(" \t\n") shouldBe false
      }

      it ("may contain any alphanumeric symbol, or underscore, or dot") {
        BkvParser.BlockNameSymbol.matchesAll("AaZz0123456789_.") shouldBe true
      }

      it ("should fail for other printable symbols") {
        BkvParser.BlockNameSymbol.matchesAny(";/#-=(){}[]`!") shouldBe false
      }
    }
  }

  describe ("Bkv Recognizer rules") {
    describe("WhiteSpace characters") {
      it ("should fail on empty input") {
        new BkvParser("").WhiteSpace.run().isFailure shouldBe true
      }

      it ("should recognize whitespace char") {
        new BkvParser(" ").WhiteSpace.run().isSuccess shouldBe true
      }

      it ("should recognize \\n as whitespace char") {
        new BkvParser("\n").WhiteSpace.run().isSuccess shouldBe true
      }

      it ("should recognize \\t as whitespace char") {
        new BkvParser("\t").WhiteSpace.run().isSuccess shouldBe true
      }
    }

    // todo: add testable class
    describe("Optional whitespaces") {
      describe ("matches zero or more whitespace chars") {
        it ("should match no whitespace chars at all") {
          new BkvParser("").MayBeWS.run().isSuccess shouldBe true
        }

        it ("should match a single whitespace char") {
          new BkvParser(" ").MayBeWS.run().isSuccess shouldBe true
        }

        it ("should match a multiple whitespace chars") {
          new BkvParser("   ").MayBeWS.run().isSuccess shouldBe true
        }

        it ("should match different combinations of whitespace characters") {
          new BkvParser("\\t\\n  ").MayBeWS.run().isSuccess shouldBe true
        }
      }
    }

    describe("Newline") {
      describe("Unix/Linux newline") {
        it ("should match unix/linux newline") {
          new BkvParser("\n").NewLine.run().isSuccess shouldBe true
        }

        it ("should match Windows type of newline") {
          new BkvParser("\r\n").NewLine.run().isSuccess shouldBe true
        }
      }
    }


    describe("Key") {
      it ("should contain only valid symbols") {
        //todo: add testable class
        new BkvParser("valid_Key0123").Root.run().isSuccess shouldBe true
      }

      it ("should fail at whitespaces") {
        new BkvParser("invalid Key0123").Root.run().isFailure shouldBe true
      }
    }

    describe("Value") {
      it ("must be a quoted string") {
        val invalidPair = "key = value"
        new BkvParser(invalidPair).Root.run().isFailure shouldBe true
      }
    }

  }
}
