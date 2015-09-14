package com.doingfp.bkv.recognizer

import org.scalatest.{Matchers, FunSpec}

import scala.util.Success


/**
 * @author ppopov
 */
class BkvRecognizerSpec extends FunSpec with Matchers {
  describe ("Bkv Recognizer rules") {

    def Recognizer(s: String) = new BkvRecognizer(s)

    describe("WhiteSpace characters") {
      it ("should recognize ' ' as whitespace char") {
        Recognizer(" ").WhiteSpace.run().isSuccess shouldBe true
      }

      it ("should recognize '\\n' as whitespace char") {
        Recognizer("\n").WhiteSpace.run().isSuccess shouldBe true
      }

      it ("should recognize '\\t' as whitespace char") {
        Recognizer("\t").WhiteSpace.run().isSuccess shouldBe true
      }
    }

    describe("Optional whitespaces") {
      describe ("matches zero or more whitespace chars") {
        it ("should match no whitespace chars at all") {
          Recognizer("").OptionalWhiteSpaces.run().isSuccess shouldBe true
        }

        it ("should match a single whitespace char") {
          Recognizer("\n").OptionalWhiteSpaces.run().isSuccess shouldBe true
        }

        it ("should match different combinations of whitespace characters") {
          Recognizer("   \n\n").OptionalWhiteSpaces.run().isSuccess shouldBe true
        }
      }
    }

    describe("Required Whitespaces") {
      describe("matches one or more whitespaces") {
        it ("shouldn't match zero whitespaces") {
//          val recognizer = new BkvRecognizer("")
//TODO
//          recognizer.OptionalWhiteSpaces.run().isFailure shouldBe true
        }

        it ("should match if one whitespace took place") {
          Recognizer(" ").OptionalWhiteSpaces.run().isSuccess shouldBe true
        }

        it ("should match if many whitespace characters take place") {
          Recognizer(" \n\t ").OptionalWhiteSpaces.run().isSuccess shouldBe true
        }
      }
    }

    describe("Newline") {
      describe("Unix/Linux newline") {
        it ("should match unix/linux newline") {
          Recognizer("\n").NewLine.run().isSuccess shouldBe true
        }

        it ("should match Windows type of newline") {
          Recognizer("\r\n").NewLine.run().isSuccess shouldBe true
        }
      }
    }

    describe("Identifier") {
      it ("contains one or more alphanumeric character or underscore") {
        Recognizer("valid_Identifier0123").Identifier.run().isSuccess shouldBe true
      }
    }

    describe("Key") {
      it ("should follow the same rules as identifier") {
        Recognizer("valid_Key0123").Key.run().isSuccess shouldBe true
      }
    }

    describe("Value") {
      it ("should not allow newlines and whitespaces") {
        val k =
        Recognizer("value text").Value.run().isSuccess shouldBe true
        println(k)
      }

      it ("value can be a text") {
        //TODO:
      }
    }


    describe("Key Value pair") {

    }

  }
}
