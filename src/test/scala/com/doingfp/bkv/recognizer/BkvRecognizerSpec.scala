package com.doingfp.bkv.recognizer

import org.scalatest.{Matchers, FunSpec}
import scala.util.{Failure, Success}


class BkvRecognizerSpec extends FunSpec with Matchers {
  describe ("Bkv Recognizer rules") {
    describe("WhiteSpace characters") {
      it ("should fail on empty input") {
        val p = Parser("\n")

        val rslt = p.test(p.WhiteSpace) match {
          case Failure(e) => println(e)
        }
      }

      it ("should recognize ' ' as whitespace char") {
       val p = Parser(" ")

        val rslt = p.test(p.WhiteSpace) match {
          case Failure(e) => println(e)
        }
      }

      it ("should recognize '\\n' as whitespace char") {
        val p = Parser("\n")

        val rslt = p.test(p.WhiteSpace) match {
          case Failure(e) => println(e)
        }
      }

      it ("should recognize '\\t' as whitespace char") {
        val p = Parser("\t")

        val rslt = p.test(p.WhiteSpace) match {
          case Failure(e) => println(e)
        }
      }
    }

    describe("Optional whitespaces") {
      describe ("matches zero or more whitespace chars") {
        it ("should match no whitespace chars at all") {
//          Recognizer("").OptionalWhiteSpaces.run().isSuccess shouldBe true
        }

        it ("should match a single whitespace char") {
//          Recognizer("\n").OptionalWhiteSpaces.run().isSuccess shouldBe true
        }

        it ("should match different combinations of whitespace characters") {
//          Recognizer("   \n\n").OptionalWhiteSpaces.run().isSuccess shouldBe true
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
//          Recognizer(" ").OptionalWhiteSpaces.run().isSuccess shouldBe true
        }

        it ("should match if many whitespace characters take place") {
//          Recognizer(" \n\t ").OptionalWhiteSpaces.run().isSuccess shouldBe true
        }
      }
    }

    describe("Newline") {
      describe("Unix/Linux newline") {
        it ("should match unix/linux newline") {
//          Recognizer("\n").NewLine.run().isSuccess shouldBe true
        }

        it ("should match Windows type of newline") {
//          Recognizer("\r\n").NewLine.run().isSuccess shouldBe true
        }
      }
    }

    describe("Key") {
      it ("should follow the same rules as identifier") {
//        Recognizer("valid_Key0123").Key.run().isSuccess shouldBe true
      }
    }

    describe("Value") {
      it ("should not allow newlines and whitespaces") {
//        val k =
//        Recognizer("value text").Value.run().isSuccess shouldBe true
//        println(k)
      }

      it ("value can be a text") {
        //TODO:
      }
    }


    describe("Key Value pair") {

    }


    describe("Parser to have fun with") {

      import org.parboiled2._

/*      class ParserToHaveFunWith(val input: ParserInput) {
        def MyRule = rule {
          capture("a") ~ capture("b") ~ capture("c") ~ capture("d") ~ capture("e") ~ capture("f") ~ capture("g") ~
          capture("h") ~ capture("i") ~ capture("j") ~ capture("k") ~ capture("l") ~ capture("m") ~ capture("n") ~
          capture("o") ~ capture("p") ~ capture("q") ~ capture("r") ~ capture("s") ~ capture("t") ~ capture("u") ~
          capture("v") ~ capture("w") ~ capture("x") ~ capture("y") ~ capture("z") ~> (println(_ + _ + _ + _ + _ + _))
        }
      }*/

//      new ParserToHaveFunWith("abcdefghijklmnopqrstuvwxyz").MyRule.run()
    }

  }
}
