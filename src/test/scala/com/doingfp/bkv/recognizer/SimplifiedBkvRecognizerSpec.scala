package com.doingfp.bkv.recognizer

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{Matchers, FunSpec}


@RunWith(classOf[JUnitRunner])
class SimplifiedBkvRecognizerSpec extends FunSpec with Matchers {
  describe ("Companion object") {
    describe ("Key symbol") {
      it ("should not contain whitespace characters") {
        SimplifiedBkvRecognizer.KeySymbol.matchesAny(" \t\n") shouldBe false
      }

      it ("may contain any alphanumeric symbol, or underscore") {
        SimplifiedBkvRecognizer.KeySymbol.matchesAll("AaZz0123456789_") shouldBe true
      }

      it ("should fail for other printable symbols") {
        SimplifiedBkvRecognizer.BlockNameSymbol.matchesAny(";/#-=(){}[]`!") shouldBe false
      }
    }

    describe ("Value symbol") {
      it ("should not contain whitespace characters") {
        SimplifiedBkvRecognizer.ValueSymbol.matchesAny(" \t\n") shouldBe false
      }

      it ("may contain any printable symbol") {
        SimplifiedBkvRecognizer.ValueSymbol.matchesAny("AaZz09_%^$#;.-+=(){}")
      }
    }

    describe("Block name symbol") {
      it ("should not contain whitespace characters") {
        SimplifiedBkvRecognizer.BlockNameSymbol.matchesAny(" \t\n") shouldBe false
      }

      it ("may contain any alphanumeric symbol, or underscore, or dot") {
        SimplifiedBkvRecognizer.BlockNameSymbol.matchesAll("AaZz0123456789_.") shouldBe true
      }

      it ("should fail for other printable symbols") {
        SimplifiedBkvRecognizer.BlockNameSymbol.matchesAny(";/#-=(){}[]`!") shouldBe false
      }
    }
  }

  describe ("Bkv Recognizer rules") {
    describe("WhiteSpace characters") {
      it ("should fail on empty input") {
        new SimplifiedBkvRecognizer("").WhiteSpace.run().isFailure shouldBe true
      }

      it ("should recognize whitespace char") {
        new SimplifiedBkvRecognizer(" ").WhiteSpace.run().isSuccess shouldBe true
      }

      it ("should recognize \\n as whitespace char") {
        new SimplifiedBkvRecognizer("\n").WhiteSpace.run().isSuccess shouldBe true
      }

      it ("should recognize \\t as whitespace char") {
        new SimplifiedBkvRecognizer("\t").WhiteSpace.run().isSuccess shouldBe true
      }
    }

    // todo: check are they fake?
    describe("Optional whitespaces") {
      describe ("matches zero or more whitespace chars") {
        it ("should match no whitespace chars at all") {
          new SimplifiedBkvRecognizer("").MayBeWS.run().isSuccess shouldBe true
        }

        it ("should match a single whitespace char") {
          new SimplifiedBkvRecognizer(" ").MayBeWS.run().isSuccess shouldBe true
        }

        it ("should match a multiple whitespace chars") {
          new SimplifiedBkvRecognizer("   ").MayBeWS.run().isSuccess shouldBe true
        }

        it ("should match different combinations of whitespace characters") {
          new SimplifiedBkvRecognizer("\\t\\n  ").MayBeWS.run().isSuccess shouldBe true
        }
      }
    }

    describe("Newline") {
      describe("Unix/Linux newline") {
        it ("should match unix/linux newline") {
          new SimplifiedBkvRecognizer("\n").NewLine.run().isSuccess shouldBe true
        }

        it ("should match Windows type of newline") {
          new SimplifiedBkvRecognizer("\r\n").NewLine.run().isSuccess shouldBe true
        }
      }
    }

    //todo: are they fake? add EOI somewhere
    describe("Key") {
      it ("should follow the same rules as identifier") {
//        new BkvRecognizer("valid_Key0123").Key.run().isSuccess shouldBe true
        new SimplifiedBkvRecognizer("invalid Key0123").Key.run().isSuccess shouldBe true
      }
    }

//    describe("Value") {
//      it ("should not allow newlines and whitespaces") {
//        val k =
//        Recognizer("value text").Value.run().isSuccess shouldBe true
//        println(k)
//      }

//      it ("value can be a text") {
        //TODO:
//      }
//    }


//    describe("Key Value pair") {
//
//    }


//    describe("Parser to have fun with") {

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
//    }

  }
}
