package com.doingfp.bkv.parser

import org.parboiled2._
import org.junit.runner.RunWith
import org.scalatest.{Matchers, FunSpec}
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ParserTest extends FunSpec with Matchers {
  describe("Bkv format parser") {

    describe("Testing the Root") {
      it ("should be possible to nest key-value nodes inside Root") {
        val text = """ key = "value" """
        val parsingResult = new BkvParser(text).Root.run()

        parsingResult.isSuccess shouldBe true
        parsingResult.get should have length 1
        parsingResult.get.head shouldBe KeyValueNode("key", "value")
      }

      it ("should be possible to nest block node inside Root") {
        val text =
          """|nodename {
             |  key = "value"
             |}""".stripMargin

        val parsingResult = new BkvParser(text).Root.run()

        parsingResult.isSuccess shouldBe true
        parsingResult.get should have length 1
        parsingResult.get.head shouldBe BlockNode("nodename", Vector(KeyValueNode("key", "value")))
      }

      it ("should be possible to have mixed nodes blocks and key value nodes") {
        val text =
          """|key1 = "value1"
             |nodename {
             |  key2 = "value2"
             |}
             |key3 = "value3"""".stripMargin

        val parsingResult = new BkvParser(text).Root.run()

        parsingResult.isSuccess shouldBe true
        parsingResult.get should have length 3
      }

      it ("should support preceding and trailing whitespaces") {
        val text =
          """|
             |key1 = "value1"
             |nodename {
             |  key2 = "value2"
             |}
             |key3 = "value3"
             |
             |""".stripMargin

        val parsingResult = new BkvParser(text).Root.run()

        parsingResult.isSuccess shouldBe true
        parsingResult.get should have length 3
      }
    }
  }
}
