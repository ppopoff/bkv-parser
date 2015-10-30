package com.doingfp.bkv.parser

import org.junit.runner.RunWith
import org.scalatest.{Matchers, FunSpec}
import org.scalatest.junit.JUnitRunner


/**
 * Tests parser as whole system. Parser components are tested
 * in BkvParserSpec
 * @author ppopoff
 */
@RunWith(classOf[JUnitRunner])
class BkvParserTest extends FunSpec with Matchers {
  describe("Bkv format parser") {
    describe("The entry point (Root rule)") {
      it ("should handle nested key-value node from Root") {
        val text = """ key = "value" """
        val parsingResult = new BkvParser(text).Root.run()

        parsingResult.isSuccess shouldBe true

        val root = parsingResult.get
        root.pairs should have length 1
        root.blocks should have length 0
        root.pairs.head shouldBe KeyValueNode("key", "value")
      }

      it ("should handle nested block node inside Root") {
        val text =
          """|nodename {
             |  key="value"
             |}""".stripMargin

        val parsingResult = new BkvParser(text).Root.run()

        parsingResult.isSuccess shouldBe true

        val root = parsingResult.get
        root.blocks should have length 1
        root.pairs should have length 0

        val keyValuePair = root.blocks.head.pair("key").get
        keyValuePair shouldBe KeyValueNode("key", "value")
      }
    }

    describe("The list of nodes (Nodes rule)") {
      it ("should accept all known types of nodes") {
        val text =
          """|key1="value1"
             |nodename {
             |  key2="value2"
             |}
             |key3="value3"""".stripMargin

        val parsingResult = new BkvParser(text).Root.run()

        parsingResult.isSuccess shouldBe true

        val root = parsingResult.get

        root.blocks should have size 1
        root.pairs  should have size 2
      }

      it ("should handle preceding and trailing whitespaces") {
        val text =
          """|
             |key1="value1"
             |nodename {
             |  key2="value2"
             |}
             |key3="value3"
             |
             |""".stripMargin

        val parsingResult = new BkvParser(text).Root.run()

        parsingResult.isSuccess shouldBe true
        val root = parsingResult.get

        root.blocks should have size 1
        root.pairs  should have size 2
      }

      it ("should handle spaces between nodes") {
        val text =
          """|key1="value1"
             |
             |nodename {
             |  key2="value2"
             |}
             |
             |key3="value3"
             |""".stripMargin

        val parsingResult = new BkvParser(text).Root.run()

        parsingResult.isSuccess shouldBe true
        val root = parsingResult.get

        root.blocks should have size 1
        root.pairs  should have size 2
      }

      it ("should accept Newline as the ONLY separator between Nodes") {
        val invalidSequence = "key1 = \"value 1\" key2 = \"value2\""
        new BkvParser(invalidSequence).Root.run().isFailure shouldBe true
      }
    }

    describe("Testing key value node") {
      it ("should be no difference whether we have whitespaces or not") {
        val kvWithoutWhitespaces = "key=\"value\""
        val kvWithWhitespacesBetween = "key = \"value\""

        val withoutWhitespaces =
          new BkvParser(kvWithoutWhitespaces).Root.run().get

        val withWhitespaces =
          new BkvParser(kvWithWhitespacesBetween).Root.run().get

        withoutWhitespaces.pairs shouldBe withWhitespaces.pairs
      }

      it ("should handle possible indentations before kv pair definition") {
        val withTabIndent = "\tindentation = \"true\""
        val withMultipleTabIndent = "\t\t\t\tindentation = \"true\""
        val withSpaceIndent = "    indentation = \"true\""
        val withNewLineIndent = "\nindentation = \"true\""

        val expectedResult = KeyValueNode("indentation", "true")

        def getFirstKeyNodePair(text: String): KeyValueNode =
          new BkvParser(text).Root.run().get.pairs.head

        val astWithTabIndent = getFirstKeyNodePair(withTabIndent)
        astWithTabIndent should be (expectedResult)

        val astWithMultipleTabIndent = getFirstKeyNodePair(withMultipleTabIndent)
        astWithMultipleTabIndent should be (expectedResult)

        val astWithSpaceIndent = getFirstKeyNodePair(withSpaceIndent)
        astWithSpaceIndent should be (expectedResult)

        val astWithNewLineIndent = getFirstKeyNodePair(withNewLineIndent)
        astWithNewLineIndent should be (expectedResult)
      }

      it ("should be possible to move value to the next line") {
        val kvWithEmbeddedNewline =
          """
            |key1 =
            |  "value1"
            |
            |key2
            | = "value2"
          """.stripMargin

        val parsingResult = new BkvParser(kvWithEmbeddedNewline).Root.run()

        val expectedAst =
          Vector(
            KeyValueNode("key1","value1"),
            KeyValueNode("key2","value2")
          )

        parsingResult.isSuccess shouldBe true
        val root = parsingResult.get

        root.pairs should have length 2
        root.blocks should have length 0

        root.pairs should equal (expectedAst)
      }
    }

    describe("Testing Block node") {
      it ("should handle key value pair on the first line") {
        val block = """|block_name { visible = "true"
                       |}""".stripMargin

        val parsingResult = new BkvParser(block).Root.run()

        parsingResult.isSuccess shouldBe true
        val root = parsingResult.get
        val nestedNodeContent = root.block("block_name") flatMap { block =>
          block pair "visible"
        }

        nestedNodeContent shouldBe Some(KeyValueNode("visible", "true"))
      }

      it ("should handle key value pair on the last line near the closing bracket") {
        val block =
          """
            |name {
            | key = "value"}
          """.stripMargin

        val parsingResult = new BkvParser(block).Root.run()
        parsingResult.isSuccess shouldBe true

        val root = parsingResult.get
        val pairs = root.block("name").get.pairs

        pairs should have length 1
        pairs.head should be (KeyValueNode("key", "value"))
      }

      it ("should handle multiple nested nodes inside block") {
        val block =
          """
            |name {
            | key1 = "value1"
            | key2 = "value2"
            | key3 = "value3"
            |}
          """.stripMargin

        val parsingResult = new BkvParser(block).Root.run()
        parsingResult.isSuccess shouldBe true

        val root = parsingResult.get

        root.block("name").get.pairs should have length 3
      }

      it ("should parse single line block definition") {
        val singleLineBlock = "block { key = \"value\" }"
        val parsingResult = new BkvParser(singleLineBlock).Root.run()

        parsingResult.isSuccess shouldBe true
        val root = parsingResult.get
        val nestedNode = root.block("block").get.pair("key").get
        nestedNode should be (KeyValueNode("key", "value"))
      }

      it ("should handle nested blocks") {
        val singleLineBlock =
          """
            |block {
            |    nested_block {
            |       can_be_nested = "true"
            |    }
            |}
          """.stripMargin
        val parsingResult = new BkvParser(singleLineBlock).Root.run()

        parsingResult.isSuccess shouldBe true
        val root = parsingResult.get

        val nestedKeyValuePair =
            root.block("block").get.block("nested_block").get.pair("can_be_nested").get

        nestedKeyValuePair should equal (KeyValueNode("can_be_nested", "true"))
      }
    }
  }
}
