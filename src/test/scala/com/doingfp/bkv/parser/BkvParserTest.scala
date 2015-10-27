package com.doingfp.bkv.parser

import scala.util.Success
import org.junit.runner.RunWith
import org.scalatest.{Matchers, FunSpec}
import org.scalatest.junit.JUnitRunner


@RunWith(classOf[JUnitRunner])
class BkvParserTest extends FunSpec with Matchers {
  describe("Bkv format parser") {
    describe("The entry point (Root rule)") {
      it ("should handle nested key-value node from Root") {
        val text = """ key = "value" """
        val parsingResult = new BkvParser(text).Root.run()

        parsingResult.isSuccess shouldBe true
        parsingResult.get should have length 1
        parsingResult.get.head shouldBe KeyValueNode("key", "value")
      }

      it ("should handle nested block node inside Root") {
        val text =
          """|nodename {
             |  key="value"
             |}""".stripMargin

        val parsingResult = new BkvParser(text).Root.run()

        parsingResult.isSuccess shouldBe true
        parsingResult.get should have length 1
        parsingResult.get.head shouldBe BlockNode("nodename", Vector(KeyValueNode("key", "value")))
      }
    }

    describe("The list of nodes (Nodes rule)") {
      it ("shoud accept all known type of nodes") {
        val text =
          """|key1="value1"
             |nodename {
             |  key2="value2"
             |}
             |key3="value3"""".stripMargin

        val parsingResult = new BkvParser(text).Root.run()

        parsingResult.isSuccess shouldBe true
        parsingResult.get should have length 3
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
        parsingResult.get should have length 3
        parsingResult.get(1).isBlockNode shouldBe true
        parsingResult.get(2).isBlockNode shouldBe false
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
        parsingResult.get should have length 3
        parsingResult.get(1).isBlockNode shouldBe true
        parsingResult.get(2).isBlockNode shouldBe false
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

        val withoutWhitespaces = new BkvParser(kvWithoutWhitespaces).Root.run()
        val withWhitespaces = new BkvParser(kvWithWhitespacesBetween).Root.run()

        withoutWhitespaces.get.head shouldBe withWhitespaces.get.head
      }

      it ("should handle possible indentations before kv pair definition") {
        val withTabIndent = "\tindentation = \"true\""
        val withMultipleTabIndent = "\t\t\t\tindentation = \"true\""
        val withSpaceIndent = "    indentation = \"true\""
        val withNewLineIndent = "\nindentation = \"true\""

        val expectedResult = Success(
          Vector(KeyValueNode("indentation", "true"))
        )

        new BkvParser(withTabIndent).Root.run() should be (expectedResult)
        new BkvParser(withMultipleTabIndent).Root.run() should be (expectedResult)
        new BkvParser(withSpaceIndent).Root.run() should be (expectedResult)
        new BkvParser(withNewLineIndent).Root.run() should be (expectedResult)
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
        parsingResult.get should have length 2
        parsingResult.get should equal (expectedAst)
      }
    }

    describe("Testing Block node") {
      it ("should handle key value pair on the first line") {
        val block = """|block_name { visible = "true"
                       |}""".stripMargin

        val parsingResult = new BkvParser(block).Root.run()

        parsingResult.isSuccess shouldBe true
        parsingResult.get should have length 1

        val expectedNode = BlockNode("block_name", Vector(KeyValueNode("visible", "true")))
        parsingResult.get.head should equal (expectedNode)
      }

      it ("should handle key value pair on the last line near the closing bracket") {
        val block =
          """
            |name {
            | key = "value"}
          """.stripMargin

        val parsingResult = new BkvParser(block).Root.run()
        parsingResult.isSuccess shouldBe true
        parsingResult.get should have length 1
        parsingResult.get.head match {
          case BlockNode(name, values) => values.head match {
            case KeyValueNode(k, v) =>
              k shouldBe "key"
              v shouldBe "value"
          }
        }
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
        parsingResult.get.head match {
          case BlockNode(name, values) =>
            values should have length 3
        }
      }

      it ("should parse single line block") {
        val singleLineBlock = "block { key = \"value\" }"
        val parsingResult = new BkvParser(singleLineBlock).Root.run()

        parsingResult.isSuccess shouldBe true
        parsingResult.get should have size 1
        parsingResult.get.head match {
          case BlockNode(blockName, nestedNodes) =>
            blockName shouldBe "block"
            nestedNodes should have length 1
            nestedNodes.head should equal (KeyValueNode("key", "value"))
        }
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

        // There should be only one block
        parsingResult.get should have size 1

        parsingResult.get.head match {
          case BlockNode(name, nodes) =>
            name shouldBe "block"
            nodes should have length 1

            // Nested block level 1
            nodes.head match {
              case BlockNode(nestedName, nestedNodes) =>
                nestedName shouldBe "nested_block"
                nestedNodes should have length 1

                nestedNodes.head shouldBe KeyValueNode("can_be_nested", "true")
            }
        }
      } // nested blocks test

    } // testing Block node
  }
}
