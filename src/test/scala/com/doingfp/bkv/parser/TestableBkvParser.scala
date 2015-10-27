package com.doingfp.bkv.parser

import org.parboiled2.{Rule1, ParserInput}

/**
 * Testable version of BkvParser.
 * (EOI was added to some rules to make them matchable)
 * @param input - parser input
 */
class TestableBkvParser(input: ParserInput) extends BkvParser(input) {
  def TestableNewLine = rule {
    NewLine ~ EOI
  }

  def TestableWhitespace = rule {
    WhiteSpace ~ EOI
  }

  def TestableOptionalWhitespaces = rule {
    MayBeWS ~ EOI
  }

  def TestableKey: Rule1[String] = rule {
    Key ~ EOI
  }

  def TestableValue = rule {
    Value ~ EOI
  }

  def TestableBlockName = rule {
    BlockName ~ EOI
  }
}
