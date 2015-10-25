package com.doingfp.bkv.parser

import org.parboiled2._

/**
 * Constants
 */
object BkvParser {
  val WhitespaceChars = "\n\t "
  val Identifier      = CharPredicate.AlphaNum ++ '.' ++ '_'
  val KeySymbol       = Identifier
  val BlockNameSymbol = Identifier
  val BlockBeginning  = '{'
  val BlockEnding     = '}'
}

/**
 * A recognizer for BKV's grammar
 *
 * Recognizer != Parser. It doesn't extract any data.
 * It just matches the input
 */
class BkvParser(val input: ParserInput) extends Parser with QuotedStringSupport {
  import BkvParser._

  def WhiteSpace = rule {
    anyOf(WhitespaceChars)
  }

  def MayBeWS = rule {
    zeroOrMore(WhiteSpace)
  }

  def NewLine = rule {
    optional('\r') ~ '\n'
  }

  def Key = rule {
    oneOrMore(KeySymbol)
  }

  def Value = rule {
    DoubleQuotedString
  }

  def KeyValuePair = rule {
    Key ~ MayBeWS ~ "=" ~ MayBeWS ~ Value
  }

  def Block = rule {
    BlockName ~ MayBeWS ~ BlockBeginning ~ Nodes ~ BlockEnding
  }

  def BlockName = rule {
    oneOrMore(BlockNameSymbol)
  }

  // Recursive call. Type MUST be specified
  def Node: Rule0 = rule {
    KeyValuePair | Block
  }

  def Nodes = rule {
    MayBeWS ~
      zeroOrMore(Node).separatedBy(NewLine ~ MayBeWS) ~
    MayBeWS
  }

  def Root = rule {
    MayBeWS ~ Nodes ~ MayBeWS ~ EOI
  }
}