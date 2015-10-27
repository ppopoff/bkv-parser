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
    capture(KeySymbol.+)
  }

  def Value: Rule1[String] = rule {
    QuotedString
  }

  def KeyValuePair: Rule1[AstNode] = rule {
    Key ~ MayBeWS ~ "=" ~ MayBeWS ~ Value ~> KeyValueNode
  }

  def Block: Rule1[AstNode] = rule {
    BlockName ~ MayBeWS ~ BlockBeginning ~ Nodes ~ BlockEnding ~> BlockNode
  }

  def BlockName = rule {
    capture(BlockNameSymbol.+)
  }

  // Recursive call. Type MUST be specified
  def Node: Rule1[AstNode] = rule {
    KeyValuePair | Block
  }

  def Nodes: Rule1[Seq[AstNode]] = rule {
    MayBeWS ~
      zeroOrMore(Node).separatedBy(NewLine ~ MayBeWS) ~
    MayBeWS
  }

  def Root: Rule1[Seq[AstNode]] = rule {
    MayBeWS ~ Nodes ~ MayBeWS ~ EOI
  }
}
