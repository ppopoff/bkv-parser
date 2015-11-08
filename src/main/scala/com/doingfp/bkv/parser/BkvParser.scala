package com.doingfp.bkv.parser

import org.parboiled2._

/**
 * Constants
 */
object BkvParser {

  /**
   * Name of the root node. It is empty string.
   * Nice placeholder for a name, and impossible for users to set.
   * because identifier can not be an empty string.
   */
  val RootNodeName = ""

  val WhitespaceChars = "\n\t "

  /**
   * Identifier must start only with a letter or underscore,
   * and contain latin letters, digits, underscore and dots
   */
  val IdentifierFirstChar = CharPredicate.Alpha ++ '_'
  val IdentifierChar      = CharPredicate.AlphaNum ++ '.' ++ '_'

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

  def OptWs = rule {
    zeroOrMore(WhiteSpace)
  }

  def Newline = rule {
    optional('\r') ~ '\n'
  }

  def Identifier = rule {
    IdentifierFirstChar ~ zeroOrMore(IdentifierChar)
  }

  def Key = rule {
    capture(Identifier)
  }

  def Value: Rule1[String] = rule {
    QuotedString
  }

  def KeyValuePair: Rule1[AstNode] = rule {
    Key ~ OptWs ~ "=" ~ OptWs ~ Value ~> KeyValueNode
  }

  def Block: Rule1[AstNode] = rule {
    BlockName ~ OptWs ~ BlockBeginning ~ Nodes ~ BlockEnding ~> BlockNode
  }

  def BlockName = rule {
    capture(Identifier)
  }

  // Recursive call. Type MUST be specified
  def Node: Rule1[AstNode] = rule {
    KeyValuePair | Block
  }

  def Nodes: Rule1[Seq[AstNode]] = rule {
    OptWs ~
      zeroOrMore(Node).separatedBy(Newline ~ OptWs) ~
    OptWs
  }

  def Root: Rule1[AstNode] = rule {
    Nodes ~ EOI ~> {nodes: Seq[AstNode] => BlockNode(RootNodeName, nodes)}
  }
}
