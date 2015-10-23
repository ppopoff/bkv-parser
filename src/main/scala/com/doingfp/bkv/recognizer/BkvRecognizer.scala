package com.doingfp.bkv.recognizer

import org.parboiled2._

object BkvRecognizer {
  val WhitespaceChars = "\n\t "
  val Identifier      = CharPredicate.AlphaNum ++ '.' ++ '_'
  val ValueSymbol     = CharPredicate.Visible
  val KeySymbol       = Identifier
  val BlockNameSymbol = Identifier
  val BlockBeginning  = '{'
  val BlockEnding     = '}'
}

class BkvRecognizer(val input: ParserInput) extends Parser {
  import BkvRecognizer._

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
    oneOrMore(ValueSymbol)
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
