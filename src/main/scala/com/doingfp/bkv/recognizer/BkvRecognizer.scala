package com.doingfp.bkv.recognizer

import org.parboiled2._

class BkvRecognizer(val input: ParserInput) extends Parser {

  val WhitespaceChars = "\n\t "
  val KeySymbol = CharPredicate.AlphaNum ++ '_'

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

  def ValueChar = rule {
    noneOf(WhitespaceChars)
  }

  def Value = rule {
    oneOrMore(ValueChar)
  }

  def KeyValuePair = rule {
    Key ~ MayBeWS ~ "=" ~ MayBeWS ~ Value
  }

  def KeyValueLine = rule {
    MayBeWS ~ KeyValuePair ~ MayBeWS
  }

  def Node: Rule0 = rule {
    KeyValueLine | Block
  }

  def Nodes = rule {
    oneOrMore(Node).separatedBy(NewLine)
  }

  def BlockName = rule {
    MayBeWS ~ Key ~ MayBeWS
  }

  def Block = rule {
    BlockName ~ "{" ~ Nodes ~ "}" ~ MayBeWS
  }

  def Root = rule {
    Nodes
  }
}
