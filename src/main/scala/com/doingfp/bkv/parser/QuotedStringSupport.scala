package com.doingfp.bkv.parser

import org.parboiled2._

object QuotedStringSupport {

  /**
   * Look here for the example.
   * https://en.wikipedia.org/wiki/Escape_sequences_in_C#Table_of_escape_sequences
   */
  val CharsToBeEscaped = "abfnrtv\\\""
  val BackSlash = '\\'

  /**
   * All printable characters except double quotation and backslash
   */
  val AllowedChars = CharPredicate.Printable -- BackSlash -- '"'
}


/**
 * Support for strings with double quotations
 * and c-like escaping rules
 */
trait QuotedStringSupport { this: Parser =>
  import QuotedStringSupport._

  def QuotedString: Rule1[String] = rule {
    '"' ~ capture(QuotedStringContent)  ~ '"'
  }

  def QuotedStringRecognizer: Rule0 = rule {
    '"' ~ QuotedStringContent  ~ '"'
  }

  def QuotedStringContent: Rule0 = rule {
    oneOrMore(AllowedChars | DoubleQuotedStringEscapeSequence)
  }

  def DoubleQuotedStringEscapeSequence = rule {
    '\\' ~ anyOf(CharsToBeEscaped)
  }
}
