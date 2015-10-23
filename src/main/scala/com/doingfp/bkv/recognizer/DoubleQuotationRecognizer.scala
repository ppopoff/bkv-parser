package com.doingfp.bkv.recognizer

import org.parboiled2._

/**
 * Support for strings with double quotations
 * and c-like escaping rules
 */
trait DoubleQuotationRecognizer { this: Parser =>

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

  def DoubleQuotedString: Rule0 = rule {
    '"' ~ oneOrMore(AllowedChars | DoubleQuotedStringEscapeSequence) ~ '"'
  }

  def DoubleQuotedStringEscapeSequence = rule {
    '\\' ~ anyOf(CharsToBeEscaped)
  }
}
