package com.doingfp.bkv.recognizer

import org.parboiled2._

/**
 * Support for strings with double quotations
 * and c-like escaping rules
 */
trait DoubleQuotationRecognizer { this: Parser =>

  /**
   * Look here for the example. There's no need to escape single quotation, but
   * I found useful to escape unicode characters in this manner '\u1234'.
   * That's why u letter was added.
   * https://en.wikipedia.org/wiki/Escape_sequences_in_C#Table_of_escape_sequences
   */
  val CharsToBeEscaped = "abfnrtv\\\"u"
  val AllowedChars = CharPredicate.Printable -- '"' -- '\\'

  def DoubleQuotedString: Rule0 = rule {
    '"' ~ DoubleQuotedStringBody ~ '"'
  }

  def DoubleQuotedStringEscapeSequence = rule {
    '\\' ~ anyOf(CharsToBeEscaped)
  }

  def DoubleQuotedStringBody = rule {
    oneOrMore(AllowedChars | DoubleQuotedStringEscapeSequence)
  }
}
