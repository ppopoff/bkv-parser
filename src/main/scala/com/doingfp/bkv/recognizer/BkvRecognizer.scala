package com.doingfp.bkv.recognizer

import org.parboiled2._
import CharPredicate.AlphaNum


class BkvRecognizer(val input: ParserInput) extends Parser {

  def WhiteSpace = rule {
    anyOf(" \n\t")
  }

  def OptionalWhiteSpaces = rule {
    zeroOrMore(WhiteSpace)
  }

  /**
   * At least one whitespace is required
   * Требуется хотя бы один пробел
   */
  def RequiredWhiteSpaces = rule {
    oneOrMore(WhiteSpace)
  }

  def NewLine = rule {
    '\r'.? ~ '\n'
  }

  def Key = rule {
    oneOrMore(AlphaNum ++ "_")
  }

  /**
   * May contain Any character, except whitespace and newline. They are not allowed because:
   *  - Newline separates one pair from another
   *  - Whitespaces may present before the Value,
   *
   * Может содержать Любой (Any) символ за исключением пробела и перевода строки. Данные символы запрещены потому что:
   *  - Перевод строки разделяет одну пару символов от другой
   *  - Множетсво пробелов может предшествовать Значению (Value)
   */
  def Value = rule {
    CharPredicate.Visible.+
  }

  def KeyValuePair = rule {
    Key ~ OptionalWhiteSpaces ~ "=" ~ OptionalWhiteSpaces ~ Value
  }

  /**
   * Rule must be explicitly specified here. In this case it will be Rule0 (we don't extract any value).
   * Правило должно быть явно объявлено здесь. В данном случае это будет Rule0, (так как мы не извлекаем значения).
   * @return
   */
  def Node: Rule0 = rule {
    KeyValuePair | Block
  }

  def Block = rule {
    Identifier ~ OptionalWhiteSpaces ~ "{" ~ oneOrMore(OptionalWhiteSpaces ~ Node).separatedBy(NewLine) ~ "}"
  }


  /**
   * Entry point
   * Точка входа
   */
  def Root = rule {
    oneOrMore(Node).separatedBy(NewLine)
  }
}
