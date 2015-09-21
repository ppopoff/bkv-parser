package com.doingfp.bkv.recognizer

import org.parboiled2._

class TestParser(input: ParserInput) extends BkvRecognizer(input) {
  type RuleToTest = () => Rule0

  /**
   * Runs the
   */
  def Test(inner: RuleToTest) = rule {
    inner() ~ EOI
  }

}
