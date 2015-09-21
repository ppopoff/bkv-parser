package com.doingfp.bkv.recognizer

import org.parboiled2._

/**
 * Wrapper for BKVRecognizer to test it easily
 */
trait Testable { this: Parser =>

  /**
   * Rule that adds EOI to testable input
   */
  def Test(inner: RuleToTest) = rule {
    inner() ~ EOI
  }

  /**
   * Runs test for the rule
   * @param ruleToTest rule you want to test
   */
  def test(ruleToTest: Rule0) = {
    this.Test(() => ruleToTest).run()
  }
}

object Parser {
 def apply(input: String) =
   new BkvRecognizer(input) with Testable
}
