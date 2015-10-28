package com.doingfp.bkv.parser

sealed trait AstNode extends NodeAccessDsl {
  def name: String
}

case class KeyValueNode
(override val name: String, value: String) extends AstNode

case class BlockNode
(override val name: String, nodes: Seq[AstNode]) extends AstNode
