package com.doingfp.bkv.parser

trait AstNode {
  val name: String
  val isBlockNode: Boolean
}


case class KeyValueNode(override val name: String, value: String) extends AstNode {
  override val isBlockNode = false
}


case class BlockNode(override val name: String, nodes: Seq[AstNode]) extends AstNode {
  override val isBlockNode = true
}
