package com.doingfp.bkv.parser

sealed trait AstNode {
  def name: String
  def isBlockNode: Boolean
}


case class KeyValueNode(override val name: String, value: String) extends AstNode {
  override val isBlockNode = false
}


case class BlockNode(override val name: String, nodes: Seq[AstNode]) extends AstNode {
  override val isBlockNode = true
}
