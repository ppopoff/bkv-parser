package com.doingfp.bkv.parser

trait NodeAccessDsl { this: AstNode =>

  def isRoot = this.name == BkvParser.RootNodeName

  lazy val isKeyValuePair = !isBlockNode
  lazy val isBlockNode = this match {
    case _: KeyValueNode => false
    case _ => true
  }


  /**
   * @return a sequence of nested key value pairs
   */
  def pairs: Seq[KeyValueNode] = this match {
    case BlockNode(_, nodes) =>
      nodes collect { case node: KeyValueNode => node }
    case _ => Seq.empty
  }


  /**
   * @param key of the blocks we need
   * @return a sequence of key-value pair nodes that have
   *         specified name
   */
  def pairs(key: String): Seq[KeyValueNode] =
    pairs filter (_.name == key)


  /**
   * @param key of the pair we're looking for
   * @return the first pair with specified name
   *         (if exists)
   */
  def pair(key: String): Option[KeyValueNode] =
    pairs find (_.name == key)


  /**
   * @return a sequence of nested block nodes
   */
  def blocks: Seq[BlockNode] = this match {
    case BlockNode(_, nodes) =>
      nodes collect { case node: BlockNode => node }
    case _ => Seq.empty
  }


  /**
   * @param name - name of the blocks we need
   * @return a sequence of block nodes that have specified name
   */
  def blocks(name: String): Seq[BlockNode] =
    blocks filter (_.name == name)


  /**
   * @param name - of the sought-for block
   * @return the first block with specified name
   *         (if exists)
   */
  def block(name: String): Option[BlockNode] =
    blocks find (_.name == name)


  /**
   * @return value, if key-value pair
   */
  def getValue: Option[String] = this match {
    case KeyValueNode(_, value) => Some(value)
    case _ => None
  }
}
