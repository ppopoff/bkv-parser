package com.doingfp.bkv.parser

trait NodeAccessDsl { this: AstNode =>


  def get(nodename: String): Option[AstNode] = this match {
    case BlockNode(name, values) =>
      values.find(_.name == nodename)
    case KeyValueNode(name, value) =>
      None
  }

  def / (nodename: String) = get(nodename)
}
