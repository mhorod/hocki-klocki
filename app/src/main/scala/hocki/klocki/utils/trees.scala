package hocki.klocki.utils

def printTree(tree: Tree): Unit = printTree(tree, 0)

private def printTree(tree: Tree, depth: Int): Unit =
  println("  ".repeat(depth) + tree)
  tree.children.foreach(printTree(_, depth + 1))

extension (tree: Tree)
  def toParenthesesString: String =
    val children = tree.children
    if children.isEmpty then
      tree.toString
    else
      val childrenParenthesesString = children.map(_.toParenthesesString).mkString(" ")
      s"($childrenParenthesesString)"