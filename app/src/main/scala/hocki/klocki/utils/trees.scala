package hocki.klocki.utils

def printTree(tree: Tree): Unit = printTree(tree, 0)

private def printTree(tree: Tree, depth: Int): Unit =
  println("  ".repeat(depth) + tree)
  tree.children.foreach(printTree(_, depth + 1))