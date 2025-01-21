package hocki.klocki.utils

import scala.annotation.tailrec

trait Tree:
  def children: List[Tree]
