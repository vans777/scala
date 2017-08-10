abstract sealed class VarArgsTree
case class Leaf(value: Int) extends VarArgsTree
case class ExtNode(f:(Int,Int) => Int, childs: VarArgsTree*) extends VarArgsTree {

  def plus(v1 : Int, v2: Int) : Int = {
    v1 + v2
  }

}
//case class Node(left: BinaryTree, right: BinaryTree) extends BinaryTree

object VarArgsTree {

  def leafSum(f:(Int,Int) => Int, tree: VarArgsTree*): Int = {
    tree.toList match {
      case Nil => 1
      case head :: tail =>
        head match {
          case leaf: Leaf => f(leaf.value, leafSum(f, tail: _*))
//          case en: ExtNode => leafSum(en.childs: _*) * leafSum(tail: _*)
          case en: ExtNode => en.f(leafSum(f, en.childs: _*), leafSum(f, tail: _*))
        }
    }
  }

  def leafSum(tree: VarArgsTree*): Int = {
    tree.toList match {
      case Nil => 1
      case head :: tail =>
        head match {
          case leaf: Leaf => leaf.value * leafSum(tail: _*)
//          case en: ExtNode => leafSum(en.childs: _*) * leafSum(tail: _*)
          case en: ExtNode => en.f(leafSum(en.childs: _*), leafSum(tail: _*))
        }
    }
  }

  //  def leafSum2(binaryTree: BinaryTree*): Int = {
  //    binaryTree match {
  //      case Nil => 0
  //      case leaf: Leaf => leaf.value
  //      case en : ExtNode => leafSum2(en.childs:_*)
  //      case seq : Seq[BinaryTree] => leafSum2(seq.head) + leafSum2(seq.tail:_*)
  //    }
  //  }

  //  @tailrec
//  def leafSum(tree: VarArgsTree*): Int = {
//    if (tree.isEmpty) return 0
//    val head = tree.head
//    val tail = tree.tail
//    head match {
//      case leaf: Leaf => leaf.value + leafSum(tail:_*)
//      case en : ExtNode => leafSum(en.childs:_*) + leafSum(tail:_*)
//      //      case _ => leafSum(head.asInstanceOf[ExtNode].childs:_*) + leafSum(tail:_*)
//    }
//  }

}
