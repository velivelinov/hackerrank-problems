package leetcode.thirtydaysofcode.weekthree

//Return the root node of a binary search tree that matches the given preorder traversal.
//
//(Recall that a binary search tree is a binary tree where for every node,
// any descendant of node.left has a value < node.val, and any descendant of node.right has a value > node.val.
// Also recall that a preorder traversal displays the value of the node first,
// then traverses node.left, then traverses node.right.)


object BstFromPreorderTraversal extends App {

  class TreeNode(var _value: Int) {
    var value: Int = _value
    var left: TreeNode = null
    var right: TreeNode = null
  }

  def bstFromPreorder(preorder: Array[Int]): TreeNode = {

    def bstFromPreorderList(preorder: List[Int]): TreeNode = {
      preorder match {
        case Nil => null
        case head :: tail =>
          val node = new TreeNode(head)
          val (lower, higher) = tail.partition(_ < head)
          node.left = bstFromPreorderList(lower)
          node.right = bstFromPreorderList(higher)
          node
      }
    }

    bstFromPreorderList(preorder.toList)
  }
}
