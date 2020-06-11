package others.sortandsearch

object ValidateBinarySearchTree extends App {

    class TreeNode(var _value: Int) {
      var value: Int = _value
      var left: TreeNode = null
      var right: TreeNode = null
    }

    def isValidBST(root: TreeNode): Boolean = {

      def helper(root: TreeNode, min: Long, max: Long): Boolean =
        root match {
          case null => true
          case r =>
            r.value < max && r.value > min && helper(r.left, min, r.value) &&
              helper(r.right, r.value, max)
        }

      helper(root, Long.MinValue, Long.MaxValue)
    }

  val leftTree = new TreeNode(1)
  val rightTreeLeft = new TreeNode(3)
  val rightTreeRight = new TreeNode(6)
  val rightTree = new TreeNode(4)
  rightTree.left = rightTreeLeft
  rightTree.right = rightTreeRight
  val tree = new TreeNode(5)
  tree.left = leftTree
  tree.right = rightTree

  println(isValidBST(tree))

}
