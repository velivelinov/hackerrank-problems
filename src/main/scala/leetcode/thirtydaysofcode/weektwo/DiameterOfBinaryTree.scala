package leetcode.thirtydaysofcode.weektwo

//Given a binary tree, you need to compute the length of the diameter of the tree.
// The diameter of a binary tree is the length of the longest path between any two nodes in a tree.
// This path may or may not pass through the root.
//
//Example:
//Given a binary tree
//    1
//   / \
//  2   3
// / \
//4   5
//Return 3, which is the length of the path [4,2,1,3] or [5,2,1,3].
//
//Note: The length of path between two nodes is represented by the number of edges between them.

object DiameterOfBinaryTree extends App {
  class TreeNode(var _value: Int) {
    var value: Int = _value
    var left: TreeNode = null
    var right: TreeNode = null
  }

  def diameterOfBinaryTree(root: TreeNode): Int = {

    def helper(root: TreeNode): (Int, Int) = {
      if (root == null) (0, 0)
      else {
        val (leftMax, leftDepth) = helper(root.left)
        val (rightMax, rightDepth) = helper(root.right)

        val diameter = List(
          leftMax,
          rightMax,
          leftDepth + rightDepth
        ).max

        (diameter, 1 + Math.max(leftDepth, rightDepth))
      }
    }

    helper(root)._1
  }
}
