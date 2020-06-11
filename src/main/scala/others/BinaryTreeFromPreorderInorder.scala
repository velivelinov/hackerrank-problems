package others

object BinaryTreeFromPreorderInorder extends App {

  class TreeNode(var _value: Int) {
    var value: Int = _value
    var left: TreeNode = null
    var right: TreeNode = null
   }

  def buildTree(preorder: Array[Int], inorder: Array[Int]): TreeNode = {
    val inorderMap = inorder.zipWithIndex.toMap

    def buildTree(preStart: Int, preEnd: Int, inStart: Int, inEnd: Int): TreeNode =
      if (preStart > preEnd || preStart == inorder.length) null
      else {
        val rootValue = preorder(preStart)
        val root = new TreeNode(rootValue)
        val inOrderIndex = inorderMap(rootValue)
        val leftTreeSize = inOrderIndex - inStart
        root.left = buildTree(preStart + 1, preStart + leftTreeSize, inStart, inOrderIndex - 1)
        root.right = buildTree(preStart + 1 + leftTreeSize, preEnd, inOrderIndex + 1, inEnd)
        root
      }

    buildTree(0, preorder.length - 1, 0, preorder.length - 1)
  }
}
