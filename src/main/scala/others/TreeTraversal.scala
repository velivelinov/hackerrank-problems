package others

case class Tree(root: Node)
case class Node(value: String, left: Option[Node], right: Option[Node])

object Tree {

  // leaves
  val nodeTwo = Some(Node("2", None, None))
  val nodeSix = Some(Node("6", None, None))
  val nodeFifteen = Some(Node("15", None, None))
  val nodeTwenty = Some(Node("20", None, None))

  //branches
  val nodeFour = Some(Node("4", nodeTwo, nodeSix))
  val nodeTen = Some(Node("10", nodeFifteen, nodeTwenty))

  //root
  val nodeEight = Some(Node("8", nodeFour, nodeTen))
}

object TreeTraversal extends App {
  import Tree._

  def inOrderTraversal(root: Option[Node]): Unit = {
    root match {
      case Some(node) =>
        inOrderTraversal(node.left)
        println(node.value)
        inOrderTraversal(node.right)
      case None => {}
    }
  }

  def preOrderTraversal(root: Option[Node]): Unit = {
    root match {
      case Some(node) =>
        println(node.value)
        preOrderTraversal(node.left)
        preOrderTraversal(node.right)
      case None => {}
    }
  }

  def postOrderTraversal(root: Option[Node]): Unit = {
    root match {
      case Some(node) =>
        postOrderTraversal(node.left)
        postOrderTraversal(node.right)
        println(node.value)
      case None => {}
    }
  }

  println(inOrderTraversal(nodeEight))
  println(preOrderTraversal(nodeEight))
  println(postOrderTraversal(nodeEight))

}
