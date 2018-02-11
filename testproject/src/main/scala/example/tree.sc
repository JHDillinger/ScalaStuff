sealed trait Tree[A]{
  def fold[B](node: (B,B) =>B, leaf:A=> B):B =
    this match {
      case Leaf(el) => leaf(el)
      case Node(left,right) => node(left.fold(node,leaf), right.fold(node,leaf))
    }

}

final case class Leaf[A](el:A) extends Tree[A]{
//  def fold[B](node: (B, B) => B, leaf: A => B): B =
//    leaf(value)
}
final case class Node[A](left:Tree[A], right:Tree[A]) extends Tree[A]{
//  def fold[B](node: (B, B) => B, leaf: A => B): B =
//    node(left.fold(node, leaf), right.fold(node, leaf))
}

val tree: Tree[String] =
  Node(Node(Leaf("To"), Leaf("iterate")),
    Node(Node(Leaf("is"), Leaf("human,")),
      Node(Leaf("to"), Node(Leaf("recurse"), Leaf("divine")))))

tree.fold[String]((a, b) => a + " " + b, str => str)

