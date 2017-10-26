package org.gs.BST

import scala.annotation.tailrec

/**
  * Created by yusuf on 24/08/2017.
  */
trait Tree[+A] {

def value : Option[A] = this match{
  case n:Node[A] => Some(n.v)
  case l:Leaf[A] => Some(l.v)
  case Empty => None
}


  def left:Option[Tree[A]] = this match{
    case n:Node[A] => Some(n.l)
    case l:Leaf[A] => None
    case Empty => None
  }


  def right:Option[Tree[A]] = this match{
    case n:Node[A] => Some(n.r)
    case l:Leaf[A] => None
    case Empty => None
  }



  @tailrec
    final def preOrderFold[A](accum : List[Tree[A]], z :List[A]) : List[A] = accum match{
      case (n : Node[A]) :: t1 =>  preOrderFold((Eval(n.v) :: List(n.l) ::: List(n.r) ::: t1),z)
      case (l : Leaf[A]) :: t1 => preOrderFold(t1, l.v :: z)
      case (e : Eval[A]) :: t1 => preOrderFold(t1,  e.v :: z)
      case Empty :: t1 =>    preOrderFold(t1, z)
      case _ => z
    }
  /**
    * preorder traversal (root, left, right)
    * Tail Recursive Optimized
    *
    *        F
    *      /   \
    *    B       G
    *   / \       \
    *  A   D       I
    *     / \     /
    *    C   E   H
    *
    * head evaluate accumulator
    * ---- -------- -----------
    *              | (F)
    * F   | ()     | F::B::G::()
    * F   | (F)    | (B,G)
    * B   | ()     | B::A::D::(G)
    * B   | (B)    | (A,D,G)
    * A   | (A)    | (D,G)
    * D   | ()     | D::C::E::(G)
    * D   | (D)    | (C,E,G)
    * C   | (C)    | (E,G)
    * E   | (E)    | (G)
    * G   | ()     | G::I::()
    * G   | (G)    | (I)
    * I   | ()     | I::H::()
    * I   | (I)    | (H)
    * H   | (H)    | ()
    *
    * result
    * F, B, A, D, C, E, G, I, H
    */


@tailrec
  final def inOrderFold[A](accum : List[Tree[A]], z: List[A]) : List[A] = accum match{
    case (n : Node[A]) :: t1 => inOrderFold((List(n.l) ::: List(Eval(n.v)) :::List(n.r) ::: t1),z)
    case (l : Leaf[A]) :: t1 => inOrderFold(t1, l.v :: z)
    case (e : Eval[A]) :: t1 => inOrderFold(t1,  e.v :: z)
    case Empty :: t1 =>    inOrderFold(t1, z)
    case _ => z
  }


  /**
    * fold with inorder traversal (left, root, right)
    * tail recursive optimized
    *
    *        F
    *      /   \
    *    B       G
    *   / \       \
    *  A   D       I
    *     / \     /
    *    C   E   H
    *
    * head evaluate accumulator
    * ---- -------- -----------
    *              | (F)
    * F   | ()     | B::F::G::()
    * B   | ()     | A::B::D::(F,G)
    * A   | (A)    | (B,D,F,G)
    * B   | (B)    | (D,F,G)
    * D   | ()     | C::D::E::(F,G)
    * C   | (C)    | (D,E,F,G)
    * D   | (D)    | (E,F,G)
    * E   | (E)    | (F,G)
    * F   | (F)    | (G)
    * G   | ()     | G::I::()
    * G   | (G)    | (I)
    * I   | ()     | H::I::()
    * H   | (H)    | H
    * I   | (I)    | ()
    *
    * result
    * A,B,C,D,E,F,G,H,I
    */



  @tailrec
  final def postOrderFold[A](accum : List[Tree[A]], z :List[A]) : List[A] = accum match{
    case (n : Node[A]) :: t1 =>  postOrderFold((List(n.l) ::: List(n.r) ::: List(Eval(n.v)) ::: t1),z)
    case (l : Leaf[A]) :: t1 => postOrderFold(t1, l.v :: z)
    case (e : Eval[A]) :: t1 => postOrderFold(t1,  e.v :: z)
    case Empty :: t1 =>    postOrderFold(t1, z)
    case _ => z
  }

  /**
    * fold with postorder traversal (left, right, root)
    * tail recursive optimized
    *
    *        F
    *      /   \
    *    B       G
    *   / \       \
    *  A   D       I
    *     / \     /
    *    C   E   H
    *
    * head evaluate accumulator
    * ---- -------- -----------
    *              | (F)
    * F   | ()     | B::G::F::()
    * B   | ()     | A::D::(B,G,F)
    * A   | (A)    | (D,B,G,F)
    * D   | ()     | C::E::D::(B,G,F)
    * C   | (C)    | (E,D,B,G,F)
    * E   | (E)    | (D,B,G,F)
    * D   | (D)    | (B,G,F)
    * B   | (B)    | (G,F)
    * G   | ()     | I::G::(F)
    * I   | ()     | H::I::(G,F)
    * H   | (H)    | (I,G,F)
    * I   | (I)    | (G,F)
    * G   | (G)    | (F)
    * F   | (F)    | ()
    *
    * result
    * A,C,E,D,B,H,I,G,F
    */



  @tailrec
  final def levelOrderFold[A](accum : List[Tree[A]], z :List[A]) : List[A] = accum match{
    case (n : Node[A]) :: t1 =>  levelOrderFold( (Eval(n.v) :: t1 )::: List(n.l,n.r),z)
    case (l : Leaf[A]) :: t1 => levelOrderFold(t1, l.v :: z)
    case (e : Eval[A]) :: t1 => levelOrderFold(t1, e.v :: z )
    case Empty :: t1 =>    levelOrderFold(t1, z)
    case _ => z
  }

  /**
    * fold with levelorder traversal
    * tail recursive optimized
    *
    *        F
    *      /   \
    *    B       G
    *   / \       \
    *  A   D       I
    *     / \     /
    *    C   E   H
    *
    * head evaluate accumulator
    * ---- -------- -----------
    *              | (F)
    * F   | ()     | (F::()) ::: (B,G)
    * F   | (F)    | (B,G)
    * B   | ()     | (B::(G)) ::: (A,D)
    * B   | (B)    | (G,A,D)
    * G   | ()     | (G::(A,D)) ::: (I)
    * G   | (G)    | (A,D,I)
    * A   | (A)    | (D,I)
    * D   | ()     | (D::(I)) ::: (C,E)
    * D   | (D)    | (I,C,E)
    * I   | ()     | (I::(C,E)) ::: (H)
    * I   | (I)    | (C,E,H)
    * C   | (C)    | (E,H)
    * E   | (E)    | (H)
    * H   | (H)    | ()
    *
    * result
    * F, B, G, A, D, I, C, E, H
    */

def size : Int = {
  def loop(accum: Tree[A]): Int = accum match {
    case (n: Node[A]) => 1 + loop(n.l) + loop(n.r)
    case (l: Leaf[A]) => 1
    case Empty => 0
    case _ => 0
  }
  loop(this)
}

  /**
    * Checks is the tree is balanced
    * Returns Boolean
    */

  def isBalanced : Boolean = {


    def loop(t: Tree[A]): Int = t match {
      case l: Leaf[A] => 1
      case n: Node[A] => {
        val l = loop(n.l)
        if (l == -1) -1
        else {
          val r = loop(n.r)
          if (r == -1) -1
          else if (math.abs(l - r) > 1) -1
          else 1 + math.max(l, r)

        }
      }
      case _ => 0
    }
    !(loop(this) == -1)
  }

  /**

    * (*) Determine the height of a binary tree.
    * Definition:  The height of a tree is the length of the path from the root to the deepest node in the tree. A (rooted) tree with only one node (the root) has a height of zero.
    */
  def height: Int = {
    def loop(t: Tree[A]): Int = t match {
      case l: Leaf[A] => 1
      case n: Node[A] => Seq(loop(n.left.get), loop(n.right.get)).max + 1
      case _          => 0
    }
    loop(this) - 1
  }


  def leafCount: Int = {

    @tailrec
    def loop(t: List[Tree[A]], z: Int): Int = t match {
      case (l: Leaf[A]) :: t1 => loop(t1, z + 1)
      case (n: Node[A]) :: t1 => loop(n.left.get :: n.right.get :: t1, z)
      case _ :: t1 => loop(t1, z)
      case _ => z
    }
    loop(List(this), 0)
  }

  /**
  * (*) Find Minimum Depth of a Binary Tree.
  * Definition:  The height of a tree is the length of the path from the root to the deepest node in the tree. A (rooted) tree with only one node (the root) has a height of zero.
  */
  //The idea is to traverse the given Binary Tree. For every node, check if it is a leaf node. If yes, then return 1.
  // If not leaf node then if left subtree is NULL, then recur for right subtree.
  // And if right subtree is NULL, then recur for left subtree.
  // If both left and right subtrees are not NULL, then take the minimum of two heights.
  def minDepth: Int = {
    def loop(t: Tree[A]): Int = t match {
      case l: Leaf[A] => 1
      case Node(_,l,r) if r == Empty => loop(l) + 1
      case Node(_,l,r) if l == Empty => loop(r) + 1
      case n: Node[A] => Seq(loop(n.left.get), loop(n.right.get)).min + 1
      case _          => 0
    }
    loop(this)
  }




//  Check whether a binary tree is a full binary tree or not
//  A full binary tree is defined as a binary tree in which all nodes have either zero or two child nodes.
//    Conversely, there is no node in a full binary tree, which has one child node.

  def IsFullTree : Boolean = {
    def loop(t: Tree[A]): Boolean = t match {
      case l: Leaf[A] => true
      case Node(_,l,r) if (r == Empty) || (l == Empty) => false
      case n: Node[A] => List(loop(n.left.get), loop(n.right.get)).forall(b => b == true)
      case _          => false
    }
    loop(this)
  }


  //Lowest Common Ancestor in a Binary Search Tree.
  //http://www.geeksforgeeks.org/lowest-common-ancestor-in-a-binary-search-tree/
  //We can solve this problem using BST properties. We can recursively traverse the BST from root.
  // The main idea of the solution is, while traversing from top to bottom, the first node n we encounter with value
  // between n1 and n2, i.e., n1 < n < n2 or same as one of the n1 or n2, is LCA of n1 and n2 (assuming that n1 < n2).
  // So just recursively traverse the BST in, if node’s value is greater than both n1 and n2 then our LCA lies in left
  // side of the node, if it’s is smaller than both n1 and n2, then LCA lies on right side.
  // Otherwise root is LCA (assuming that both n1 and n2 are present in BST)

//  def LCA[B >: A <% Ordered[B]](n1:B,n2:B):B = {
//
//    def loop[B](accum : Tree[A]): B = accum match{
//      case n: Node[A] => if((n.value.get > n1) &&  (n.value.get > n2))
//
//  }
//
//
//
//    loop(this)
//
//  }


  def add[B >: A <% Ordered[B]](x : B): Tree[B] = {   //   <% Ordered[B] is to say that only work on elements where each element, of type B, that implemented Ordered[A]

    if (this == Empty) Leaf(x)
    else if (x < value.get) {
      if(right == None && left == None) Tree.make(value.get, Leaf(x), Empty)
      else if(left == None) Tree.make(value.get, Leaf(x), right.get)
      else Tree.make(value.get, left.get.add(x), right.get)
    }
    else if(x > value.get) {
      if(right == None && left == None) Tree.make(value.get,Empty,Leaf(x))
      else if(right == None) Tree.make(value.get, left.get, Leaf(x))
      else  Tree.make(value.get, left.get, right.get.add(x))

    }
    else this
  }


  //Check if a binary tree is subtree of another binary tree
  //Tree S is a subtree of T if both inorder and preorder traversals of S are substrings of inorder and preorder traversals of T respectively.
  //Note
  //Following combination can uniquely identify a tree.

//    Inorder and Preorder.
 //   Inorder and Postorder.
 //   Inorder and Level-order.

  def CheckIfSubtree[A](that : Tree[A]) : Boolean = {

    val s_preorder = this.preOrderFold(List(this),List()).reverse
    val s_inorder = this.inOrderFold(List(this),List())
    val t_preorder = that.preOrderFold(List(that),List()).reverse
    val t_inorder = that.inOrderFold(List(that),List())

    if (t_preorder.containsSlice(s_preorder) && t_inorder.containsSlice(s_inorder))
      true
    else
      false


  }


}

object Tree{
  def make[A <% Ordered[A]](x:A,l:Tree[A] = Empty, r: Tree[A] = Empty) : Tree[A] ={
  Node(x,l,r)
  }



}

case class Node[A](v:A,l:Tree[A],r:Tree[A]) extends Tree[A]

case class Leaf[A](v:A) extends Tree[A]

case object Empty extends Tree[Nothing]

case class Eval[A](v:A) extends Tree[A]

object Run extends App {
  val t: Tree[Symbol] = Node('F, Node('B, Leaf('A), Node('D, Leaf('C), Leaf('E))), Node('G, Empty, Node('I, Leaf('H), Empty)))
  println("tree: " + t)

  //print the value of b node navigating from root
  for {
    b <- t.left
    value <- b.value
  } println("B node: " + value)

  //print the value of e node navigating from root
  for {
    b <- t.left
    d <- b.right
    value <- d.value
  } println("D node: " + value)

  //no println() is executed for empty node chain
  for {
    b <- t.left
    d <- b.right
    e <- d.right
    x <- e.right
    value <- x.value
  } println("X node SHOUL NOT PRINT!: " + value)

  val preOrderfolderTree = t.preOrderFold(List(t),List()).reverse
  println(preOrderfolderTree.mkString(""))

  val inOrderfolderTree = t.inOrderFold(List(t),List())
  println(inOrderfolderTree.mkString(""))

  val postOrderfolderTree = t.postOrderFold(List(t),List())
  println(postOrderfolderTree.mkString(""))

  val levelOrderfolderTree = t.levelOrderFold(List(t),List())
  println(levelOrderfolderTree.mkString(""))



  val sizeOfTree1 = t.size
  println(sizeOfTree1)

  val heightOfTree = t.height
  println(heightOfTree)


  val minDepth = t.minDepth
  println(minDepth)

  val t1: Tree[Symbol] = Node('A, Node('B, Node('C, Node('D, Node('E, Leaf('F), Empty), Empty), Empty),Empty ), Node('G,Leaf('H),Empty ))
  val t2: Tree[Symbol] = Node('A, Leaf('B), Node('G,Leaf('H),Empty ))
  val t3: Tree[Symbol] = Node('F, Node('B, Leaf('A), Node('D, Leaf('C), Leaf('E))), Node('G, Leaf('J), Node('I, Leaf('H), Leaf('I))))

  println("tree: " + t1)

  val heightOfTree1 = t1.height
  println(heightOfTree1)

  val minDepth1 = t1.minDepth
  println(minDepth1-1)

  val minDepth2 = t2.minDepth
  println(minDepth2-1)



  val checkIfFullTree = t.IsFullTree
  println(checkIfFullTree)

  val checkIfFullTree1 = t1.IsFullTree
  println(checkIfFullTree1)

  val checkIfFullTree2 = t2.IsFullTree
  println(checkIfFullTree2)

  val checkIfFullTree3 = t3.IsFullTree
  println(checkIfFullTree3)

  println("-----Full Tree Checks complete-------")

  val checkIfBalanced = t.isBalanced
  println(checkIfBalanced)

  val checkIfBalanced1 = t1.isBalanced
  println(checkIfBalanced1)

  val checkIfBalanced2 = t2.isBalanced
  println(checkIfBalanced2)

  val checkIfBalanced3 = t3.isBalanced
  println(checkIfBalanced3)


  println("-----Balanced Tree Checks complete-------")
  //  println("as seq: " + t.toSeq)
//
//  println("count: " + t.size)
//  assert(t.size == 9)
//
//  println("height: " + t.height)
//  assert(t.height == 3)
//
//  println("leaft count: " + t.leafCount)
//  assert(t.leafCount == 4)

  val t4: Tree[Int] = Node(10,Empty,Empty)
  val t5: Tree[Int] = t4.add(8)
  val t6: Tree[Int] = t5.add(5)
  val t7: Tree[Int] = t6.add(9)
  val t8: Tree[Int] = t7.add(3)
  val t9: Tree[Int] = t8.add(6)
  val t10: Tree[Int] = t9.add(7)
  println("tree4: " + t10)

  println("******************Check subtree*************")



  val t11: Tree[Int] = Node(1,Empty,Empty)



}




