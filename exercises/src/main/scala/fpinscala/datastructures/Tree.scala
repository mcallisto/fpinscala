package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  // Exercise 25: Write a function size that counts the number of nodes in a tree
  def size[A](t: Tree[A]): Int = t match {
    case _: Leaf[A] ⇒ 1
    case b: Branch[A] ⇒ size(b.left) + size(b.right)
  }

  // Exercise 26: Write a function maximum that returns the maximum element in a Tree[Int]
  def maximum(t: Tree[Int]): Int = {

    def go(t: Tree[Int], acc: Int): Int = t match {
      case l: Leaf[Int] ⇒ l.value max acc
      case b: Branch[Int] ⇒ go(b.left, acc) max go(b.right, acc)
    }

    go(t, 0)
  }

  // Exercise 27: write a function depth that returns the maximum path length
  def depth[A](t: Tree[A]): Int = {

    def go[A](t: Tree[A], acc: Int): Int = t match {
      case l: Leaf[A] ⇒ acc
      case b: Branch[A] ⇒ go(b.left, acc + 1) max go(b.right, acc + 1)
    }

    go(t, 1)
  }

  // Exercise 28: write a function map, analogous to the method of the same name on List
  def map[A, B](t: Tree[A])(f: A ⇒ B): Tree[B] = t match {
    case l: Leaf[A] ⇒ Leaf(f(l.value))
    case b: Branch[A] ⇒ Branch(map(b.left)(f), map(b.right)(f))
  }
  
  // Exercise 29: Generalize size, maximum, depth, and map, writing a new function fold
  def fold[A, B](t: Tree[A])(l: A ⇒ B)(b: (B,B) ⇒ B): B = t match {
    case Leaf(v) ⇒ l(v)
    case Branch(left, right) ⇒ b(fold(left)(l)(b), fold(right)(l)(b))
  }
  
  def sizeF[A](t: Tree[A]): Int = fold(t)(x ⇒ 1)((x, y) ⇒ x + y)

  def maximumF(t: Tree[Int]): Int = fold(t)(identity)(_ max _)

  def depthF[A](t: Tree[A]): Int = fold(t)(x ⇒ 1)((x, y) ⇒ (x max y) + 1)
    
  def mapF[A, B](t: Tree[A])(f: A ⇒ B): Tree[B] = fold(t)(x ⇒ Leaf(f(x)): Tree[B])((x, y) ⇒ Branch(x, y))

}