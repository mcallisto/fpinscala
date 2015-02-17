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
  def fold[A, B](t: Tree[A], z: B)(f: (A, B) ⇒ B): B = t match {
    case l: Leaf[A] ⇒ f(l.value, z)
    case b: Branch[A] ⇒ fold(b.left, fold(b.right, z)(f))(f)
  }
  
  def sizeF[A](t: Tree[A]): Int = fold(t, 0)((x, y) ⇒ y + 1)

  def maximumF(t: Tree[Int]): Int = fold(t, 0)(_ max _)

  def depthF[A](t: Tree[A]): Int = fold(t, 0)((x, y) ⇒ 1 max (y + 1))
    
//  def map[A, B](l: List[A])(f: A ⇒ B): List[B] = foldRight(l, Nil: List[B])((x, y) ⇒ Cons(f(x), y))
//  def map2[A, B](t: Tree[A])(f: A ⇒ B): Tree[B] = fold(t, Nel: Tree[B])((x, y) ⇒ Branch(Leaf(f(x)), y))

}