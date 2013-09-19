package scala.tools.refactoring.util

import scala.collection.breakOut

class UnionFind(n: Int) {
  val parents: Array[Int] = Array.tabulate(n){(x) => x}
  val ranks: Array[Int] = Array.fill(n)(1)

  /**
   * Return the parent (representant) of the equivalence class
   * of the element indexed at i
   */
  def find(i: Int): Int = {
    val pi = parents(i)
    if (pi == i) i else {
      val ci = find(pi)
      parents(i) = ci
      ci
    }
  }
  
  /** 
   *  Unify equivalence classes of elements indexed at x and y
   */
  def union(x: Int, y: Int): Unit = {
    val cx = find(x)
    val cy = find(y)
    if (cx != cy) {
      val rx = ranks(x)
      val ry = ranks(y)
      if (rx > ry) parents(cy) = cx
      else if (rx < ry) parents(cx) = cy
      	else {
      	  ranks(cx) += 1
      	  parents(cy) = cx
      	}
    }
  }
  
  /**
   * Enumerates the equivalence class of element at index x
   */
  def equivalenceClass[T](A: Array[T], x: Int): List[T] = {
    val px = parents(x)
    (for (i <- (0 to n-1) if (parents(i) == px)) yield A(i))(breakOut)
  }
   
}