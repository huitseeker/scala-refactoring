package scala.tools.refactoring.util

import scala.collection.mutable.Map
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap

class UnionFind[T]() {

  val parents: Map[T, T] = new HashMap[T,T] {
    override def default(s: T) = {
        get(s) match {
          case Some(v) => v
          case None    => put(s, s); s
        }
    }
  }

  private val ranks: Map[T, Int] = new HashMap[T,Int] {
    override def default(s: T) = {
        get(s) match {
          case Some(v) => v
          case None    => put(s, 1); 1
        }
    }
  }

  /**
   * Return the parent (representant) of the equivalence class.
   * Uses path compression.
   */
  def find(s: T): T = {
    val ps = parents(s)
    if (ps == s) s else {
      val cs = find(ps)
      parents(s) = cs
      cs
    }
  }

  /**
   *  Unify equivalence classes of elements.
   *  Uses union by rank.
   */
  def union(x: T, y: T): Unit = {
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
   * Enumerates the equivalence class of element x
   */
  def equivalenceClass(x: T): List[T] = {
    val px = parents(x)
    parents.keys filter (parents(_:T) == px) toList
  }

}