import trees.bst.avl.AVL
import trees.bst.splay.SplayTree

import scala.util.Random

object Tests {

   def massert(bool: => Boolean): Unit = {
      if (!bool) {
         println("Assertion failed")
         throw new RuntimeException("Assertion failed")
      }
   }

   def splay_test(): Unit = {
      val tree: SplayTree[Int] = new SplayTree[Int]()
      tree.insert(5)
      tree.insert(1)
      tree.insert(3)
      //println(tree)
      massert(tree.find(1))
      massert(tree.find(3))
      massert(tree.find(5))
      massert(!tree.find(0))
      tree.remove(5)
      massert(tree.find(1))
      massert(tree.find(3))
      massert(!tree.find(5))
   }

   def build_avl[T <% Ordered[T]](seq: Seq[T]): AVL[T] = seq.foldLeft(AVL.empty[T]) { (tree: AVL[T], i: T) => tree.insert(i) }

   def avl_test(): Unit = {
      val empty: AVL[Int] = AVL.empty
      println(empty)
      val t135 = empty.insert(3).insert(5).insert(1)
      println(t135)
      assert(t135.find(1))
      assert(t135.find(3))
      assert(t135.find(5))
      assert(!t135.find(0))
      val t13 = t135.remove(5)
      println(t13)
      assert(!t13.find(5))
      assert(t13 == t13.remove(0))
      assert(t13.find(1))
      assert(t13.find(3))
      val ints: Seq[Int] = 1 to 100
      val shuffled_ints = Random.shuffle(ints)
      val random = build_avl(shuffled_ints)
      println("Random height = " + random.height)
      //println(random)
      val sorted: AVL[Int] = build_avl(ints)
      println("Sorted height = " + sorted.height)
      println(sorted)
   }

   def main(args: Array[String]): Unit = {
      //splay_test()
      avl_test()
   }
}
