package trees.bst.avl

sealed abstract class AVL[T <% Ordered[T]] {
   def insert(x: T): AVL[T]
   def find(x: T): Boolean
   def remove(x: T): AVL[T]
   def max(): Option[T]
   def min(): Option[T]
   val height: Int
   def balance: Int
   def invariants: Boolean
}

case class Leaf[T <% Ordered[T]]() extends AVL[T] {
   override def insert(x: T): AVL[T] = Node(Leaf(), x, Leaf())

   override def find(x: T): Boolean = false

   override def remove(x: T): AVL[T] = this // nothing to remove

   override def max(): Option[T] = None

   override def min(): Option[T] = None

   override val height: Int = 0

   override def balance: Int = 0

   override def invariants: Boolean = true

   override def toString: String = "∅"
}

//TODO add balance
case class Node[T <% Ordered[T]](left: AVL[T], value: T, right: AVL[T]) extends AVL[T] {
   override def insert(x: T): AVL[T] = {
      val modified = if (x == value) this
                     else if (x < value) Node(left.insert(x), value, right)
                     else Node(left, value, right.insert(x))

      val balanced = rebalanced(modified)

      assert(balanced.invariants)
      balanced
   }

   override def find(x: T): Boolean =
      if (x == value) true
      else if (x < value) left.find(x)
      else right.find(x)

   override def remove(x: T): AVL[T] = {
      val modified = if (x == value) {
         this match {
            case Node(Leaf(), `x`, b) => b
            case Node(a, `x`, Leaf()) => a
            case Node(a, `x`, b) => {
               val m = a.max().get // max exists because otherwise first case should have matched
               Node(a.remove(m), m, b)
            }
         }
      }
      else if (x < value) Node(left.remove(x), value, right)
      else Node(left, value, right.remove(x))

      val balanced = rebalanced(modified)

      assert(balanced.invariants)
      balanced
   }

   override def max(): Option[T] =
      right.max() match {
         case None => Some(value)
         case Some(x) => Some(x)
      }

   override def min(): Option[T] =
      left.min() match {
         case None => Some(value)
         case Some(x) => Some(x)
      }

   override val height: Int = math.max(left.height, right.height) + 1

   override def balance: Int = left.height - right.height

   private def rebalanced(t: AVL[T]): AVL[T] =
      t match {
         case n: Node[T] => n.rebalance()
         case l: Leaf[T] => l
      }

   private def rebalance(): AVL[T] = {
      assert(math.abs(balance) <= 2)
      if (balance == -2) {
         val Node(a, w, b) = right // can do that as right is high enough
         if (b.height > a.height)
            Node(Node(left, value, a), w, b)
         else { // a is higher, need double rot
            val Node(a1, u, a2) = a
            Node(Node(left, value, a1), u, Node(a2, w, b))
         }
      } else if (balance == 2) {
         val Node(a, w, b) = left
         if (a.height > b.height)
            Node(a, w, Node(b, value, right))
         else { // a is higher, need double rot
            val Node(b1, u, b2) = b
            Node(Node(a, w, b1), u, Node(b2, value, right))
         }
      } else {
         this // should be already balanced
      }
   }

   override def invariants: Boolean =
      math.abs(balance) <= 1 && right.min().forall(_ > value) && left.max().forall(_ < value)

   override def toString: String = this match {
      case Node(Leaf(), v, Leaf()) => "Node(" + v + ")"
      case Node(l, v, r) => "Node(" + l + "," + v + "," + r + ")"
   }

}

object AVL {
   def empty[T <% Ordered[T]]: AVL[T] = {
      Leaf()
   }
}