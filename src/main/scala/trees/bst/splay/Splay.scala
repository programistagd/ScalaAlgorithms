package trees.bst.splay

abstract sealed class Splay[T <% Ordered[T]] {
   def splay(x: T): Splay[T] // splays the tree
   def find(x: T, missing: T): T
}

case class Leaf[T <% Ordered[T]]() extends Splay[T] {
   override def splay(x: T): Splay[T] = this
   override def find(x: T, missing: T): T = missing
}

case class Node[T <% Ordered[T]](l: Splay[T], value: T, r: Splay[T]) extends Splay[T] {
   override def find(x: T, missing: T): T =
      if (x == value) x
      else if (x < value) l.find(x, value)
      else /* x > v */ r.find(x, value)

   override def splay(x: T): Splay[T] = {
      val v = find(x, value)
      println(v + " <- " + x)
      if (v == value) return this
      val rec = if (v < value) Node(l.splay(x), value, r) else Node(l, value, r.splay(x))
      val splayed = rec match {
         case Node(_, `v`, _) => rec
         // v is our son
         case Node(Node(a, `v`, b), w, c) => Node(a, v, Node(b, w, c))
         case Node(a, w, Node(b, `v`, c)) => Node(Node(a, w, b), v, c)

         //v is our grandson -> left left or right right
         case Node(Node(Node(a, `v`, b), w, c), u, d) => Node(a, v, Node(b, w, Node(c, u, d)))
         case Node(a, u, Node(b, w, Node(c, `v`, d))) => Node(Node(Node(a, u, b), w, c), v, d)

         //v is our grandson -> left right / right left
         case Node(a, u, Node(Node(b, `v`, c), w, d)) => Node(Node(a, u, b), v, Node(c, w, d))
         case Node(Node(a, w, Node(b, `v`, c)), u, d) => Node(Node(a, w, b), v, Node(c, u, d))
         case _ => throw new RuntimeException("That should not happen")
      }
      assert(splayed.value == v)
      println(x+" " +v + " " + splayed.value )
      println(splayed)
      splayed
   }

}

class SplayTree[T <% Ordered[T]](private var root: Splay[T]) {
   def this() = this(Leaf())
   def find(x: T): Boolean = {
      println("Looking for "+ x)
      splay(x)
      root match {
         case Node(_, `x`, _) => true
         case _ => false
      }
   }

   def insert(x: T): Unit = {
      println("Inserting "+ x)
      splay(x)
      root = root match {
         case Node(_, `x`, _) => root // already inserted
         case Leaf() => Node(Leaf(), x, Leaf())
         case Node(l, y, r) => Node(Node(l, y, Leaf()), x, r)
      }
   }

   def remove(x: T): Unit = {
      splay(x)
   }

   private def splay(x: T): Unit = {
      root = root.splay(x)
   }

   override def toString: String = root.toString
}