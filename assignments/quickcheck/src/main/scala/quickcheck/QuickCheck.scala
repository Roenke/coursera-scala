package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    value <- arbitrary[A]
    heap <- oneOf(const(empty), genHeap)
  } yield insert(value, heap)
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { (a: Int) =>
    val heap = insert(a, empty)
    findMin(heap) == a
  }

  property("min2") = forAll { (a: Int, b: Int) =>
    val heap = insert(a, insert(b, empty))
    math.min(a, b) == findMin(heap)
  }

  property("delete1") = forAll { (a: Int) =>
    val heap = insert(a, empty)
    isEmpty(deleteMin(heap))
  }

  property("delete2") = forAll { (a: Int, b: Int) =>
    val heap = insert(a, insert(b, empty))
    math.min(a, b) == findMin(heap) &&
      math.max(a, b) == findMin(deleteMin(heap))
  }

  property("sorted1") = forAll { (a: Int, b: Int, c: Int) =>
    val heap = insert(c, insert(b, insert(a, empty)))
    val val1 = findMin(heap)
    val val2 = findMin(deleteMin(heap))
    val val3 = findMin(deleteMin(deleteMin(heap)))
    val1 <= val2 && val2 <= val3
  }

  property("sorted2") = forAll(genHeap) { (heap: H) =>
    def isSorted(h: H): Boolean =
      isEmpty(h) || isEmpty(deleteMin(h)) || (findMin(h) <= findMin(deleteMin(h)) && isSorted(deleteMin(h)))
    isSorted(heap)
  }

  property("sorted3") = forAll { (heap1: H, heap2: H) =>
    def sort(h: H): List[A] =
      if (isEmpty(h)) Nil
      else findMin(h) :: sort(deleteMin(h))
    val one = meld(heap1, heap2)
    val two = meld(deleteMin(heap1), insert(findMin(heap1), heap2))
    sort(one) == sort(two)
  }

  property("meld1") = forAll { (heap1: H, heap2: H) =>
    math.min(findMin(heap1), findMin(heap2)) == findMin(meld(heap1, heap2))
  }

}
