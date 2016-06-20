package parallel

/**
  * Created by vitaly on 09.06.2016.
  */
object Parallel extends App {

  class a extends Thread {
    override def run() = {
      println("Hello")
    }
  }

  sealed abstract class  Tree[A]
  case class Leaf[A](a:A) extends Tree[A]
  case class Node[A](l:Tree[A], r: Tree[A]) extends Tree[A]

  sealed abstract class TreeRes[A] {
    val res: A
  }
  case class LeafRes[A](override val res: A) extends TreeRes[A]
  case class NodeRes[A](l: TreeRes[A], override val res: A, r: TreeRes[A]) extends TreeRes[A]

  def reduceRes[A](t:Tree[A], f: (A, A) => A): TreeRes[A] = {
    t match {
      case Leaf(a) => LeafRes(a)
      case Node(l, r) => {
        val left = reduceRes(l, f)
        val right = reduceRes(r, f)
        NodeRes(left, f(left.res, right.res), right)
      }
    }
  }

  def scanLeftSeq[A](inp: Array[A], a0: A, f: (A, A) => A, out: Array[A]): Unit = {
    out.update(0, a0)
    var i = 1
    while (i <= inp.length) {
      out(i) = f(out(i - 1), inp(i - 1))
      i += 1
    }
  }

  def reduceSeg1[A](inp: Array[A], left: Int, right: Int, a0: A, f: (A, A) => A): A = {

  }

  def mapSeg[A, B](inp: Array[A], left: Int, right: Int, fi: (Int, A) => B, out: Array[B]): Unit = {

  }

  def scanLeft[A](inp: Array[A], a0: A, f: (A, A) => A, out: Array[A]): Unit = {
    val fi = (i: Int, value: A) => reduceSeg1(inp, 0, i, a0, f)
    mapSeg(inp, 0, inp.length, fi, out)
    val last = inp.length - 1
    out(last + 1) = f(out(last), inp(last))
  }

  override def main(args: Array[String]) = {
    val arr1 = Array[Int](1, 2, 3, 4, 5)
    val arr2 = Array[Int](0, 0, 0, 0, 0, 0)
    def sum = (x: Int, y: Int) => x + y
    scanLeftSeq(arr1, 0, sum, arr2)
    arr2.foreach(x => println(x))
  }
}
