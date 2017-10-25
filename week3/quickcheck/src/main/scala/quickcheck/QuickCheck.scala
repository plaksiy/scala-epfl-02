package quickcheck

import java.util.NoSuchElementException

import common._
import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] =
    frequency(
      (1, empty),
      (3, for {
        e <- arbitrary[Int]
        h <- genHeap
      } yield insert(e, h))
    )

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("gen2") = forAll { (a: Int, b: Int) =>
    val h = insert(a, empty)
    findMin(insert(b, h)) == (a min b)
  }

  property("ins-del") = forAll { a: Int =>
    val h = insert(a, empty)
    deleteMin(h) == empty
  }

  property("del-elem-from-empty") = forAll { a: Int =>
    val h = insert(a, empty)
    lazy val m = deleteMin(deleteMin(h))
    Prop.throws(classOf[NoSuchElementException]) { m }
  }

  property("sort-seq") = forAll { h: H =>
    val l = heap2List(h)
    l == l.sorted
  }

  property("sort-seq1") = forAll { l: List[A] =>
    val h = l.foldLeft(empty)((h: H, a: A) => insert(a, h))
    val lh = heap2List(h)
    lh == l.sorted
  }

  def heap2List(h: H): List[A] = {
    @scala.annotation.tailrec
    def loop(l: List[A], h: H): List[A] = {
      if (isEmpty(h)) l
      else loop(l :+ findMin(h), deleteMin(h))
    }
    loop(Nil, h)
  }

  property("min-of-meld1") = forAll { (h1: H, h2: H) =>
    lazy val e = findMin(meld(h1, h2)) == (findMin(h1) min findMin(h2))
    if (isEmpty(h1) || isEmpty(h2)) Prop.throws(classOf[NoSuchElementException]) { e }
    else e
  }
}