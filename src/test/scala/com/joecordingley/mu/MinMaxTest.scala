package com.joecordingley.mu

import org.scalatest.FreeSpec
import org.scalatest.Matchers
import cats.implicits._

class MinMaxTest extends FreeSpec with Matchers {

  sealed trait Tree {
    val value: Int
  }
  case class Branch(value: Int, trees: Stream[Tree]) extends Tree
  case class Leaf(value: Int) extends Tree

  "alphaBeta should work" in {
//    val tree = Branch(
//      value = 5,
//      trees = Stream(
//        Branch(
//          value = 5,
//          trees = Stream(
//            Branch(value = 5, trees = Stream(Leaf(3), Leaf(5))),
//            Branch(value = 6, trees = Leaf(6) #:: Stream(fail)))),
//        Branch(
//          value = 2,
//          trees = Stream(
//            Branch(value = 2, trees = Stream(Leaf(1), Leaf(2))),
//            Branch(value = 6, trees = Stream(Leaf(0), Leaf(1)))))
//      )
//    )
//    implicit val evaluator = new Evaluate2[Tree, Int] {
//      override def value(tree: Tree) = tree.value
//      override def children(tree: Tree) = tree match {
//        case _: Leaf => Stream.empty
//        case Branch(_, trees) => trees.toStream
//      }
//    }
//    MinMax2.alphaBeta3[Tree, Int](tree, true, 10) shouldEqual 5
  }
}
