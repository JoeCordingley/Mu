package com.joecordingley.mu

import cats.data._
import cats.implicits._
import cats._

object Random {
  def nextLong(n: Long) = (n * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
}
object RandomState {
  type RandomS[A] = State[Long, A]
  val nextLong: State[Long, Long] = State.modify(Random.nextLong).get
  val nextInt: State[Long, Int] = nextLong.map(n => (n >>> 16).toInt)
  def nextIntModulo(operand: Int) = nextInt.map(_ % operand)
  def shuffle[A](l: Vector[A]): RandomS[Vector[A]] = 
    if (l.size <= 1) State.pure[Long, Vector[A]](l)
    else 
      for {
        tuple <- nextIntModulo(l.size).map(l.splitAt)
        (l1, n +: l2) = tuple
        rest <- shuffle(l1 ++ l2)
      } yield n +: rest

  def dealAllEvenly[A](l: Set[A], piles:Int): RandomS[List[Set[A]]] = shuffle(l.toVector).map{
    _.grouped(l.size / piles)
    .map(_.toSet)
    .toList
  }

}
