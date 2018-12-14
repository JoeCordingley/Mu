package com.joecordingley.mu

import cats.data._
import cats.implicits._
import cats._

object Random {

  def nextLong(n:Long) =( n*  0x5DEECE66DL + 0xBL ) & 0xFFFFFFFFFFFFL
}
object RandomState {
  val nextLong: State[Long,Long] = State.modify(Random.nextLong).get
  val nextInt: State[Long,Int] = nextLong.map(n  => (n >>> 16).toInt)
  def nextIntModulo(operand:Int) = nextInt.map(_%operand)
  def shuffle[A](l: List[A]): State[Long, List[A]] = l match {
    case Nil => State.pure[Long,List[A]](Nil)
    case _ => for {
      tuple <- nextIntModulo(l.size).map(l.splitAt)
      (l1, n :: l2) = tuple
      rest <- shuffle(l1 ::: l2)
    } yield n :: rest
  } 
  def take[A](index:Int):State[Vector[A],A] = State{vector => 
    val (v1,Vector(n, v2 @ _*)) = vector.splitAt(index)
    (v1 ++ v2,n)
  }
  def takeRandom[A]:State[(Long,Vector[A]),A] = {
    def updateSeed: ((Long,Vector[A]),Long) => (Long, Vector[A]) = {
      case ((_,v),seed) => (seed,v)
    }
    def updateVector: ((Long, Vector[A]),Vector[A]) => (Long, Vector[A]) = {
      case ((seed,_),v) => (seed,v)
    }
    for {
      vector <- State.get[(Long,Vector[A])].map(_._2)
      index <- nextIntModulo(vector.size).transformS[(Long,Vector[A])](_._1,updateSeed)
      n <- take(index).transformS[(Long,Vector[A])](_._2,updateVector)
    } yield n
  }
}
