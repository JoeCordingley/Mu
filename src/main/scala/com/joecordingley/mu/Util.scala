package com.joecordingley.mu
import cats.data.State
import scala.util.Random

object Util {
  implicit class ListOps[A](l: List[A]) {
    def outrightHighest(implicit o: Ordering[A]): Option[A] =
      l.sorted.reverse match {
        case List(a1)                                => Some(a1)
        case a1 :: a2 :: _ if o.compare(a1, a2) != 0 => Some(a1)
        case _                                       => None
      }
  }

  case class RNG(seed:Long){
    private val next = new Random(seed).nextLong
    def nextLong:(RNG,Long) = (RNG(next),next)
  }
  type RNGState[A] = State[RNG,A]
  def nextLong: RNGState[Long] = State(_.nextLong)
  def nextCardFrom(cards: Set[Card]):RNGState[Card] = {
    val cardsSize = cards.size
    nextLong.map(r => cards.toVector.apply((r%cardsSize).toInt))
  }
}
object SingleElementSet {
  def unapply[A](s: Set[A]): Option[A] = if (s.size == 1) Some(s.head) else None
}

